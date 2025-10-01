library(tidyverse)

source("R/gedcom_functions.R")
source("R/fun_parse_person.R")
FILE_PATH <- "data/banyai.txt"
PROGRESS <- TRUE
read_records <- function(file_path = FILE_PATH) {
  if (PROGRESS) cat('reading records\n ')
  # Read the file line by line
  lines_raw <- readLines(file_path, encoding = "UTF-8") # Handle character encoding
  # get rid of surname block headers
  lines <- vector()
  i = 1
  while (i <= length(lines_raw)) {
    cat(lines_raw[i])
    if (grepl("^[A-Z]+$", lines_raw[i])) {
      i <- i + 2 #skip this and the next line
    } else {
      lines <- c(lines, lines_raw[i])
      # cat(lines[i], "\n")
      i <- i + 1
    }
  }
  # remove empty lines
  lines <- lines[!grepl("^\\s*$", lines)]
  return(lines)
}

# Example usage
records_raw <- read_records(FILE_PATH)

# apply the tag_text function to each line
# tagged_lines <- map(records, tag_text) |>
#   unlist()

# separate into list of records by splitting at <I\d+> tags
record_indices <- which(grepl("^<\\d+>", records_raw))
record_indices <- c(record_indices, length(records_raw) + 1) # add end index

records <- list()
# for loop version
for (i in seq_along(record_indices[-length(record_indices)])) {
  if (PROGRESS) cat(i, " ")
  start_idx <- record_indices[i]
  end_idx <- record_indices[i + 1] - 1
  records[[i]] <- records_raw[start_idx:end_idx]
}

separate_people <- function(family) {
  person_tag <- c("<\\d+>", "oo", "o‐o", "\\d+\\.")
  # separate into list of records by splitting at person tags
  person_indices <- which(grepl(
    paste0("^", person_tag, collapse = "|^"),
    family
  ))
  person_indices <- c(person_indices, length(family) + 1) # add end index
  people <- list()

  for (i in seq_along(person_indices[-length(person_indices)])) {
    start_idx <- person_indices[i]
    end_idx <- person_indices[i + 1] - 1
    person <- family[start_idx:end_idx] |> paste(collapse = " ")
    if (str_detect(person, "oo|o‐o")) {
      role <- "spouse"
    } else if (str_detect(person, "^\\d+\\.")) {
      role <- "child"
    } else {
      role <- "primary"
    }
    # assign role based on person_tag
    people[[i]] <- list(role = role, person = person)
  }
  return(people)
}

# people_in_family <- separate_people(records[[1]])

make_person <- function(record, indiv_id, fam_ids = list(indiv_id), relationship = "primary") {
  parsed <- parse_person_lines(record)[[1]]
  
  # Use XREF indiv_id if present, otherwise use provided indiv_id
  final_indiv_id <- if (!is.null(parsed$xref_indiv_id)) {
    parsed$xref_indiv_id
  } else {
    indiv_id
  }
  
  # Add XREF fam_id to the fam_ids list if present
  # Ensure fam_ids is always a flat character vector
  base_fam_ids <- unlist(fam_ids)  # Flatten any nested lists
  
  final_fam_ids <- if (!is.null(parsed$xref_fam_id)) {
    unique(c(base_fam_ids, parsed$xref_fam_id))
  } else {
    base_fam_ids
  }
  
  tibble(
    record = record,
    name = parsed$name,
    events = list(parsed$events),
    indiv_id = final_indiv_id,
    fam_ids = list(final_fam_ids),  # Always wrap the character vector in list()
    relationship = relationship
  )
}


possible_relationships <- c(
  "primary",
  "primary_spouse",
  "primary_child",
  "primary_child_spouse",
  "primary_child_child"
) |>
  as_factor()


increment_relationship <- function(current_relationship, delta = 1) {
  current_index <- as.integer(current_relationship)
  if (current_index < length(levels(current_relationship))) {
    current_relationship <- levels(current_relationship)[current_index + delta]
  } else {
    current_relationship # Don't increment beyond the last relationship
  }
}
append_to_list <- function(lst, element) {
  lst[[length(lst) + 1]] <- element
  lst
}

make_family <- function(people) {
  family <- list()
  current_relationship <- "primary"
  indiv_id <- str_match(people[[1]]$person, "<(\\d+)>")[, 2]
  cat(indiv_id, "\n")
  fam_id_root <- indiv_id
  fam_id <- fam_id_root
  spouse_num <- 0
  last_child <- NA_integer_

  family[[1]] <- make_person(
    people[[1]]$person,
    indiv_id,
    fam_ids = list(fam_id),
    relationship = current_relationship
  )

  if (length(people) >= 2) {
    for (i in 2:length(people)) {
      person <- people[[i]]$person
      role <- people[[i]]$role

      if ((role == "spouse") && (current_relationship == "primary")) {
        spouse_num <- spouse_num + 1
        indiv_id <- paste0(fam_id, "S", spouse_num)
        fam_id <- fam_id_root
        current_relationship <- "primary_spouse"
        family[[i]] <- make_person(
          person,
          indiv_id,
          fam_ids = list(fam_id),
          relationship = current_relationship
        )

      } else if (role == "child") {
        current_relationship <- "primary_child"
        indiv_id <- paste0(fam_id_root, ".", str_match(person, "^(\\d+)\\.")[, 2])
        fam_id <- fam_id_root
        last_child <- i
        family[[i]] <- make_person(
          person,
          indiv_id,
          fam_ids = list(fam_id),
          relationship = current_relationship
        )

      } else if ((role == "spouse") && (current_relationship == "primary_child")) {
        current_relationship <- "primary_child_spouse"
        # Let make_person handle the XREF extraction
        family[[i]] <- make_person(
          person,
          paste0(fam_id_root, "S", spouse_num + 1), # fallback ID
          fam_ids = list(fam_id_root),
          relationship = current_relationship
        )
        
        # Get the actual fam_id from the created person (could be from XREF)
        actual_fam_id <- str_extract(family[[i]]$indiv_id, "^[0-9]+")
        
        if (!is.na(last_child)) {
          family[[last_child]]$fam_ids <- list(unique(c(
            unlist(family[[last_child]]$fam_ids),  # Flatten any nested structure
            actual_fam_id
          )))
        }

      } else if ((role == "spouse") && (current_relationship == "primary_child_spouse")) {
        current_relationship <- "primary_child_spouse"
        # Let make_person handle the XREF extraction
        family[[i]] <- make_person(
          person,
          paste0(fam_id_root, "S", spouse_num + 1), # fallback ID
          fam_ids = list(fam_id_root),
          relationship = current_relationship
        )
        
        # Get the actual fam_id from the created person (could be from XREF)
        actual_fam_id <- str_extract(family[[i]]$indiv_id, "^[0-9]+")
        
        if (!is.na(last_child)) {
          family[[last_child]]$fam_ids <- list(unique(c(
            unlist(family[[last_child]]$fam_ids),  # Flatten any nested structure
            actual_fam_id
          )))
        }
      }
    }
  }

  map_dfr(
    family,
    ~tibble(
      record = paste(.x$record, collapse = " "),
      name = rearrange_name(.x$name),
      events = list(.x$events[[1]]),
      indiv_id = .x$indiv_id,
      fam_ids = .x$fam_ids,
      relationship = .x$relationship
    )
  )
}

# people_in_family <- separate_people(records[[1]])
all_families <- records |>
  map(separate_people) |>
  map(make_family)

# save all_families as a file
saveRDS(all_families, file = "data/all_families.rds")
