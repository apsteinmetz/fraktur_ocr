library(tidyverse)

# source("R/gedcom_functions.R")
FILE_PATH <- "data/persons_sm.txt"
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
      cat(lines[i], "\n")
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

people_in_family <- separate_people(records[[1]])

make_person <- function(
  record,
  indiv_id,
  fam_ids = list(indiv_id),
  relationship = "primary"
) {
  person = list(
    record = record,
    indiv_id = indiv_id,
    fam_ids = fam_ids,
    relationship = relationship
  )
  return(person)
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




# The error is caused by using the global `people_in_family` length inside `make_family()`
# Families shorter than `length(people_in_family)` trigger out-of-bounds indexing
len_first <- length(people_in_family)
lens <- purrr::map_int(all_families, length)
c(len_first = len_first, min_len = min(lens))
which(lens < len_first)

# Fix: use the function argument `people`, not the global `people_in_family`
make_family <- function(people) {
  family <- list()
  current_relationship <- "primary"
  indiv_id <- str_match(people[[1]]$person, "<(\\d+)>")[, 2]
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
        indiv_id <- str_match(person, "(< |> )(\\d+\\.*\\d*)")[, 3]
        fam_id <- indiv_id
        family[[i]] <- make_person(
          person,
          indiv_id,
          fam_ids = list(fam_id),
          relationship = current_relationship
        )
        if (!is.na(last_child)) {
          family[[last_child]]$fam_ids <- append_to_list(
            family[[last_child]]$fam_ids,
            fam_id
          )
        }

      } else if ((role == "spouse") && (current_relationship == "primary_child_spouse")) {
        current_relationship <- "primary_child_spouse"
        indiv_id <- str_match(person, "(< |> )(\\d+\\.*\\d*)")[, 3]
        fam_id <- indiv_id
        family[[i]] <- make_person(
          person,
          indiv_id,
          fam_ids = list(fam_id),
          relationship = current_relationship
        )
        if (!is.na(last_child)) {
          family[[last_child]]$fam_ids <- append_to_list(
            family[[last_child]]$fam_ids,
            fam_id
          )
        }
      }
    }
  }

  map_dfr(
    family,
    ~tibble(
      record = .x$record,
      indiv_id = .x$indiv_id,
      fam_ids = list(.x$fam_ids),
      relationship = .x$relationship
    )
  )
}

people_in_family <- separate_people(records[[1]])
all_families <- records |>
  map(separate_people) |>
  map(make_family) |>
  bind_rows()


