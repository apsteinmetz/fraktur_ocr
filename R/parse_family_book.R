library(tidyverse)

source("R/gedcom_functions.R")
source("R/fun_parse_person.R")
FILE_PATH <- "data/banyai.txt"
PROGRESS <- TRUE
read_records <- function(file_path = FILE_PATH) {
  if (PROGRESS) {
    cat('reading records\n ')
  }
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
  if (PROGRESS) {
    cat(i, " ")
  }
  start_idx <- record_indices[i]
  end_idx <- record_indices[i + 1] - 1
  records[[i]] <- records_raw[start_idx:end_idx]
}

separate_people <- function(family) {
  person_tag <- c("<\\d+>", "\\d+\\.")
  marriage_tag <- c("oo", "o‐o")

  # Create combined pattern for splitting - person tags OR marriage tags at start of line
  split_pattern <- paste0("^", c(person_tag, marriage_tag), collapse = "|^")

  # Find indices where we should split
  person_indices <- which(grepl(split_pattern, family))
  person_indices <- c(person_indices, length(family) + 1) # add end index
  people <- list()

  for (i in seq_along(person_indices[-length(person_indices)])) {
    start_idx <- person_indices[i]
    end_idx <- person_indices[i + 1] - 1
    person <- family[start_idx:end_idx] |> paste(collapse = " ")

    # Assign role based on content (check anywhere in string)
    if (str_detect(person, "oo|o‐o")) {
      role <- "spouse"
    } else if (str_detect(person, "^\\d+\\.")) {
      role <- "child"
    } else {
      role <- "primary"
    }

    people[[i]] <- list(role = role, person = person)
  }
  return(people)
}
# people_in_family <- separate_people(records[[1]])

make_person <- function(
  record,
  indiv_id,
  fam_ids = list(indiv_id),
  relationship = "primary"
) {
  parsed <- parse_person_lines(record)[[1]]

  # Use XREF indiv_id if present, otherwise use provided indiv_id
  final_indiv_id <- if (!is.null(parsed$xref_indiv_id)) {
    parsed$xref_indiv_id
  } else {
    indiv_id
  }

  # Add XREF fam_id to the fam_ids list if present
  # Ensure fam_ids is always a flat character vector
  base_fam_ids <- unlist(fam_ids) # Flatten any nested lists

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
    fam_ids = list(final_fam_ids), # Always wrap the character vector in list()
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

  # Check if primary person has an XREF (making them also a child)
  parsed_primary <- parse_person_lines(people[[1]]$person)[[1]]

  # Primary person logic:
  # - indiv_id should be the <nn> number (family head ID)
  # - spouse_fam_ids should only be their own family ID
  # - child_fam_ids should be the XREF family ID if present
  primary_indiv_id <- indiv_id # Use <nn> as indiv_id, not the XREF
  primary_spouse_fam_ids <- fam_id_root # Primary is spouse in their own family only
  primary_child_fam_ids <- if (!is.null(parsed_primary$xref_fam_id)) {
    # Primary is also a child in the XREF family
    parsed_primary$xref_fam_id
  } else {
    character(0) # Empty if no parent family
  }

  family[[1]] <- make_person_with_separate_fam_ids(
    people[[1]]$person,
    primary_indiv_id, # Use the family head ID, not XREF ID
    spouse_fam_ids = primary_spouse_fam_ids,
    child_fam_ids = primary_child_fam_ids,
    relationship = current_relationship
  )

  if (length(people) >= 2) {
    for (i in 2:length(people)) {
      person <- people[[i]]$person
      role <- people[[i]]$role

      if ((role == "spouse") && (current_relationship == "primary")) {
        spouse_num <- spouse_num + 1

        # Parse the spouse to check for XREF
        parsed_spouse <- parse_person_lines(person)[[1]]

        if (!is.null(parsed_spouse$xref_indiv_id)) {
          # Spouse has XREF - use XREF as indiv_id, spouse in current family, child in XREF family
          spouse_indiv_id <- parsed_spouse$xref_indiv_id
          spouse_spouse_fam_ids <- fam_id_root # Spouse in the current family
          spouse_child_fam_ids <- parsed_spouse$xref_fam_id # Child in their birth family
        } else {
          # No XREF - use generated ID
          spouse_indiv_id <- paste0(fam_id, "S", spouse_num)
          spouse_spouse_fam_ids <- fam_id_root
          spouse_child_fam_ids <- character(0)
        }

        current_relationship <- "primary_spouse"
        family[[i]] <- make_person_with_separate_fam_ids(
          person,
          spouse_indiv_id,
          spouse_fam_ids = spouse_spouse_fam_ids,
          child_fam_ids = spouse_child_fam_ids,
          relationship = current_relationship
        )
      } else if (role == "child") {
        current_relationship <- "primary_child"
        indiv_id <- paste0(
          fam_id_root,
          ".",
          str_match(person, "^(\\d+)\\.")[, 2]
        )
        last_child <- i
        family[[i]] <- make_person_with_separate_fam_ids(
          person,
          indiv_id,
          spouse_fam_ids = character(0),
          child_fam_ids = fam_id_root,
          relationship = current_relationship
        )
      } else if (
        (role == "spouse") && (current_relationship == "primary_child")
      ) {
        current_relationship <- "primary_child_spouse"
        # Let the function handle XREF extraction
        temp_person <- make_person_with_separate_fam_ids(
          person,
          paste0(fam_id_root, "S", spouse_num + 1), # fallback ID
          spouse_fam_ids = character(0),
          child_fam_ids = character(0),
          relationship = current_relationship
        )

        # Get the actual marriage fam_id
        actual_fam_id <- str_extract(temp_person$indiv_id, "^[0-9]+")

        # Set the spouse's own spouse_fam_ids to include the marriage family
        temp_person$spouse_fam_ids <- list(actual_fam_id)

        family[[i]] <- temp_person

        # Add marriage family to child's spouse families
        if (!is.na(last_child)) {
          current_spouse_fams <- unlist(family[[last_child]]$spouse_fam_ids)
          family[[last_child]]$spouse_fam_ids <- list(unique(c(
            current_spouse_fams,
            actual_fam_id
          )))
        }
      } else if (
        (role == "spouse") && (current_relationship == "primary_child_spouse")
      ) {
        current_relationship <- "primary_child_spouse"
        temp_person <- make_person_with_separate_fam_ids(
          person,
          paste0(fam_id_root, "S", spouse_num + 1), # fallback ID
          spouse_fam_ids = character(0),
          child_fam_ids = character(0),
          relationship = current_relationship
        )

        # Get the actual marriage fam_id
        actual_fam_id <- str_extract(temp_person$indiv_id, "^[0-9]+")

        # Set the spouse's own spouse_fam_ids to include the marriage family
        temp_person$spouse_fam_ids <- list(actual_fam_id)

        family[[i]] <- temp_person

        # Add marriage family to child's spouse families
        if (!is.na(last_child)) {
          current_spouse_fams <- unlist(family[[last_child]]$spouse_fam_ids)
          family[[last_child]]$spouse_fam_ids <- list(unique(c(
            current_spouse_fams,
            actual_fam_id
          )))
        }
      } else if (
        (role == "spouse") && (current_relationship == "primary_child_spouse")
      ) {
        current_relationship <- "primary_child_spouse"
        temp_person <- make_person_with_separate_fam_ids(
          person,
          paste0(fam_id_root, "S", spouse_num + 1), # fallback ID
          spouse_fam_ids = character(0),
          child_fam_ids = character(0),
          relationship = current_relationship
        )
        family[[i]] <- temp_person

        # Get the actual marriage fam_id
        actual_fam_id <- str_extract(temp_person$indiv_id, "^[0-9]+")

        # Add marriage family to child's spouse families
        if (!is.na(last_child)) {
          current_spouse_fams <- unlist(family[[last_child]]$spouse_fam_ids)
          family[[last_child]]$spouse_fam_ids <- list(unique(c(
            current_spouse_fams,
            actual_fam_id
          )))
        }
      }
    }
  }

  map_dfr(
    family,
    ~ {
      # Get existing events or create empty tibble
      existing_events <- if (
        length(.x$events) > 0 && !is.null(.x$events[[1]])
      ) {
        .x$events[[1]]
      } else {
        tibble(
          event = character(),
          date = character(),
          place = character(),
          text = character()
        )
      }

      # Add the record as a NOTE event
      record_note <- tibble(
        event = "NOTE",
        date = NA_character_,
        place = NA_character_,
        text = paste(.x$record, collapse = " ")
      )

      # Combine events
      combined_events <- bind_rows(existing_events, record_note)

      tibble(
        record = paste(.x$record, collapse = " "),
        name = rearrange_name(.x$name),
        events = list(combined_events),
        indiv_id = .x$indiv_id,
        spouse_fam_ids = .x$spouse_fam_ids,
        child_fam_ids = .x$child_fam_ids,
        relationship = .x$relationship
      )
    }
  )
}

# Updated helper function
make_person_with_separate_fam_ids <- function(
  record,
  indiv_id,
  spouse_fam_ids = character(0),
  child_fam_ids = character(0),
  relationship = "primary"
) {
  parsed <- parse_person_lines(record)[[1]]

  # For non-primary relationships, use XREF indiv_id if present
  # For primary relationship, the indiv_id is already correctly passed in
  final_indiv_id <- if (
    relationship != "primary" && !is.null(parsed$xref_indiv_id)
  ) {
    parsed$xref_indiv_id
  } else {
    indiv_id
  }

  # Don't automatically add XREF to spouse_fam_ids here -
  # let the calling function handle the logic

  tibble(
    record = record,
    name = rearrange_name(parsed$name), # Apply name rearrangement here
    events = list(parsed$events),
    indiv_id = final_indiv_id,
    spouse_fam_ids = list(spouse_fam_ids),
    child_fam_ids = list(child_fam_ids),
    relationship = relationship
  )
}

# perform reconciliation after the make_family() step
# Scan the cross references in each record to check if it
# references the same person as a child in a different family.
#   As an example, all_families[[4]][1,] shows that indiv_id 26 is
# cross-referenced to 23.4 so he is the fourth child of indiv_id 23.
# go back and change instances of indiv_id=23.4 to indiv_id=26
# in any prior record.
reconcile_families <- function(all_families) {
  cat("Starting reconciliation...\n")

  # Step 1: Build a mapping of cross-references
  xref_mapping <- tibble(
    old_indiv_id = character(),
    new_indiv_id = character(),
    family_idx = integer()
  )

  # Find all individuals who are family heads and also children elsewhere
  for (i in seq_along(all_families)) {
    family <- all_families[[i]]
    head_person <- family[1, ] # First person is always the family head

    # Check if this head person has child_fam_ids (meaning they're also a child)
    if (length(head_person$child_fam_ids[[1]]) > 0) {
      parent_fam_id <- head_person$child_fam_ids[[1]][1]

      # Look for this person as a child in the parent family
      for (j in seq_along(all_families)) {
        if (j == i) {
          next
        } # Skip the same family

        parent_family <- all_families[[j]]
        parent_head <- parent_family[1, ]

        # Check if this family has the matching family ID
        if (parent_head$indiv_id == parent_fam_id) {
          # Look for children in this parent family
          children <- parent_family %>% filter(relationship == "primary_child")

          for (k in 1:nrow(children)) {
            child <- children[k, ]

            # Check if this child has the same name (indicating same person)
            if (
              str_detect(child$name, str_extract(head_person$name, "\\w+")) ||
                str_detect(head_person$name, str_extract(child$name, "\\w+"))
            ) {
              # This is a match! Record the mapping
              xref_mapping <- bind_rows(
                xref_mapping,
                tibble(
                  old_indiv_id = child$indiv_id,
                  new_indiv_id = head_person$indiv_id,
                  family_idx = j,
                  child_row = k + 1 # +1 because head is row 1
                )
              )

              cat(
                "Found match:",
                child$indiv_id,
                "is also ",
                head_person$indiv_id,
                "\n"
              )
            }
          }
        }
      }
    }
  }

  # Step 2: Apply the mappings (UPDATE IDs ONLY, DO NOT REMOVE ROWS)
  if (nrow(xref_mapping) > 0) {
    for (mapping_idx in 1:nrow(xref_mapping)) {
      mapping <- xref_mapping[mapping_idx, ]
      old_id <- mapping$old_indiv_id
      new_id <- mapping$new_indiv_id

      cat("Updating", old_id, "to", new_id, "\n")

      # Update all references throughout all families
      for (i in seq_along(all_families)) {
        family <- all_families[[i]]

        # Update indiv_id references
        for (j in 1:nrow(family)) {
          if (family[j, ]$indiv_id == old_id) {
            all_families[[i]][j, ]$indiv_id <- new_id
          }

          # Update spouse_fam_ids references
          spouse_fam_ids <- family[j, ]$spouse_fam_ids[[1]]
          updated_spouse_fam_ids <- map_chr(spouse_fam_ids, function(fam_id) {
            if (fam_id == old_id) new_id else fam_id
          })
          if (!identical(spouse_fam_ids, updated_spouse_fam_ids)) {
            all_families[[i]][j, ]$spouse_fam_ids <- list(
              updated_spouse_fam_ids
            )
          }

          # Update child_fam_ids references
          child_fam_ids <- family[j, ]$child_fam_ids[[1]]
          updated_child_fam_ids <- map_chr(child_fam_ids, function(fam_id) {
            if (fam_id == old_id) new_id else fam_id
          })
          if (!identical(child_fam_ids, updated_child_fam_ids)) {
            all_families[[i]][j, ]$child_fam_ids <- list(updated_child_fam_ids)
          }
        }
      }
    }
  }

  cat("Reconciliation complete.\n")
  return(all_families)
}
# Update the end of parse_family_book.R to include reconciliation
all_families <- records |>
  map(separate_people) |>
  map(make_family) |>
  reconcile_families()


# save all_families as a file
saveRDS(all_families, file = "data/all_families.rds")
