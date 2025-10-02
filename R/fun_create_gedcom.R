families_to_gedcom <- function(all_families) {
  map(all_families, function(family_tibble) {
    gedcom_lines <- character()
    cat("Processing family with ", nrow(family_tibble), " members...\n")
    # display first row of family_tibble
    cat(family_tibble[1, ]$record, "\n")

    # Get all unique family IDs from both spouse and child family columns
    all_spouse_fam_ids <- unique(unlist(family_tibble$spouse_fam_ids))
    all_child_fam_ids <- unique(unlist(family_tibble$child_fam_ids))
    all_fam_ids <- unique(c(all_spouse_fam_ids, all_child_fam_ids))
    primary_fam_id <- family_tibble$spouse_fam_ids[[1]][1] # Primary person's first spouse family

    # Process each person in the family
    for (i in 1:nrow(family_tibble)) {
      person <- family_tibble[i, ]
      # Create individual record
      gedcom_lines <- c(gedcom_lines, paste0("0 @I", person$indiv_id, "@ INDI"))

      # Add name in GEDCOM format
      if (!is.na(person$name) && person$name != "") {
        # Split name into given names and surname
        name_parts <- str_split(person$name, " ")[[1]]
        if (length(name_parts) >= 2) {
          given_names <- paste(name_parts[-length(name_parts)], collapse = " ")
          surname <- name_parts[length(name_parts)]
          gedcom_name <- paste0(given_names, " /", surname, "/")
        } else {
          gedcom_name <- paste0(person$name, " //")
        }
        gedcom_lines <- c(gedcom_lines, paste0("1 NAME ", gedcom_name))
      }

      # Process events from the events tibble - handle NULL/empty events
      # EXCLUDE MARR events from individual records
      events <- if (length(person$events) > 0) person$events[[1]] else NULL
      if (!is.null(events) && nrow(events) > 0) {
        # Filter out MARR events - they belong in family records only
        non_marriage_events <- events |> filter(event != "MARR")

        if (nrow(non_marriage_events) > 0) {
          for (j in 1:nrow(non_marriage_events)) {
            event <- non_marriage_events[j, ]

            # Build event line
            event_line <- paste0("1 ", event$event)

            # Add date if present
            if (!is.na(event$date) && event$date != "") {
              event_line <- c(event_line, paste0("2 DATE ", event$date))
            }

            # Add place if present
            if (!is.na(event$place) && event$place != "") {
              event_line <- c(event_line, paste0("2 PLAC ", event$place))
            }

            # Add text for NOTE events (and any other events with text)
            if (!is.na(event$text) && event$text != "") {
              if (event$event == "NOTE") {
                # For NOTE events, add text directly after the NOTE tag
                event_line[1] <- paste0("1 NOTE ", event$text)
              } else {
                # For other events, add as subordinate NOTE
                event_line <- c(event_line, paste0("2 NOTE ", event$text))
              }
            }

            gedcom_lines <- c(gedcom_lines, event_line)
          }
        }
      }

      # Add family links using separate spouse and child family columns
      person_spouse_fam_ids <- person$spouse_fam_ids[[1]]
      person_child_fam_ids <- person$child_fam_ids[[1]]

      # Add FAMS links (spouse relationships)
      if (length(person_spouse_fam_ids) > 0) {
        for (fam_id in person_spouse_fam_ids) {
          gedcom_lines <- c(gedcom_lines, paste0("1 FAMS @F", fam_id, "@"))
        }
      }

      # Add FAMC links (child relationships) - ensure each person is child of only one family
      if (length(person_child_fam_ids) > 0) {
        # Take only the first child family ID to ensure no child belongs to multiple families
        child_fam_id <- person_child_fam_ids[1]
        gedcom_lines <- c(gedcom_lines, paste0("1 FAMC @F", child_fam_id, "@"))
      }
    }

    # Create FAM blocks for ALL family IDs
    for (fam_id in all_fam_ids) {
      gedcom_lines <- c(gedcom_lines, paste0("0 @F", fam_id, "@ FAM"))
      cat("Creating family record for:", fam_id, "\n")

      # Find husband(s) - people who have this fam_id in their spouse_fam_ids and are male/primary
      husbands <- family_tibble |>
        filter(map_lgl(spouse_fam_ids, ~ fam_id %in% .x)) |>
        filter(relationship %in% c("primary", "primary_child"))

      if (nrow(husbands) > 0) {
        gedcom_lines <- c(
          gedcom_lines,
          paste0("1 HUSB @I", husbands$indiv_id[1], "@")
        )
      }

      # Find wife/wives - people who have this fam_id in their spouse_fam_ids and are female/spouse
      wives <- family_tibble |>
        filter(map_lgl(spouse_fam_ids, ~ fam_id %in% .x)) |>
        filter(relationship %in% c("primary_spouse", "primary_child_spouse"))

      if (nrow(wives) > 0) {
        for (w in 1:nrow(wives)) {
          gedcom_lines <- c(
            gedcom_lines,
            paste0("1 WIFE @I", wives$indiv_id[w], "@")
          )
        }
      }

      # Find children - people who have this fam_id in their child_fam_ids
      children <- family_tibble |>
        filter(map_lgl(child_fam_ids, ~ fam_id %in% .x))

      if (nrow(children) > 0) {
        for (c in 1:nrow(children)) {
          gedcom_lines <- c(
            gedcom_lines,
            paste0("1 CHIL @I", children$indiv_id[c], "@")
          )
        }
      }

      # Add marriage events from spouses in this family
      if (nrow(wives) > 0) {
        for (w in 1:nrow(wives)) {
          wife <- wives[w, ]
          spouse_events <- if (length(wife$events) > 0) {
            wife$events[[1]]
          } else {
            NULL
          }
          if (!is.null(spouse_events) && nrow(spouse_events) > 0) {
            marriage_events <- spouse_events |> filter(event == "MARR")
            if (nrow(marriage_events) > 0) {
              for (m in 1:nrow(marriage_events)) {
                marriage <- marriage_events[m, ]
                gedcom_lines <- c(gedcom_lines, "1 MARR")
                if (!is.na(marriage$date) && marriage$date != "") {
                  gedcom_lines <- c(
                    gedcom_lines,
                    paste0("2 DATE ", marriage$date)
                  )
                }
                if (!is.na(marriage$place) && marriage$place != "") {
                  gedcom_lines <- c(
                    gedcom_lines,
                    paste0("2 PLAC ", marriage$place)
                  )
                }
              }
            }
          }
        }
      }
    }

    # Return as single string with newlines
    paste(gedcom_lines, collapse = "\n")
  })
}


# Update your GEDCOM snippets - process ALL families, not just one
gedcom_snippets <- families_to_gedcom(all_families)

# combine all snippets into a single GEDCOM file content with a header and footer
gedcom_header <- "0 HEAD\n1 SOUR RPackage\n1 GEDC\n2 VERS 5.5.1\n1 CHAR UTF-8"
gedcom_footer <- "0 TRLR"
full_gedcom <- paste(
  c(gedcom_header, gedcom_snippets, gedcom_footer),
  collapse = "\n"
)

# save to file
writeLines(full_gedcom, "data/output.ged")
