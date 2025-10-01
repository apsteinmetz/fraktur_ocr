# fun create gedcom snippets
families_to_gedcom <- function(all_families) {
  map(all_families, function(family_tibble) {
    gedcom_lines <- character()
    
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
      
      # Process events from the events tibble
      events <- person$events[[1]]
      if (nrow(events) > 0) {
        for (j in 1:nrow(events)) {
          event <- events[j, ]
          
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
          
          gedcom_lines <- c(gedcom_lines, event_line)
        }
      }
      
      # Add family links
      for (fam_id in person$fam_ids[[1]]) {
        if (person$relationship %in% c("primary", "primary_spouse")) {
          gedcom_lines <- c(gedcom_lines, paste0("1 FAMS @F", fam_id, "@"))
        } else {
          gedcom_lines <- c(gedcom_lines, paste0("1 FAMC @F", fam_id, "@"))
        }
      }
    }
    
    # Create family records based on relationships
    family_ids <- unique(unlist(family_tibble$fam_ids))
    
    for (fam_id in family_ids) {
      gedcom_lines <- c(gedcom_lines, paste0("0 @F", fam_id, "@ FAM"))
      
      # Find husband (primary person)
      husband <- family_tibble |> filter(relationship == "primary")
      if (nrow(husband) > 0) {
        gedcom_lines <- c(gedcom_lines, paste0("1 HUSB @I", husband$indiv_id[1], "@"))
      }
      
      # Find wife (primary_spouse)
      wife <- family_tibble |> filter(relationship == "primary_spouse")
      if (nrow(wife) > 0) {
        gedcom_lines <- c(gedcom_lines, paste0("1 WIFE @I", wife$indiv_id[1], "@"))
      }
      
      # Find children for this family
      children <- family_tibble |> 
        filter(map_lgl(fam_ids, ~fam_id %in% .x) & 
               relationship %in% c("primary_child", "primary_child_child"))
      
      for (k in 1:nrow(children)) {
        gedcom_lines <- c(gedcom_lines, paste0("1 CHIL @I", children$indiv_id[k], "@"))
      }
      
      # Add marriage events from spouse records
      spouse_events <- family_tibble |> 
        filter(relationship == "primary_spouse") |>
        pull(events)
      
      if (length(spouse_events) > 0) {
        spouse_events_df <- spouse_events[[1]]
        marriage_events <- spouse_events_df |> filter(event == "MARR")
        
        if (nrow(marriage_events) > 0) {
          for (m in 1:nrow(marriage_events)) {
            marriage <- marriage_events[m, ]
            gedcom_lines <- c(gedcom_lines, "1 MARR")
            
            if (!is.na(marriage$date) && marriage$date != "") {
              gedcom_lines <- c(gedcom_lines, paste0("2 DATE ", marriage$date))
            }
            
            if (!is.na(marriage$place) && marriage$place != "") {
              gedcom_lines <- c(gedcom_lines, paste0("2 PLAC ", marriage$place))
            }
          }
        }
      }
    }
    
    # Return as single string with newlines
    paste(gedcom_lines, collapse = "\n")
  })
}

# Convert all families to GEDCOM snippets
gedcom_snippets <- families_to_gedcom(all_families)

# combine all snippets into a single GEDCOM file content with a header and footer
gedcom_header <- "0 HEAD\n1 SOUR RPackage\n1 GEDC\n2 VERS 5.5.1\n1 CHAR UTF-8"
gedcom_footer <- "0 TRLR"
full_gedcom <- paste(c(gedcom_header, gedcom_snippets, gedcom_footer), collapse = "\n")
# save to file
writeLines(full_gedcom, "data/output.ged")
