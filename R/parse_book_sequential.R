library(tidyverse)

source("R/gedcom_functions.R")
FILE_PATH <- "data/persons_sm.txt"
PROGRESS <- TRUE
read_records <- function(file_path=FILE_PATH) {
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
records <- read_records(FILE_PATH)

# apply the tag_text function to each line
tagged_lines <- map(records, tag_text) |> 
  unlist()

# separate into list of records by splitting at <I\d+> tags
record_indices <- which(grepl("^<\\d+>", tagged_lines))
record_indices <- c(record_indices, length(tagged_lines) + 1) # add end index

records_list <- list()
# for loop version
for (i in seq_along(record_indices[-length(record_indices)])) {
  if (PROGRESS) cat(i, " ")
  start_idx <- record_indices[i]
  end_idx <- record_indices[i + 1] - 1
  records_list[[i]] <- tagged_lines[start_idx:end_idx]
}

get_person <- function(family_record) {
  # Initialize list to store person records
  persons <- list()
  
  if (length(family_record) == 0) return(persons)
  
  # Extract family ID from first line
  family_id_line <- family_record[1]
  family_id <- str_extract(family_id_line, "(?<=<)\\d+(?=>)")
  
  if (is.na(family_id)) return(persons)
  
  # Helper function to parse events from lines
  parse_events_from_lines <- function(lines) {
    events <- list()
    
    for (line in lines) {
      # Skip ID lines and empty lines
      if (str_detect(line, "^<\\d+>$") || str_trim(line) == "") next
      
      # Define event tags to look for
      event_tags <- c("BIRT", "DEAT", "MARR", "BURI", "BAPM", "RELI", "PLAC", "XREF")
      
      for (tag in event_tags) {
        # Look for the tag in the line
        if (str_detect(line, paste0("\\b", tag, "\\b"))) {
          
          # Extract content after the tag
          tag_pattern <- paste0(tag, "\\s+([^\\s]+(?:\\s+[^A-Z\\s][^\\s]*)*(?:\\s+[A-Z][^\\s]*)*?)(?=\\s+[A-Z]{3,}|$)")
          tag_matches <- str_match_all(line, tag_pattern)[[1]]
          
          if (nrow(tag_matches) > 0) {
            for (i in 1:nrow(tag_matches)) {
              content <- str_trim(tag_matches[i, 2])
              
              event <- list(
                event_type = tag,
                raw_content = content
              )
              
              # Parse specific content based on tag type
              if (tag == "BIRT" || tag == "DEAT" || tag == "MARR" || tag == "BURI" || tag == "BAPM") {
                # Parse date (including ABT dates)
                date_patterns <- c(
                  "ABT\\s+(\\d{4})",              # ABT 1811
                  "(\\d{2}\\.\\d{2}\\.\\d{4})",   # 11.04.1866
                  "([A-Za-z]+\\s+\\d{4})",        # Juni 1852
                  "(\\d{4})"                      # 1852
                )
                
                for (date_pattern in date_patterns) {
                  date_match <- str_extract(content, date_pattern)
                  if (!is.na(date_match)) {
                    event$date <- date_match
                    if (str_detect(date_match, "ABT")) {
                      event$date_qualifier <- "ABT"
                      event$date <- str_extract(date_match, "\\d{4}")
                    }
                    break
                  }
                }
              } else if (tag == "PLAC") {
                event$place <- content
              } else if (tag == "RELI") {
                event$religion <- content
              } else if (tag == "XREF") {
                event$cross_reference <- content
              }
              
              # Create unique event key
              event_key <- paste0(tag, "_", length(events) + 1)
              events[[event_key]] <- event
            }
          }
        }
      }
    }
    
    return(events)
  }
  
  # Helper function to extract name from a line
  extract_person_name <- function(line) {
    # Remove event tags and content after them
    clean_line <- str_replace_all(line, "\\b(BIRT|DEAT|MARR|BURI|BAPM|RELI|PLAC|ABT|AGE|XREF)\\b.*$", "")
    clean_line <- str_replace(clean_line, "^\\d+\\.\\s*", "") # Remove number prefix
    clean_line <- str_trim(clean_line)
    
    # Extract name (assuming format: SURNAME Given names)
    name_match <- str_match(clean_line, "^([A-ZÄÖÜ]+)\\s+([A-ZÄÖÜ][a-zäöüß]+(?:\\s+[A-ZÄÖÜ][a-zäöüß]+)*)")
    if (!is.na(name_match[1])) {
      return(list(
        full_name = str_trim(paste(name_match[2], name_match[3])),
        surname = name_match[2],
        given_names = name_match[3]
      ))
    }
    
    return(list(full_name = clean_line))
  }
  
  # Initialize variables to track current person context
  current_person_id <- family_id
  current_person_data <- list()
  spouse_created <- FALSE
  i <- 1
  
  while (i <= length(family_record)) {
    line <- family_record[i]
    
    # Check for head of household pattern <\d+>
    if (str_detect(line, "^<\\d+>$")) {
      # If we have accumulated person data, save it
      if (length(current_person_data) > 0) {
        persons[[length(persons) + 1]] <- current_person_data
      }
      
      # Start new head of household
      current_person_id <- str_extract(line, "(?<=<)\\d+(?=>)")
      current_person_data <- list(
        person_id = current_person_id,
        family_id = current_person_id,
        relationship = "head",
        lines = c(line)
      )
      spouse_created <- FALSE
      i <- i + 1
      next
    }
    
    # Check for child pattern (number followed by period)
    child_match <- str_match(line, "^(\\d+)\\.")
    if (!is.na(child_match[1])) {
      # Save previous person if exists
      if (length(current_person_data) > 0) {
        # Parse events for current person before saving
        current_person_data$name <- extract_person_name(current_person_data$lines[2])
        current_person_data$events <- parse_events_from_lines(current_person_data$lines)
        persons[[length(persons) + 1]] <- current_person_data
      }
      
      # Start new child
      child_number <- child_match[2]
      current_person_id <- paste0(family_id, ".", child_number)
      current_person_data <- list(
        person_id = current_person_id,
        family_id = family_id,
        relationship = "child",
        child_number = as.numeric(child_number),
        lines = c(line)
      )
      i <- i + 1
      next
    }
    
    # Check for spouse after MARR (only for head of household)
    if (str_detect(line, "^MARR\\s*$") && 
        length(current_person_data) > 0 && 
        current_person_data$relationship == "head" && 
        !spouse_created) {
      
      # Look for spouse name on next line
      if (i < length(family_record)) {
        next_line <- family_record[i + 1]
        
        # If next line contains a name (not starting with number or tag)
        if (str_detect(next_line, "[A-Za-z]") && 
            !str_detect(next_line, "^\\d+\\.") && 
            !str_detect(next_line, "^[A-Z]{3,4}\\s")) {
          
          # Parse current head of household and save
          current_person_data$name <- extract_person_name(current_person_data$lines[2])
          current_person_data$events <- parse_events_from_lines(current_person_data$lines)
          persons[[length(persons) + 1]] <- current_person_data
          
          # Create spouse record
          spouse_lines <- c(line, next_line)
          spouse_name <- extract_person_name(next_line)
          spouse_events <- parse_events_from_lines(spouse_lines)
          
          current_person_data <- list(
            person_id = paste0(family_id, ".spouse"),
            family_id = family_id,
            relationship = "spouse",
            name = spouse_name,
            events = spouse_events,
            lines = spouse_lines
          )
          
          spouse_created <- TRUE
          i <- i + 2  # Skip both MARR and spouse name line
          next
        }
      }
    }
    
    # Add line to current person's data
    if (length(current_person_data) > 0) {
      current_person_data$lines <- c(current_person_data$lines, line)
    }
    
    i <- i + 1
  }
  
  # Don't forget to add the last person
  if (length(current_person_data) > 0) {
    # Parse name if not already done
    if (!exists("name", current_person_data)) {
      name_line <- current_person_data$lines[2]
      current_person_data$name <- extract_person_name(name_line)
    }
    # Parse events
    current_person_data$events <- parse_events_from_lines(current_person_data$lines)
    persons[[length(persons) + 1]] <- current_person_data
  }
  
  return(persons)
}

# Test the enhanced function
# Test on the first record

person <- get_person(records_list[[1]])
person
