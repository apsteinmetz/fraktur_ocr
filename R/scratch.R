library(tidyverse)




# Define the function
separate_by_tags <- function(text) {
  date_regex <- paste0("^(\\d{2}\\.\\d{2}\\.\\d{4})|\\s+(ABT \\d{4})")

  # Create a regular expression pattern with the break tags
  pattern <- paste0("(", paste(break_tags, collapse = "|"), ")")

  # Separate the text based on the pattern
  elements <- str_split(str_trim(text), pattern, simplify = TRUE) |>
    trimws() |>
    # remove empty elements
    keep(~ .x != "")
  # Extract the tags and their corresponding text
  tag_positions <- unlist(str_extract_all(text, pattern))
  # make a new element list if element begins with a date, separate it into 2 parts, date and text and add them to the new list
  new_elements <- list()
  for (i in 1:length(elements)) {
    if (grepl(date_regex, elements[i])) {
      tag <- paste0(tag_positions[i], "\n")
      date <- str_extract(elements[i], date_regex)
      # make not_date the rest of the string
      not_date <- paste0("PLAC ", str_trim(str_remove(elements[i], date)), "\n")
      date <- paste0("DATE ", date, "\n")
      new_elements <- paste0(new_elements, tag, date, not_date)
      # insert "PLAC" tag into tag positions
    } else {
      tag <- tag_positions[i]
      new_elements <- paste0(new_elements, tag, " ", elements[i], "\n")
    }
  }
  # remove PLAC if no following text
  new_elements_list <- new_elements |>
    str_remove_all("PLAC \n") |>
    str_replace_all("\n\n", "\n") |>
    str_split("\n") |>
    unlist()
  new_elements_list <- new_elements_list[which(new_elements_list != "")]

  # if Element does not start with a break tag, add the "NOTE" tag to the beginning
  for (i in 1:length(new_elements_list)) {
    if (!str_detect(new_elements_list[i], pattern)) {
      print(new_elements_list[i])
      new_elements_list[i] <- paste0("NOTE ", new_elements_list[i])
    }
  }

  return(trimws(new_elements_list))
}

# Example usage
break_tags <- c(
  "BIRT",
  "MARR",
  "DEAT",
  "BURI",
  "PLAC",
  "RELI",
  "WITN",
  "GODP",
  "BAPM",
  "NOTE"
)
break_sub_tags <- c("WITN", "GODP", "AGE", "PLAC")

# function to create family record using using ID as family and ID as individual and person as spouse
create_family_record <- function(id, spouse, children) {
   id <- record$ID
   spouse <- record$spouse0
   children <- record$relatives

  family_record <- paste0(
    "@F", id, "@ FAM\n",
    "HUSB @I", id, "@\n",
    "WIFE @I", spouse, "@\n"
  )
  for (child in children) {
    family_record <- paste0(family_record, "CHIL @I", child, "@\n")
  }
  return(family_record)
}

create_family_record(records)

# function to group by ID and collapse all records into a single record
collapse_records <- function(records) {
  records <- records |>
    group_by(ID) |>
    summarize(tag_ged =str_c(tag_ged, collapse = "\n"), .groups = "drop")
  return(records)
}


records_ged <- collapse_records(records) |>
  left_join(distinct(select(records,ID,gedcom)), by = "ID") |>
  mutate(gedcom = paste0(gedcom,tag_ged)) |>
  select(ID,gedcom)

# extract gedcom as a single character vector
gedcom <- records_ged |>
  pull(gedcom) |>
  str_c(collapse = "\n")

make_header_col <- function(records) {
  # Extract the person's name from the first line of the record
  records <- records |>
    mutate(.before = ID,gedcom = make_individual_ged_v(ID))
  return(records)
}

make_tag_ged <- function(records) {
  records <- records |>
    mutate(.before = ID,tag_ged = make_ged_v(record))
  return(records)
}

# write gedcom out to a text file as a string of characters, not using writeLines
# writeChar(gedcom, con = "data/gedcom.ged")
