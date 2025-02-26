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

  return(new_elements_list)
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


separate_by_tags(records$record[1])

for (i in 1:10) {
  separate_by_tags(records$record[i])
  print("")
}
