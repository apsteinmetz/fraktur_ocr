library(tidyverse)

source("r/gedcom_functions.r")
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

