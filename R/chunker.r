# separate txt file into chunks
library(tidyverse)
library(progress)
#' Function to read the file and extract records
#'
#' @param file_path Path to the text file.
#' @return A list of character vectors, each vector representing a record.
read_records <- function(file_path) {
   # Read the file line by line
   lines_raw <- readLines(file_path, encoding = "UTF-8")  # Handle character encoding
   # get rid of surname block headers
   lines <- vector()
   i = 1
   while (i <= length(lines_raw)) {
      cat(lines_raw[i])
      if (grepl("^[A-Z]+$", lines_raw[i])) {
         i <- i +2 #skip this and the next line
      } else {
         lines <- c(lines, lines_raw[i])
         cat(lines[i],"\n")
         i <- i + 1
      }

   }
   # replace '<num>' with '@I<num>@'
   lines <- gsub("<(\\d+)>", "@I\\1@", lines)
   # remove empty lines
   lines <- lines[!grepl("^\\s*$", lines)]

   # Identify record start lines (lines starting with "<")
   record_start_indices <- grep("^@", lines)

   # Handle edge case of no records being found
   if(length(record_start_indices) == 0){
      warning("No records found in the file.")
      return(list())
   }

   # Extract records.  The person's name is on the line immediately after the record start.
   records <- list()
   for (i in seq_along(record_start_indices)) {
      start_index <- record_start_indices[i]
      end_index <- ifelse(i < length(record_start_indices), record_start_indices[i + 1] - 1, length(lines))

      # Include the person's name line with the record
      record_lines <- lines[start_index:end_index]
      if(start_index +1 <= length(lines)){
         if((start_index+1) %in% record_start_indices){ #check if the next line is a new record before adding it.
            record_lines <- lines[start_index:end_index]
         } else {
            record_lines <- lines[start_index:(end_index)]
         }

      }
      records[[i]] <- record_lines # List of character vectors, each vector being a record
   }

   return(records)
}


file_path <- "data/persons.txt" # Set file path
data <- read_records(file_path)

save_records <- function(data, output_dir = "data/chunks"){
   pb <- progress_bar$new(total = length(data))
   # Save each item in the data list to a separate text file
   for (i in seq_along(data)) {
      writeLines(data[[i]],
                 con = paste0("data/chunks/record_",str_pad(i,width=4,pad = "0"), ".txt"))
      pb$tick()
   }
}

save_records(data)

# save each item in data list to a separate text file
# Print the resulting data frame
print(data)

# Inspect the structure
str(data, list.len = 5)
