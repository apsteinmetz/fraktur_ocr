# parse family records
library(tidyverse)

file_path <- "data/persons.txt"

sample_text_to_parse <-
"@I7@
ALBUS Christian
*um 1842
oo 23.04.1865 Neu Werbas
GEYER Magdalena ev.
*um 1845 † 10.09.1873 AS b. 11.09.1873 (†mit 28J)

1. Albus Magdalena * 22.01.1867 Neu Werbas
1.oo 22.08.1886 NS Hobler Michael > 1579
2.oo 23.03.1893 NS Geiss Josef > 748
2. Albus Katharina *um 1871 Nadalj
oo 22.02.1888 NS Haller Heinrich Georg > 1139
3. Albus Jakob ev. * 03.09.1873 AS † 18.09.1873 Neu Werbas b. 19.09.1873
~ 06.09.1873 TP: Adam SchmidtSofia Werle
nach ev. Matrikel Werbas (†mit 10T als Johanna)
2.o‐o
WOLF Christina * 23.04.1853 NS > 4068.2
Eltern: Wolf JakobKreter Katharina
lebt 1885 in Nadalj

4. Wolf Christina * 16.11.1874 AS
oo Herzberger Georg > 1450
5. Wolf Christian * 15.11.1885 AS
oo 28.07.1907 AS Geist Ethel > 4090"



tag_text <- function(text_vec) {
   # Replace special characters
   # text_vec <- gsub("@I(\\d+)@","@I\\1@ INDI\nNOTE ID=\\1",text_vec)
   # text_vec <- gsub("<(\\d+)>","NOTE ID\\1",text_vec)
   text_vec <- gsub("INDI\n","",text_vec)
   text_vec <- gsub("o‐o"," Unknown_spouse ",text_vec)
   text_vec <- gsub("\\*","BIRT ",text_vec)
   text_vec <- gsub(" um "," ABT ",text_vec)
   text_vec <- gsub("oo","MARR ",text_vec)
   text_vec <- gsub(" TZ:"," WITN: ",text_vec)
   text_vec <- gsub("†mit","aged ",text_vec)
   text_vec <- gsub("([0-9]{1,2})J","\\1 years",text_vec)
   text_vec <- gsub("([0-9]{1,2})M","\\1 months",text_vec)
   text_vec <- gsub("([0-9]{1,2})T","\\1 days",text_vec)
   text_vec <- gsub("†"," DEAT ",text_vec)
   text_vec <- gsub("b. "," BURI ",text_vec)
   text_vec <- gsub(" AS"," PLAC Alt Schowe ",text_vec)
   text_vec <- gsub(" NS"," PLAC Neu Schowe ",text_vec)
   text_vec <- gsub("( Lager [a-zA-Z]+)"," PLAC \\1",text_vec)
   text_vec <- gsub(" ev\\."," RELI Evangelical ",text_vec)
   text_vec <- gsub(" ref\\."," RELI Reformed ",text_vec)
   text_vec <- gsub(" kath\\."," RELI Catholic ",text_vec)
   text_vec <- gsub(" TZ:"," WITN: ",text_vec)
   text_vec <- gsub(" TP:"," GODP: ",text_vec)
   text_vec <- gsub("~"," BAPM ",text_vec)
   text_vec <- gsub("# ","NOTE ",text_vec)
   text_vec <- gsub("[<>] (\\d{1,4}\\.\\d{1,2})"," link to @I\\1@ ",text_vec)
   text_vec <- gsub("[<>] (\\d{1,4})"," link to @I\\1@ ",text_vec)
   return (text_vec)
}

line_break_tags <- c("BIRT","ABT","MARR","DEAT","BURI","PLAC","RELI","WITN","GODP","BAPM","NOTE")

# insert new item in character vector at line break tags
insert_line_breaks <- function(text_vec) {
   for (tag in line_break_tags) {
      text_vec <- gsub(paste0(tag," "),paste0("\n",tag," "),text_vec)
   }
   return(text_vec)
}



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
   lines <- gsub("<(\\d+)>", "@I\\1@ INDI", lines)
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

parse_name <- function(records) {
   # Extract the person's name from the first line of the record
   records <- records |>
      separate_wider_delim(cols = record,
                           delim = regex(" |\\n"),
                           names = c("last","first","record"),
                           too_many = "merge",
                           too_few = "align_start") |>
      mutate(name = paste0(first," /",last,"/"),.before = "record") |>
      select(-last,-first)
   return(records)

}
parse_children <- function(records) {
   # Extract the person's name from the first line of the record
   records <- records |>
      separate_wider_delim(cols = record,
                           delim = regex("\\d+\\. "),
                           names = c("record",
                                     # create columns for up to 20 children
                                     "CHIL1","CHIL2","CHIL3","CHIL4","CHIL5",
                                     "CHIL6","CHIL7","CHIL8","CHIL9","CHIL10",
                                     "CHIL11","CHIL12","CHIL13","CHIL14","CHIL15",
                                     "CHIL16","CHIL17","CHIL18","CHIL19","CHIL20"),
                           too_many = "merge",
                           too_few = "align_start")
   return(records)

}

file_path <- "data/persons.txt" # Set file path
all_recs <- read_lines(file_path)
#raw_data <- read_records(file_path)
#save(raw_data,file = "data/raw_data.RData")
load("data/raw_data.RData")
# convert to data frame
records <- map(raw_data,str_flatten,collapse = "\n") %>%
   enframe(name = NULL,value = "record") |>
   unnest(record) |>
   separate(record,c("ID","record"),sep = " ",extra = "merge") |>
   mutate(ID = as.integer(str_remove_all(ID,"\\D"))) |>
   mutate(record = tag_text(record)) |>
   filter(!str_detect(record,"nach Korrektur unbesetzt"))

records <- records |>
   parse_name()

records <- records |>
   parse_children()
