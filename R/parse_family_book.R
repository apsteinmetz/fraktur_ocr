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
   text_vec <- gsub(" TZ:"," WITN ",text_vec)
   text_vec <- gsub("\\(†mit (.*?)\\)", "AGE \\1 ", text_vec)
   # text_vec <- gsub("\\(†mit(.+)\\)","AGE \\1 ",text_vec)
   text_vec <- gsub("([0-9]{1,2})J","\\1 years ",text_vec)
   text_vec <- gsub("([0-9]{1,2})M","\\1 months ",text_vec)
   text_vec <- gsub("([0-9]{1,2})T","\\1 days ",text_vec)
   text_vec <- gsub("†"," DEAT ",text_vec)
   text_vec <- gsub("b. "," BURI ",text_vec)
   text_vec <- gsub(" AS"," PLAC Alt Schowe ",text_vec)
   text_vec <- gsub(" NS"," PLAC Neu Schowe ",text_vec)
   text_vec <- gsub("( Lager [a-zA-Z]+)"," PLAC \\1",text_vec)
   text_vec <- gsub(" ev\\."," RELI Evangelical ",text_vec)
   text_vec <- gsub(" ref\\."," RELI Reformed ",text_vec)
   text_vec <- gsub(" kath\\."," RELI Catholic ",text_vec)
   text_vec <- gsub(" TZ:"," WITN ",text_vec)
   text_vec <- gsub(" TP:"," GODP ",text_vec)
   text_vec <- gsub("~"," BAPM ",text_vec)
   text_vec <- gsub("# ","NOTE ",text_vec)
   text_vec <- gsub("[<>] (\\d{1,4}\\.\\d{1,2})"," XREF \\1 ",text_vec)
   text_vec <- gsub("[<>] (\\d{1,4})"," XREF \\1 ",text_vec)
   return (text_vec)
}

break_tags <- c("XREF","BIRT","MARR","DEAT","BURI","PLAC","RELI","WITN","GODP","BAPM","NOTE")
break_sub_tags <- c("WITN","GODP","AGE","PLAC")

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

extract_name <- Vectorize(function(text, type = c("raw", "formatted", "surname")) {
   # Split the text into words
   words <- str_split(text, "\\s+")[[1]]
   # Check if there is a third word and if it's a lowercase alphabetic string
   if (length(words) >= 3 && grepl("^[A-Z][a-z]+$", words[3])) {
      # Append the third word if it qualifies
      name_vec <- c(words[1], c(words[2], words[3]))
   } else {
      name_vec <- c(words[1], words[2])
   }
   name <- switch(
      type,
      raw = paste(name_vec, collapse = " "),
      formatted = paste0(paste0(name_vec[2:length(name_vec)], collapse = " "),
                         " /", name_vec[1], "/"),
      surname = words[1],
      stop("Invalid type specified")
   )
   return(name)
})
extract_tag <- function(text, tag) {
   # extract substring starting with "tag" and ending with any other tag from break_tags
   # or end of string

   startregex <- paste0(tag, ".*?\\n")
   myregex <- paste0(tag,".+\\n|", paste(break_tags, collapse = "|"))
   matches <- str_extract(text, myregex)
   return(matches)
}

extract_date <- function(text, tag,type = c("raw","posix","gedcom")) {
      # Regular expression to match the tag word followed by a date in the format dd.mm.yyyy
   myregex <- paste0(tag, "\\s+(\\d{2}\\.\\d{2}\\.\\d{4})|",
                     tag, "\\s+(ABT \\d{4})")

   matches <- str_extract_all(text, myregex)
   if (is.na(matches)) {return(NA)} # tag not found
   if (length(matches[[1]]) > 0) {
      date <- str_replace(matches[[1]][1], paste0(tag, "\\s+"), "")
      date <- switch(type, raw = date,
                     posix = if (grepl("ABT", date)) {
                        # if posix is selected and the date is approximate use jan 1
                        as.Date(paste0(sub("ABT (\\d{4})", "\\1", date), "-", "01-01"))}
                     else{ as.Date(date, format = "%d.%m.%Y") },
                     gedcom = paste0(tag, "\n", date, "\n")
      )

   } else {date <- NA}
   return(date)
}

extract_all_dates_posix <- function(text, tag) {
   # Regular expression to match the tag word followed by a date in the format dd.mm.yyyy
   myregex <- paste0(tag, "\\s+(\\d{2}\\.\\d{2}\\.\\d{4})|",
                     tag, "\\s+(ABT \\d{4})")

   matches <- str_extract_all(text, myregex)
   if (is.na(matches)) {return(NA)} # tag not found
   if (length(matches[[1]]) > 0) {
      dates <- str_replace(matches[[1]], paste0(tag, "\\s+"), "")
      # replace ABT with jan 1
      dates_a <- dates[grepl("ABT", dates)]
      if(length(dates_a > 0)) {
         dates_a <- dates_a |>
            paste0("-01-01") |>
            str_remove("ABT ") |>
            as.Date()
      } else {
         dates_a <- NA
      }
      dates_b <- dates[!grepl("ABT", dates)] |>
         as.Date(dates, format = "%d.%m.%Y")
      dates <- as.Date(c(dates_a,dates_b))
      # remove NA values
      dates <- dates[!is.na(dates)]
   } else {
      dates <- NA
   }
   return(dates)
}

extract_date_v <- Vectorize(extract_date)

make_name_col <- function(records) {
   # Extract the person's name from the first line of the record
   name_records <- records |>
      mutate(surname = extract_name(record,type="surname"),.before = "record") |>
      mutate(name = extract_name(record,type = "formatted"),.before = "record") |>
      # remove name string from record
      mutate(record = str_remove(record,extract_name(record,type = "raw")))
   return(name_records)

}
make_child_col <- function(records) {
   # Extract the person's name from the first line of the record
   child_records <- records |>
      separate_wider_delim(cols = record,
                           delim = regex("\\d+\\. "),
                           names = c("record",
                                     # create columns for up to 20 children
                                     "FAM1","FAM2","FAM3","FAM4","FAM5",
                                     "FAM6","FAM7","FAM8","FAM9","FAM10",
                                     "FAM11","FAM12","FAM13","FAM14","FAM15",
                                     "FAM16","FAM17","FAM18","FAM19","FAM20"),
                           too_many = "merge",
                           too_few = "align_start")
   return(child_records)
}

make_spouse_col <- function(records) {
   # Extract spouse sub-record from MARR tag
   spouse_records <- records |>
      separate_wider_delim(cols = record,
                           delim = regex("MARR"),
                           names = c("record","FAM0"),
                           too_many = "merge",
                           too_few = "align_start")
   return(spouse_records)
}

make_dates_col <- function(records) {
   # add columns for birth and death date of main person
   date_records <- records |>
      mutate(birth = as.Date(extract_date_v(record,"BIRT",type = "posix")),.before = "record") |>
      mutate(death = as.Date(extract_date_v(record,"DEAT",type = "posix")),.before = "record")
 return(date_records)
}


make_gedcom <- function(records) {
   gedcom <- records |>
      mutate(test = paste0("0 @I",ID,"@ INDI\n",
                           "1 NAME ",name,"\n",
                           "1 BIRT\n2 DATE ",extract_date_v(record,"BIRT",type = "raw"),"\n",
                           "1 DEAT\n",
                           "2 DATE ",extract_date_v(record,"DEAT",type = "raw"),"\n",
                            "1 MARR\n",
                            "2 DATE ",extract_date_v(record,"MARR",type = "raw"),"\n",
                            "1 BURI\n",
                            "2 DATE ",extract_date_v(record,"BURI",type = "raw"),"\n",
                            # "2 PLAC ",extract_date(record,"BURI",type = "raw"),"\n",
                            # "1 NOTE ",extract_date(record,"NOTE",type = "raw"),"\n",
                            # "1 FAMC @F",ID,"@\n",
                            # "1 FAMS @F",ID,"@\n",
                            # "1 CHIL @I",ID,"@\n",
                            # "1 SPOU @I",ID,"@\n",
                            # "1 WITN @I",ID,"@\n",
                            # "1 GODP @I",ID,"@\n",
                            # "1 BAPM @I",ID,"@\n",
                            # "1 RELI @I",ID,"@\n",
                            # "1 PLAC @I",ID,"@\n",
                            # "1 AGE @I",ID,"@\n",
                             "1 XREF
                             ")
             )
   return(gedcom)
}

make_gedcom(records[100,]) %>% pull(gedcom) %>% cat()
extract_date(records$record[100],"BIRT",type = "raw")

# file_path <- "data/persons.txt" # Set file path
all_recs <- read_lines(file_path)
#raw_data <- read_records(file_path)
#save(raw_data,file = "data/raw_data.RData")
load("data/raw_data.RData")
# convert to data frame
records_base <- map(raw_data,str_flatten,collapse = "\n") %>%
   enframe(name = NULL,value = "record") |>
   unnest(record) |>
   separate(record,c("ID","record"),sep = " ",extra = "merge") |>
   mutate(ID = as.integer(str_remove_all(ID,"\\D"))) |>
   mutate(record = tag_text(record)) |>
   filter(!str_detect(record,"nach Korrektur unbesetzt"))

records <- records_base |>
   make_name_col() |>
   make_child_col() |>
   make_spouse_col() |>
   pivot_longer(cols = starts_with("FAM"),
                names_to = "relationship",
                values_to = "person") |>
    filter(!is.na(person)) |>
    nest(relatives = c(relationship,person)) |>
   identity()
records

records <- records |>
   make_dates_col()
save(records,file = "data/records.RData")
