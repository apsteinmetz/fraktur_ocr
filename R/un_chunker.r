# separate txt file into chunks
library(tidyverse)


to_just_gedcom <- function(record){
   text <- record |> str_flatten(collapse = "\n",last = "")
   if (str_detect(record[1],"HEAD")){
      text <- sub(".*?(@.*?)", "\\1", text, perl = FALSE)
   } else {
      text <- sub("(.*)?```gedcom\\n(.*?)```.*", "\\2", text, perl = FALSE)
   }
   # strip header
   text <- strsplit(text,"\n")
   return(text)
}

progress_info = list(
   type = "iterator",
   format = "Calculating {cli::pb_bar} {cli::pb_percent}",
   clear = TRUE)

RECS_TO_READ <- Inf
read_gedcom <- function(input_dir = "data/gedcom"){
   data_raw <- input_dir |>
      list.files(full.names = TRUE,pattern = ".+\\.ged") |>
      head(RECS_TO_READ) |>
      map(readLines,.progress = progress_info)

   data <- data_raw |>
      map(to_just_gedcom,.progress = progress_info) |>
      unlist(recursive = FALSE)
   return(data)
}

gedcom_raw <- read_gedcom()

gedcom <- gedcom_raw |>
   enframe(name = NULL,value = "gedcom") |>
   mutate(gedcom = str_squish(gedcom)) |>
   separate_wider_delim(gedcom,
                        delim = " ",
                        names = c("level","tag","data"),
                        too_many = "merge",
                        too_few = "align_start") |>
   # move any @id@ to a to tag column and make level 0
   mutate(data = ifelse(str_detect(level,"^@.*"),tag,data),
          tag = ifelse(str_detect(level,"^@.*"),level,tag),
          level = ifelse(str_detect(level,"^@.*"),0,level)
   ) |>
   # add row numbers
   # filter(str_detect(tag,"^@")) |>
mutate(.before = everything(),row_num = row_number())

