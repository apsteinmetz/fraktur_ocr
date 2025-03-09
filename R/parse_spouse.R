# extract and parse spouse records
library(tidyverse)
source("r/gedcom_functions.r")
PROGRESS <- TRUE
global_count = 0
break_tags <- c(
  "NAME",
  "DATE",
  "XREF",
  "BIRT",
  "MARR",
  "DEAT",
  "BURI",
  "PLAC",
  "RELI",
  "WITN",
  "GODP",
  "BAPM",
  "Eltern:",
  "Wohnort",
  "letzter",
  "NOTE"
)

extract_date_ged <- function(text) {
  missing_date <- FALSE
  missing_place <- FALSE
  #date_regex <- paste0("^(\\d{4}-\\d{2}\\.\\d{2})|\\s+(ABT \\d{4})")
  date_regex <- paste0("^\\d{4}-\\d{2}-\\d{2}")
  # extract date from record
  tokens <- text |> str_squish() |> str_split(" ") |> unlist()
  # if 2nd  token is either ABT, BEF or BET paste second and third token together
  if (tokens[2] %in% c("ABT", "BEF", "BET")) {
    date <- paste(tokens[2], tokens[3])
    start_char <- 4
  } else {
    # is it a valid date?
    if (str_detect(tokens[2], date_regex)) {
      date <- tokens[2]
      start_char <- 3
    } else {
      # placeholder. need to remove date tag while keeping PLAC
      missing_date <- TRUE
      start_char <- 2
      date <- ""
    }
  }
  if (str_detect(date, "BET")) {
    date <- str_replace(date, "-", " AND ")
  }
  #paste remaining tokens together as PLAC
  if (length(tokens) > start_char - 1) {
    place <- paste0(
      "2 PLAC ",
      paste0(tokens[start_char:length(tokens)], collapse = " "),
      "\n")
  } else {
    missing_place <- TRUE
  }

  if  (!missing_date & !missing_place) ged <- paste0("2 DATE ", fix_dates_2(date), "\n", place)
  if  (!missing_date & missing_place)  ged <-  paste0("2 DATE ", fix_dates_2(date), "\n")
  if  (missing_date & !missing_place)  ged <- place

  return(ged)
}

extract_date_ged_v <- Vectorize(extract_date_ged)
separate_all_tags <- function(records) {
  if (PROGRESS) cat('Separating Records GED tags\n')
  pattern <- paste0("(", paste(break_tags, collapse = "|"), ")")
  all_records_data <- records |>
    mutate(record = str_split(record, pattern)) |>
    unnest(record)
  # extract all break tags as a column
  all_records_tags <- records |>
    mutate(tags = str_extract_all(paste("NAME", record), pattern)) |>
    unnest(tags) |>
    select(tags)
  all_records <- cbind(all_records_data, all_records_tags) |>
    #  all_records <- all_records_data |>
    as_tibble() |>
    mutate(record = trimws(paste(tags, record))) |>
    # remove empty elements
    filter(str_length(record) > 4) |>
    select(-tags)
  return(all_records)
}

fix_unknown_names <- function(records){
  records |>
    mutate(spouse0 = str_replace_all(spouse0,"NN\\.","UNKNOWN"))
}

fix_marriages_ged <- function(text) {
  global_count <<- global_count + 1
  text <- str_squish(text)
  if (str_detect(text, "NAME")) {
    name <- str_extract(text, "[ÜÄA-Z.]+ [A-Z][äüa-z]+")
    if (!is.na(name)) {
      remaining_text <- str_remove(text, name) |>
        # remove NAME from remaining text and add back to name
      str_remove("NAME") |>
        trimws()
      name <- paste0("NAME ", name)
    } else {
      name <- ""
      remaining_text <- text |>
        str_remove("NAME") |>
        trimws()
    }
    # now try to extract the date and place
    if (str_length(remaining_text) > 0) {
      marriage_date_plac <-
        extract_date_ged(paste("DATE", remaining_text))
    } else {
      marriage_date_plac <- ""
    }
    text <- return(paste0(name," MARR ",marriage_date_plac))
  }
  return(text)
}
fix_marraiges_ged_v <- Vectorize(fix_marriages_ged)

add_name_tag <- function(records) {
  records <- records |>
    mutate(spouse0 = str_replace(spouse0,"([ÜÄA-Z.]+ [A-Z][äüa-z]+)","NAME \\1"))
  return(records)
}

load("data/records.RData")
spouses <- records |>
  filter(!is.na(spouse0)) |>
  fix_unknown_names() |>
  add_name_tag() |>
  mutate(record = spouse0) |>
  select(ID, record, spouse0) |>
  separate_all_tags()

num <- 1
cat("Extracting Marriage Data\n")
temp <- spouses |>
  mutate(record = fix_marraiges_ged_v(record)) |>
  # separate again now that MARR data is separated
  separate_all_tags()


