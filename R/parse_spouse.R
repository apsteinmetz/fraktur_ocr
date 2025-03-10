# extract and parse spouse records
library(tidyverse)
source("r/gedcom_functions.r")
PROGRESS <- TRUE
GLOBAL_COUNT = 0


fix_marriages_ged <- function(text) {
  global_count <<- global_count + 1
  text <- str_squish(text)
  if (str_detect(text, "NAME")) {
    name <- str_extract(text, name_regex)
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

add_marriage_tag <- function(records) {
  records <- records |>
    mutate(spouse0 = str_replace(spouse0,date_regex_2 ,"MARR \\1"))

  return(records)
}
add_name_tag <- function(records) {
  records <- records |>
    mutate(spouse0 = str_replace(spouse0,name_regex,"NAME \\1"))

  return(records)
}

load("data/records.RData")
spouses <- records |>
  select(ID, spouse0) |>
  filter(!is.na(spouse0)) |>
  # need to stop this from adding NAME to no match records
  add_marriage_tag() |>
 # need to fix
  # # add_name_tag() |>
  mutate(spouse_name =  extract_name_v(spouse0,type = "formatted")) |>
#  mutate(marriage_date = extract_date_ged_v(spouse0)) |>
   mutate(record = str_squish(spouse0)) |>
  select(ID, record, spouse_name, spouse0) |>
  separate_all_tags() |>
  as_tibble()

# num <- 1
# cat("Extracting Marriage Data\n")
#temp <- spouses |>
#  mutate(record = fix_marraiges_ged_v(record)) |>
  # separate again now that MARR data is separated
#  separate_all_tags()


