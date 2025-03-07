# extract and parse spouse records
# load records

fix_names <- function(text){
  if (str_detect(text, "NAME")) {
    name <- str_extract(text, "[A-Z]+ [A-Z][ a-z]+")
    new_text <- str_squish(paste("NAME ", name))
  } else {
    new_text <- text
  }
  return(new_text)
}

load("data/records.RData")
spouses <- records |>
  filter(!is.na(spouse0)) |>
  mutate(record = spouse0) |>
  select(ID,record,spouse0) |>
  separate_all_tags()

num <- 3
spouses |>
  filter(str_detect(record, "NAME")) |>
  pull(record) |>
#  pluck(num) |>
  map(fix_names) |>
  unlist()

