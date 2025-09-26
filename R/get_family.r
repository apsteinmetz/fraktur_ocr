library(stringr)
library(dplyr)
library(purrr)
library(tibble)
library(glue)

make_gedcom <- function(lines) {
  # assume input is already a character vector, one record line per element
  lines <- str_trim(lines)

  # record number
  rec_id <- str_match(lines[1], "<(\\d+)>")[,2]

  # husband line
  husband_line <- lines[2]
  husband_name <- husband_line

  # birth info
  birth_line <- lines[3]
  husb_birth <- str_match(birth_line, "\\*um?\\s?(\\d{4})")[,2]

  # marriage line
  marriage_line <- lines[4]
  marriage_date <- str_match(marriage_line, "(\\d{2}\\.\\d{2}\\.\\d{4})")[,2]
  marriage_place <- str_trim(str_remove(marriage_line, paste0("oo ", marriage_date)))

  # wife line(s)
  wife_line <- lines[5]
  wife_name <- str_match(wife_line, "([A-ZÄÖÜß][a-zäöüß]+\\s+[A-ZÄÖÜß][a-zäöüß]+)")[,2]
  wife_birth <- str_match(wife_line, "\\*um?\\s?(\\d{4})")[,2]
  wife_death <- str_match(wife_line, "†\\s?(\\d{2}\\.\\d{2}\\.\\d{4})")[,2]

  # children (lines starting with number + ".")
  child_lines <- lines[str_detect(lines, "^\\d+\\.\\s")]

  if (length(child_lines) > 0) {
    children <- map_df(child_lines, function(cl) {
      cid <- str_match(cl, "^(\\d+)\\.")[,2]
      nm <- str_match(cl, "(?:\\d+\\.\\s)([A-ZÄÖÜß][a-zäöüß]+\\s+[A-ZÄÖÜß][a-zäöüß]+)")[,2]
      bdate <- str_match(cl, "\\*\\s?(\\d{2}\\.\\d{2}\\.\\d{4})")[,2]
      tibble(cid, name = nm, bdate)
    })

    # child blocks
    child_blocks <- pmap(children, function(cid, name, bdate) {
      c(
        glue("0 @I{rec_id}C{cid}@ INDI"),
        glue("1 NAME {name}"),
        if (!is.na(bdate)) glue("1 BIRT"),
        if (!is.na(bdate)) glue("2 DATE {bdate}"),
        glue("1 FAMC @F{rec_id}@")  # link back to family
      ) %>% discard(is.na)
    })

    family_children <- map(children$cid, ~ glue("1 CHIL @I{rec_id}C{.x}@"))
  } else {
    children <- tibble()
    child_blocks <- list()
    family_children <- character()
  }

  # GEDCOM construction
  ged <- c(
    "0 HEAD",
    "1 SOUR make_gedcom_R",
    "1 GEDC",
    "2 VERS 5.5.1",
    "1 CHAR UTF-8",

    # husband
    glue("0 @I{rec_id}H@ INDI"),
    glue("1 NAME {husband_name}"),
    if (!is.na(husb_birth)) c("1 BIRT", glue("2 DATE ABT {husb_birth}")),

    # wife
    glue("0 @I{rec_id}W@ INDI"),
    glue("1 NAME {wife_name}"),
    if (!is.na(wife_birth)) c("1 BIRT", glue("2 DATE ABT {wife_birth}")),
    if (!is.na(wife_death)) c("1 DEAT", glue("2 DATE {wife_death}")),

    # family block
    glue("0 @F{rec_id}@ FAM"),
    glue("1 HUSB @I{rec_id}H@"),
    glue("1 WIFE @I{rec_id}W@"),
    if (!is.na(marriage_date)) c("1 MARR", glue("2 DATE {marriage_date}"),
                                 glue("2 PLAC {marriage_place}")),
    family_children,

    # children INDI blocks
    unlist(child_blocks),

    "0 TRLR"
  ) %>% unlist() %>% discard(is.na)

  return(ged)
}
