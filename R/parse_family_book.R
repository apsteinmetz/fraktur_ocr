# parse family records
library(tidyverse)

source("r/gedcom_functions.r")

PROGRESS <- TRUE
# extraction functions ---------------------------------------------------------

file_path <- "data/persons.txt"

date_regex <- paste0("^(\\d{2}\\.\\d{2}\\.\\d{4})|\\s+(ABT \\d{4})")

element_tags <- c(
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

break_tags <- c(element_tags)

# insert new item in character vector at line break tags
# insert_line_breaks <- function(text_vec) {
#  for (tag in line_break_tags) {
#    text_vec <- gsub(paste0(tag, " "), paste0("\n", tag, " "), text_vec)
#  }
#  return(text_vec)
# }

#' Function to read the file and extract records
#'
#' @param file_path Path to the text file.
#' @return A list of character vectors, each vector representing a record.
read_records <- function(file_path) {
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
  # replace '<num>' with '@I<num>@'
  lines <- gsub("<(\\d+)>", "@I\\1@ INDI", lines)
  # remove empty lines
  lines <- lines[!grepl("^\\s*$", lines)]

  # Identify record start lines (lines starting with "<")
  record_start_indices <- grep("^@", lines)

  # Handle edge case of no records being found
  if (length(record_start_indices) == 0) {
    warning("No records found in the file.")
    return(list())
  }

  # Extract records.  The person's name is on the line immediately after the record start.
  records <- list()
  for (i in seq_along(record_start_indices)) {
    start_index <- record_start_indices[i]
    end_index <- ifelse(
      i < length(record_start_indices),
      record_start_indices[i + 1] - 1,
      length(lines)
    )

    # Include the person's name line with the record
    record_lines <- lines[start_index:end_index]
    if (start_index + 1 <= length(lines)) {
      if ((start_index + 1) %in% record_start_indices) {
        #check if the next line is a new record before adding it.
        record_lines <- lines[start_index:end_index]
      } else {
        record_lines <- lines[start_index:(end_index)]
      }
    }
    records[[i]] <- record_lines # List of character vectors, each vector being a record
  }

  return(records)
}

tag_text <- function(text_vec) {
  # Replace special characters
  # text_vec <- gsub("@I(\\d+)@","@I\\1@ INDI\nNOTE ID=\\1",text_vec)
  # text_vec <- gsub("<(\\d+)>","NOTE ID\\1",text_vec)
  text_vec <- gsub("INDI\n", "", text_vec)
  text_vec <- gsub("o‐o", " Unknown_spouse ", text_vec)
  text_vec <- gsub("\\*", "BIRT ", text_vec)
  text_vec <- gsub("um ", " ABT ", text_vec)
  text_vec <- gsub("vor ", " BEF ", text_vec)
  text_vec <- gsub("zw.", " BET ", text_vec)
  text_vec <- gsub("oo", "MARR ", text_vec)
  text_vec <- gsub(" TZ:", " WITN ", text_vec)
  text_vec <- gsub("\\(†mit (.*?)\\)", "AGE \\1 ", text_vec)
  # text_vec <- gsub("\\(†mit(.+)\\)","AGE \\1 ",text_vec)
  text_vec <- gsub("([0-9]{1,2})J", "\\1 years ", text_vec)
  text_vec <- gsub("([0-9]{1,2})M", "\\1 months ", text_vec)
  text_vec <- gsub("([0-9]{1,2})T", "\\1 days ", text_vec)
  text_vec <- gsub("†", " DEAT ", text_vec)
  text_vec <- gsub("b\\. ", " BURI ", text_vec)
  text_vec <- gsub(" AS", " PLAC Alt Schowe ", text_vec)
  text_vec <- gsub(" NS", " PLAC Neu Schowe ", text_vec)
  text_vec <- gsub("( Lager [a-zA-Z]+)", " PLAC \\1", text_vec)
  text_vec <- gsub(" ev\\.", " RELI Evangelical ", text_vec)
  text_vec <- gsub(" ref\\.", " RELI Reformed ", text_vec)
  text_vec <- gsub(" kath\\.", " RELI Catholic ", text_vec)
  text_vec <- gsub(" TZ:", " WITN ", text_vec)
  text_vec <- gsub(" TP:", " GODP ", text_vec)
  text_vec <- gsub("~", " BAPM ", text_vec)
  text_vec <- gsub("# ", "NOTE ", text_vec)
  text_vec <- gsub("[<>] (\\d{1,4}\\.\\d{1,2})", " XREF \\1 ", text_vec)
  text_vec <- gsub("[<>] (\\d{1,4})", " XREF \\1 ", text_vec)
  return(text_vec)
}



make_name_col <- function(records) {
  # Extract the person's name from the first line of the record
  name_records <- records |>
    mutate(
      surname = extract_name_v(record, type = "surname"),
      .before = "record"
    ) |>
    mutate(
      name = extract_name_v(record, type = "formatted"),
      .before = "record"
    )
  return(name_records)
}
make_child_col <- function(records) {
  # Extract the person's name from the first line of the record
  child_records <- records |>
    separate_wider_delim(
      cols = record,
      delim = regex("\\d+\\. "),
      names = c(
        "record",
        # create columns for up to 20 children
        "fam1",
        "fam2",
        "fam3",
        "fam4",
        "fam5",
        "fam6",
        "fam7",
        "fam8",
        "fam9",
        "fam10",
        "fam11",
        "fam12",
        "fam13",
        "fam14",
        "fam15",
        "fam16",
        "fam17",
        "fam18",
        "fam19",
        "fam20"
      ),
      too_many = "merge",
      too_few = "align_start"
    ) %>%
    pivot_longer(
      cols = starts_with("fam"),
      names_to = "relationship",
      values_to = "person"
    ) |>
    # reduce multiple NAs to just one
    nest(children = c(relationship, person)) %>%
    #replace multiple nas with single na
    mutate(children = map(children, \(x) na.omit(x)))

  return(child_records)
}
make_spouse_col <- function(records) {
  # Extract spouse sub-record from MARR tag
  spouse_records <- records |>
    separate_wider_delim(
      cols = record,
      delim = regex("MARR"),
      names = c("record", "spouse0"),
      too_many = "merge",
      too_few = "align_start"
    )
  return(spouse_records)
}
make_dates_col <- function(records) {
  records$birth <- as.Date(rep(NA, nrow(records)))
  records$death <- as.Date(rep(NA, nrow(records)))
  for (i in 1:nrow(records)) {
    records$birth[i] <- extract_date(records$record[i], "BIRT", type = "posix")
    records$death[i] <- extract_date(records$record[i], "DEAT", type = "posix")
  }
  records <- records |> select(ID, surname, name, birth, death, everything())
  return(records)
}
make_header_cols <- function(records) {
  # Extract the person's name from the first line of the record
  records <- records |>
    mutate(.before = ID, ged_indi = make_individual_ged_v(ID, "indi")) |>
    mutate(.before = ID, ged_fam = make_individual_ged_v(ID, "fam"))
  return(records)
}

# cleaning up tag positions that throw GEDCOM format warnings
fix_reli_pos <- function(records) {
  # if a record starts with RELI and the record above it starts with NAME, swap
  # the row containing RELI with the row below it, which should be an event
  for (i in 2:nrow(records)) {
    if (
      grepl("^RELI", records$record[i]) && grepl("^NAME", records$record[i - 1])
    ) {
      temp <- records$record[i]
      records$record[i] <- records$record[i + 1]
      records$record[i + 1] <- temp
    }
  }
  return(records)
}
fix_plac_pos <- function(records,tag="PLAC") {
  # squish rows with double tags
  for (i in 2:nrow(records)) {
    if (
      grepl("^2 PLAC", records$tag_ged[i]) &&
        grepl("^2 PLAC", records$tag_ged[i + 1])
    ) {
      records$record[i] <- paste(
        records$record[i],
        str_remove(records$record[i + 1], paste0("^", tag))
      )
      records$record[i + 1] <- NA

      records$tag_ged[i] <- paste(
        records$tag_ged[i],
        str_remove(records$tag_ged[i + 1], paste0("^", tag))
      )
      records$tag_ged[i + 1] <- NA
      print(paste("got one at row", i))
    }
  }
  records <- records |> filter(!is.na(record))
  return(records)
}

# records2 <- fix_plac_pos(records2)

make_tag_ged <- function(records) {
  if (PROGRESS) cat('making GED tags\n ')
  # records <- fix_reli_pos(records)
  records <- records |>
    mutate(.before = ID, tag_ged = make_ged_v(record)) |>
  # expand tag_ged with linefeeds into separate rows
  # remove rows where tag_ged is empty
    separate_rows(record, sep = "\n") |>
    filter(str_length(tag_ged) > 6) |>
    identity()

    return(records)
}

final_cleanup <- function(records){
  records <- records |>
    mutate(tag_ged = str_replace_all(tag_ged, "\n\n", "\n"))
  return(records)

  }

# loader section ---------------------------------------------------------------
read_raw_text <- function() {
  file_path <- "data/persons.txt" # Set file path
  all_recs <- read_lines(file_path)
  raw_data <- read_records(file_path)
  save(raw_data, file = "data/raw_data.RData")
}
load_raw_records <- function() {
  load("data/raw_data.RData")
  records_base <- map(raw_data, str_flatten, collapse = "\n") %>%
    enframe(name = NULL, value = "record") |>
    unnest(record) |>
    separate(record, c("ID", "record"), sep = " ", extra = "merge") |>
    mutate(ID = as.integer(str_remove_all(ID, "\\D"))) |>
    mutate(record = tag_text(record)) |>
    mutate(record = fix_dates_v(record)) |>
    # remove missing persons
    filter(!str_detect(record, "nach Korrektur unbesetzt"))
  save(records_base, file = "data/records_base.RData")
}
# processing section ---------------------------------------------------------------
make_records <- function(records_base) {
  records <- records_base |>
    # peel off layers of info
    make_child_col() |>
    make_spouse_col() |>
    make_name_col() |>
    make_header_cols()
    # now just the main person is left in the record column
    save(records, file = "data/records.RData")

    records_tagged <- records |>
    separate_all_tags() |>
    make_tag_ged() |>
    # final_cleanup() |>
    as_tibble()
  save(records_tagged, file = "data/records_tagged.RData")
  return(records_tagged)
}

# save records to gedcom file --------------------------------------------------
# function to group by ID and collapse all records into a single record
collapse_records <- function(records) {
  records <- records |>
    group_by(ID) |>
    summarize(tag_ged = str_c(tag_ged, collapse = "\n"), .groups = "drop") |>
    # remove double line breaks
    mutate(tag_ged = str_replace_all(tag_ged, "\\n\\n", "\n")) |>
    as_tibble()
  return(records)
}

save_ged <- function(records, outfile = "schowe") {
  header <- readChar("data/header.ged", nchars = 1e6)
  footer <- readChar("data/footer.ged", nchars = 1e6)

  # make one long gedcom string with line breaks as separators
  records_ged <- collapse_records(records) |>
    left_join(distinct(select(records, ID, ged_indi, ged_fam)), by = "ID") |>
    # eventually paste spouse and children ged
    mutate(gedcom = paste0(ged_indi, tag_ged, ged_fam)) |>
    select(ID, gedcom) |>
    # combine gedcom into a single character vector
    pull(gedcom) |>
    str_c(collapse = "")

  # remove elements that were not properly tagged
  records_ged <- strsplit(records_ged, "\n")[[1]] |>
    enframe(name = NULL, value = "record") |>
    # remove any row that does not begin with a number
    filter(str_detect(record, "^\\d+ ")) |>
    filter(!str_detect(record, "^\\d{4}")) |>
    filter(!str_detect(record, "^\\d+\\.$")) |>
    pull(record) |>
    str_c(collapse = "\n") |>
    # change unrecognized tags to NOTE
    str_replace_all("GODP", "NOTE GODP") |>
    str_replace_all("WITN", "NOTE WITN")

  records_ged <- paste0(header, records_ged, footer) |>
    # get rid of space padding
    str_replace_all("  ", " ")
  writeChar(records_ged, con = paste0("data/", outfile, ".ged"), eos = NULL)
}

# main body --------------------------------------------------------------------
# read_raw_text()
# load_raw_records()
load("data/records_base.RData")
records_tagged <- make_records(records_base)
# load("data/records.RData")
# records <- records |> fix_plac_pos()
save_ged(records_tagged)
steinmetz <- records_tagged |>
  filter(str_detect(surname, "STEINMETZ"))
save_ged(steinmetz, "steinmetz")
