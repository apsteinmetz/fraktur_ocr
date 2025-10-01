# gedcom utility functions
date_regex <- paste0("(\\d{2}\\.\\d{2}\\.\\d{4})|(ABT \\d{4})")
date_regex_2 <- paste0("(\\d{4}-\\d{2}-\\d{2})|(ABT \\d{4})")
name_regex <- "([ßÖÜÄA-Z\\.]+ [ßÖÜÄA-Z][öäüa-z]+( [ÖÜÄA-Z][öäüa-z]+)?)"
# name_regex <- "([ßÖÜÄA-Z\\.]+ [ßÖÜÄA-Z][öäüa-z]+( [ÖÜÄA-Z][öäüa-z]+)?)"
# name_regex <- "([ÖÜÄA-Z]+ [ÖÜÄA-Z][öäüa-z]+)"

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

tag_text <- function(text_vec) {
  text_vec <- gsub("OH |OH\\n", "Ohio ", text_vec)
  text_vec <- gsub("INDI\n", "", text_vec)
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
  text_vec <- gsub(" NN.", " Unknown ", text_vec)
  text_vec <- gsub("o‐o", " MARR NOTE Divorced", text_vec)
  text_vec <- gsub("~", " BAPM ", text_vec)
  text_vec <- gsub("# ", "NOTE ", text_vec)
  text_vec <- gsub("[<>] (\\d{1,4}\\.\\d{1,2})", " XREF \\1 ", text_vec)
  text_vec <- gsub("[<>] (\\d{1,4})", " XREF \\1 ", text_vec)

  return(text_vec)
}

extract_name_new <- function(text, type = c("raw", "formatted", "surname")) {
   # remove break tags from text
   text <- str_remove_all(text, paste(break_tags, collapse = "|"))
   name_vec <- text |> str_extract(name_regex)
  if (is.na(name_vec)) {
    return(NA)
  } else {
    words <- str_split(name_vec, "\\s+")[[1]]
    name <- switch(
      type,
      raw = name_vec,
      formatted = paste0(
        paste0(words[2:length(words)], collapse = " "),
        " /",
        words[1],
        "/"
      ),
      surname = words[1],
      stop("Invalid type specified")
    )
    return(trimws(name))
  }
}

extract_name_old <- function(
  text,
  type = c("raw", "formatted", "surname")
) {
  # remove break tags from text
  text <- str_remove_all(text, paste(break_tags, collapse = "|"))
  # Split the text into words
  words <- str_split(text, "\\s+")[[1]] |>
    # Remove empty elements
    keep(~ .x != "")
  # Check if there is a third word and if it's a lowercase alphabetic string
  if (length(words) >= 3 && grepl(paste0("^", name_regex, "$"), words[3])) {
    # Append the third word if it qualifies
    name_vec <- c(words[1], c(words[2], words[3]))
  } else {
    name_vec <- c(words[1], words[2])
  }
  name <- switch(
    type,
    raw = paste(name_vec, collapse = " "),
    formatted = paste0(
      paste0(name_vec[2:length(name_vec)], collapse = " "),
      " /",
      name_vec[1],
      "/"
    ),
    surname = words[1],
    stop("Invalid type specified")
  )
  return(trimws(name))
}

library(stringr)
library(dplyr)

library(stringr)
library(dplyr)
library(purrr)

extract_name <- function(x) {
  stop_tokens <- c(
    "ABT", "um", "AGE", "siehe", "lebt", "als",
    "Evangelical", "Catholic", "ref\\.", "ev\\.",
    "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}",   # dates
    "Januar","Februar","März","April","Mai","Juni","Juli","August",
    "September","Oktober","November","Dezember",
    "Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
  )
  stop_regex <- str_c("\\b(", str_c(stop_tokens, collapse="|"), ")\\b")

  x %>%
    # remove record numbering like "<1>" or "1."
    str_remove_all("^<\\d+>\\s*") %>%
    str_remove_all("^\\d+\\.\\s*") %>%
    str_squish() %>%
    str_split("\\s+") %>%
    map_chr(function(tokens) {
      if (length(tokens) == 0) return(NA_character_)

      # detect marriage tags ("oo" or "o-o")
      if (tokens[1] %in% c("oo","o-o")) {
        # skip the tag and any immediate dates/places until first "real" name
        # find first token that looks like a name (starts with capital letter, not in stop list)
        start_idx <- which(str_detect(tokens, "^[A-ZÄÖÜ][a-zäöüßA-ZÄÖÜ-]*"))[1]
        tokens <- tokens[start_idx:length(tokens)]
      }

      # stop at first "non-name token"
      cut_idx <- which(str_detect(tokens, stop_regex))
      if (length(cut_idx) > 0) {
        tokens <- tokens[1:(min(cut_idx) - 1)]
      }
      str_squish(str_c(tokens, collapse = " "))
    })
}

# ---- Example ----
test_lines <- c(
  "<1> ABRAHAM Stefan",
  "RHUR Karolina  Evangelical    ABT 1811    11.04.1866  Neu Schowe",
  "2. Abraham Franziska   ABT 1846",
  "ALBERT Hans Jakob    11.09.1709 Mettweiler",
  "ALLGAIER Matthias   29.09.1905    29.05.1943 Cleveland, Ohio",
  "WILSON Thelma Florence   22.06.1907 Elyria    01.01.1994",
  "oo 23.04.1865 Neu Werbas GEYER Magdalena  Evangelical    ABT 1845",
  "o-o 15.08.1902 Neu Schowe HERTZ Anna Maria   ABT 1875"
)

extract_name(test_lines)


extract_name_v <- Vectorize(extract_name)

extract_tag <- function(text, tag) {
  # extract substring starting with "tag" and ending with any other tag from break_tags
  # or end of string

  startregex <- paste0(tag, ".*?\\n")
  myregex <- paste0(tag, ".+\\n|", paste(break_tags, collapse = "|"))
  matches <- str_extract(text, myregex)
  return(matches)
}

extract_all_dates_posix <- function(text, tag) {
  # Regular expression to match the tag word followed by a date in the format dd.mm.yyyy
  myregex <- paste0(
    tag,
    "\\s+(\\d{2}\\.\\d{2}\\.\\d{4})|",
    tag,
    "\\s+(ABT \\d{4})"
  )

  matches <- str_extract_all(text, myregex)
  if (is.na(matches)) {
    return(NA)
  } # tag not found
  if (length(matches[[1]]) > 0) {
    dates <- str_replace(matches[[1]], paste0(tag, "\\s+"), "")
    # replace ABT with jan 1
    dates_a <- dates[grepl("ABT", dates)]
    if (length(dates_a > 0)) {
      dates_a <- dates_a |>
        paste0("-01-01") |>
        str_remove("ABT ") |>
        as.Date()
    } else {
      dates_a <- NA
    }
    dates_b <- dates[!grepl("ABT", dates)] |>
      as.Date(dates, format = "%d.%m.%Y")
    dates <- as.Date(c(dates_a, dates_b))
    # remove NA values
    dates <- dates[!is.na(dates)]
  } else {
    dates <- NA
  }
  return(dates)
}

fix_dates <- function(text) {
  # fix dates where the day is missing
  text <- text |>
    str_replace_all(" (\\.\\d{1,2}\\.\\d{4})", "1\\1") |>
    # arrrange dates to yyyy-mm-dd
    str_replace_all("(\\d{1,2})\\.(\\d{1,2})\\.(\\d{4})", "\\3-\\2-\\1") |>
    # weird dash character
    str_replace_all("‐", "-")
  return(text)
}

fix_dates_2 <- function(text) {
  text <- text |>
    # replace month number with month abbreviation
    # add leading zeros if needed
    str_replace("(\\d{4})-(\\d{1})-(\\d{2})", "\\1 0\\2 \\3") |>
    str_replace("(\\d{4})-(\\d{2})-(\\d{1})$", "\\1 \\2 0\\3") |>
    str_replace_all("(\\d{4})-(\\d{2})-(\\d{2})", "\\1 \\2 \\3") |>
    str_replace_all("(\\d{4}) (01) (\\d{2})", "\\3 JAN \\1") |>
    str_replace_all("(\\d{4}) (02) (\\d{2})", "\\3 FEB \\1") |>
    str_replace_all("(\\d{4}) (03) (\\d{2})", "\\3 MAR \\1") |>
    str_replace_all("(\\d{4}) (04) (\\d{2})", "\\3 APR \\1") |>
    str_replace_all("(\\d{4}) (05) (\\d{2})", "\\3 MAY \\1") |>
    str_replace_all("(\\d{4}) (06) (\\d{2})", "\\3 JUN \\1") |>
    str_replace_all("(\\d{4}) (07) (\\d{2})", "\\3 JUL \\1") |>
    str_replace_all("(\\d{4}) (08) (\\d{2})", "\\3 AUG \\1") |>
    str_replace_all("(\\d{4}) (09) (\\d{2})", "\\3 SEP \\1") |>
    str_replace_all("(\\d{4}) (10) (\\d{2})", "\\3 OCT \\1") |>
    str_replace_all("(\\d{4}) (11) (\\d{2})", "\\3 NOV \\1") |>
    str_replace_all("(\\d{4}) (12) (\\d{2})", "\\3 DEC \\1") |>
    identity()
  return(text)
}

# elements following BIRT DEAT BURI MARR BAPM are dates that might be
# either a date, "ABT" followed by a year, "BET" followed by two years separated by a dash.
# the date group might be followed by a place name
# the place name might be followed by a note

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
      "\n"
    )
  } else {
    missing_place <- TRUE
  }

  if (!missing_date & !missing_place)
    ged <- paste0("2 DATE ", fix_dates_2(date), "\n", place)
  if (!missing_date & missing_place)
    ged <- paste0("2 DATE ", fix_dates_2(date), "\n")
  if (missing_date & !missing_place) ged <- place

  return(ged)
}

extract_date_ged_v <- Vectorize(extract_date_ged)

fix_dates_v <- Vectorize(fix_dates)

extract_date <- function(text, tag, type = c("raw", "posix", "gedcom")) {
  # Regular expression to match the tag word followed by a date in the format dd.mm.yyyy
  myregex <- paste0(
    tag,
    "\\s+(\\d{1,2}\\.\\d{1,2}\\.\\d{4})|",
    tag,
    "\\s+(\\.\\d{2}\\.\\d{4})|",
    tag,
    "\\s+(ABT \\d{4})"
  )
  text <- str_squish(text)

  matches <- str_extract_all(text, myregex)
  if (is.na(matches)) {
    return(NA)
  } # tag not found
  if (length(matches[[1]]) > 0) {
    match <- matches[[1]][1]
    extras <- str_trim(str_remove(text, match))
    if (grepl("^\\..+", match)) {
      # missing day, pretend it's first of month
      match <- paste0("1", match)
    }
    date <- str_replace(match, paste0(tag, "\\s+"), "")
    date <- switch(
      type,
      raw = date,
      posix = if (grepl("ABT", date)) {
        # if posix is selected and the date is approximate use jan 1
        as.Date(paste0(sub("ABT (\\d{4})", "\\1", date), "-", "01-01"))
      } else {
        as.Date(date, format = "%d.%m.%Y")
      },
      gedcom = if (str_length(extras) > 0) {
        paste0("1 ", tag, "\n 2 DATE ", date, "\n", " 2 PLAC ", extras, "\n")
      } else {
        paste0("1 ", tag, "\n 2 DATE ", date, "\n")
      },
    )
  } else {
    date <- NULL
  }
  return(date)
}

extract_date_v <- Vectorize(extract_date)

extract_xref <- function(text) {
  xref <- str_extract(text, "\\d+")
  return(paste0("1 FAMC ", "@F", xref, "@\n"))
}

make_ged <- function(text) {
  tag <- str_extract(text, break_tags) |>
    # remove NAs
    discard(is.na)
  # remove double line breaks
  text <- str_replace_all(text, "\n\n", "\n")
  ged <- switch(
    tag,
    "NAME" = paste0(
      "1 NAME ",
      extract_name(str_trim(str_remove(text, tag)), type = "formatted"),
      "\n"
    ),
    "BIRT" = paste0("1 BIRT\n", extract_date_ged(text)),
    "DEAT" = paste0("1 DEAT\n", extract_date_ged(text)),
    "BURI" = paste0("1 BURI\n", extract_date_ged(text)),
    "BAPM" = paste0("1 BAPM\n", extract_date_ged(text)),
    "MARR" = paste0("1 MARR\n", extract_date_ged(text)),
    "XREF" = extract_xref(text),
    "PLAC" = paste0("2 ", text, "\n"),
    "RELI" = paste0("2 ", text, "\n"),
    "WITN" = paste0("2 ", text, "\n"),
    "GODP" = paste0("2 ", text, "\n"),
    "Eltern:" = paste0("2 NOTE ", text, "\n"),
    "Wohnort" = paste0("2 PLAC ", text, "\n"),
    "letzter" = paste0("2 PLAC ", text, "\n"),
    "NOTE" = paste0("2 ", text, "\n")
  )
  if (is.null(ged)) {
    ged <- paste0("1 NOTE ", text, "\n")
  }
  return(ged)
}

make_individual_ged <- function(ID, type = "indi") {
  if (type == "indi") {
    gedcom <- paste0("0 @I", ID, "@ INDI\n")
    gedcom <- paste0(gedcom, "1 FAMS @F", ID, "@\n")
  } else {
    gedcom <- paste0("0 @F", ID, "@ FAM\n")
    gedcom <- paste0(gedcom, "1 HUSB @I", ID, "@\n")
    # is it husband? not always, then a mistake
  }
  return(gedcom)
}

make_ged_v <- Vectorize(make_ged)
make_individual_ged_v <- Vectorize(make_individual_ged)

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

get_person <- function(family_record) {
  persons <- list()

  if (length(family_record) == 0) return(persons)

  # Extract family ID from first line
  family_id <- str_extract(family_record[1], "(?<=<)\\d+(?=>)")
  if (is.na(family_id)) return(persons)

  # Helper function to extract name from a line
  extract_name <- function(line) {
    # Remove number prefixes and clean
    clean_line <- str_replace(line, "^\\d+\\.\\s*", "")
    clean_line <- str_replace_all(clean_line, "\\b(BIRT|DEAT|MARR|BURI|BAPM|RELI|PLAC|ABT|AGE|XREF)\\b.*$", "")
    clean_line <- str_trim(clean_line)

    # Handle "SURNAME Given" format
    name_match <- str_match(clean_line, "^([A-ZÄÖÜ]+)\\s+([A-Za-zäöüß]+(?:\\s+[A-Za-zäöüß]+)*)")
    if (!is.na(name_match[1])) {
      return(paste(name_match[3], name_match[2]))  # "Given SURNAME"
    }

    return(clean_line)
  }

  # Find person boundaries
  person_starts <- c()
  for (i in seq_along(family_record)) {
    line <- family_record[i]

    # Head of household
    if (str_detect(line, "^<\\d+>$")) {
      person_starts <- c(person_starts, i)
    }
    # Child
    else if (str_detect(line, "^\\d+\\.")) {
      person_starts <- c(person_starts, i)
    }
    # Spouse (name with RELI)
    else if (str_detect(line, "^[A-ZÄÖÜ]+\\s+[A-Za-zäöüß]+\\s+RELI")) {
      person_starts <- c(person_starts, i)
    }
  }

  # Add end position
  person_starts <- c(person_starts, length(family_record) + 1)

  # Create person counter
  person_counter <- 1

  # Process each person
  for (p in 1:(length(person_starts) - 1)) {
    start_pos <- person_starts[p]
    end_pos <- person_starts[p + 1] - 1

    person_lines <- family_record[start_pos:end_pos]
    first_line <- person_lines[1]

    # Determine person_id based on relationship
    if (str_detect(first_line, "^<\\d+>$")) {
      # Head of household gets family_id as person_id
      person_id <- family_id
      name_line <- if (length(person_lines) > 1) person_lines[2] else ""
    } else {
      # All others get sequential person_ids
      person_id <- as.character(person_counter)
      person_counter <- person_counter + 1
      name_line <- first_line
    }

    # Extract name
    name <- extract_name(name_line)

    # Combine all remaining text as notes
    notes <- paste(person_lines, collapse = " ")

    # Create person record
    person <- list(
      name = name,
      person_id = person_id,
      family_id = family_id,
      notes = notes
    )

    persons[[length(persons) + 1]] <- person
  }

  return(persons)
}

