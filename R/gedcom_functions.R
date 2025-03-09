# gedcom utility functions

extract_name <- function(
      text,
      type = c("raw", "formatted", "surname")) {
   # Split the text into words
   words <- str_split(text, "\\s+")[[1]] |>
      # Remove empty elements
      keep(~ .x != "")
   # Check if there is a third word and if it's a lowercase alphabetic string
   if (length(words) >= 3 && grepl("^[A-Z][äüa-z]+$", words[3])) {
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
