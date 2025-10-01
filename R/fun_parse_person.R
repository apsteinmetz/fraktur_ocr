library(stringr)
library(purrr)
library(dplyr)
library(tibble)

# ------------ helpers ------------
# Helper function to rearrange names from surname-first to given-first
rearrange_name <- function(name) {
  if (is.na(name) || name == "") return(name)
  
  # Split the name into parts
  name_parts <- str_split(name, "\\s+")[[1]]
  
  if (length(name_parts) < 2) return(name)
  
  # Remove commas from parts
  name_parts <- str_remove_all(name_parts, ",")
  
  # First part is typically the surname, rest are given names
  surname <- name_parts[1]
  given_names <- name_parts[-1]
  
  # Rearrange to given names first, then surname
  paste(c(given_names, surname), collapse = " ")
}

.cap_word <- function(tok) str_detect(tok, "^[A-ZÄÖÜ][A-Za-zÄÖÜäöüß\\-]+,?$")

.place_prefixes <- c("Neu","Alt","Groß","Gross","Klein","Ober","Unter","St\\.","Sankt","Bad")

.is_prefix_place_pair <- function(t1, t2) {
  t1 %in% .place_prefixes && !is.na(t2) && .cap_word(t2)
}

# extend as needed with your gazetteer
.is_placeish <- function(tok) {
  str_detect(tok, "[,/]$|,|/") || tok %in% c(
    "Ohio","Cleveland","Cuyahoga","Elyria","USA","Syrmien","Titel","Nadalj",
    "Mettweiler","Linden","Baumholder","Werbas","Schowe"
  )
}

.is_probable_name_start <- function(tokens, j) {
  j + 1L <= length(tokens) &&
    .cap_word(tokens[j]) && .cap_word(tokens[j+1]) &&
    !.is_prefix_place_pair(tokens[j], tokens[j+1])
}

# ------------ NAME extraction (fixed for MARR … PLAC …) ------------
# returns list(name = <chr>, idx = <integer indices consumed for name>)
.extract_name_tokens_mark <- function(tokens) {
  if (!length(tokens)) return(list(name = NA_character_, idx = integer()))
  stop_regex <- "^(ABT|BEF|BET|AGE|Evangelical|Catholic|Reformed|REL[Ii]|\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}|\\d{1,2}\\.\\d{1,2}\\.\\d{2}|\\d{4}|MARR|BIRT|BAPM|DEAT|BURI|PLAC|NOTE|WITN|GODP|XREF)$"
  is_date <- function(tok) str_detect(tok, "^\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}$")

  i <- 1L

  if (tokens[1] == "MARR") {
    i <- 2L
    while (i <= length(tokens) && is_date(tokens[i])) i <- i + 1L

    if (i <= length(tokens) && tokens[i] == "PLAC") {
      i <- i + 1L
      while (i <= length(tokens)) {
        if (str_detect(tokens[i], stop_regex)) break
        if (.is_prefix_place_pair(tokens[i], tokens[i+1])) { i <- i + 2L; next }
        if (.is_placeish(tokens[i])) { i <- i + 1L; next }
        if (.cap_word(tokens[i])) break
        i <- i + 1L
      }
    } else {
      while (i <= length(tokens)) {
        if (.is_prefix_place_pair(tokens[i], tokens[i+1])) { i <- i + 2L; next }
        if (.is_placeish(tokens[i])) { i <- i + 1L; next }
        break
      }
    }
  }

  j <- i
  name_idx <- integer()
  while (j <= length(tokens)) {
    tk <- tokens[j]
    if (str_detect(tk, stop_regex)) break
    name_idx <- c(name_idx, j)
    if (length(name_idx) >= 2 &&
        (j == length(tokens) || str_detect(tokens[j+1], stop_regex))) break
    j <- j + 1L
  }

  out <- if (length(name_idx)) str_squish(paste(tokens[name_idx], collapse = " ")) else NA_character_
  list(name = out, idx = name_idx)
}

# ------------ EVENT parsing (XREF suppressed; RELI value→text; WITN/GODP→NOTE) ------------
# returns list(events = tibble(event,date,place,text), used = logical vector)
.parse_events_mark <- function(tokens, used_init = NULL) {
  n <- length(tokens)
  used <- if (is.null(used_init)) rep(FALSE, n) else used_init

  # events we actually emit (WITN/GODP are NOT events now)
  tags_event <- c("BIRT","BAPM","MARR","DEAT","BURI","RELI","NOTE")
  is_tag <- function(tok) tok %in% tags_event
  # boundary tags that terminate segments (include WITN, GODP as boundaries)
  is_block_tag <- function(tok) tok %in% c(tags_event, "WITN", "GODP", "PLAC", "AGE", "XREF", "ABT","BEF","BET")

  # dates
  is_date_dot <- function(tok) str_detect(tok, "^\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}$")
  months <- c("Januar","Februar","März","April","Mai","Juni","Juli","August",
              "September","Oktober","November","Dezember",
              "Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  is_month <- function(tok) tok %in% months
  is_year4 <- function(tok) str_detect(tok, "^\\d{4}$")
  is_qual <- function(tok) tok %in% c("ABT","BEF","BET")

  events <- list()
  i <- 1L

  while (i <= n) {
    tk <- tokens[i]

    # ---- XREF: consume silently (do not emit; do not leak to NOTE) ----
    if (identical(tk, "XREF")) {
      used[i] <- TRUE
      if (i + 1L <= n) used[i + 1L] <- TRUE
      i <- i + 2L
      next
    }

    # ---- WITN/GODP: capture text and emit NOTE "WITN <text>" / "GODP <text>" ----
    if (identical(tk, "WITN") || identical(tk, "GODP")) {
      tag <- tk
      used[i] <- TRUE
      j <- i + 1L
      parts <- character()
      while (j <= n && !is_block_tag(tokens[j])) {
        parts <- c(parts, tokens[j]); used[j] <- TRUE; j <- j + 1L
      }
      txt <- str_squish(paste(c(tag, parts), collapse = " "))
      events[[length(events) + 1L]] <- list(
        event = "NOTE",
        date  = NA_character_,
        place = NA_character_,
        text  = if (!identical(txt, "")) txt else NA_character_
      )
      i <- j
      next
    }

    # ---- Explicit NOTE block ----
    if (identical(tk, "NOTE")) {
      used[i] <- TRUE
      j <- i + 1L
      note_parts <- character()
      while (j <= n && !is_block_tag(tokens[j])) {
        note_parts <- c(note_parts, tokens[j]); used[j] <- TRUE; j <- j + 1L
      }
      events[[length(events) + 1L]] <- list(
        event = "NOTE",
        date  = NA_character_,
        place = NA_character_,
        text  = if (length(note_parts)) str_squish(paste(note_parts, collapse = " ")) else NA_character_
      )
      i <- j
      next
    }

    if (!is_tag(tk)) { i <- i + 1L; next }

    ev <- tk
    used[i] <- TRUE
    j <- i + 1L
    date_parts <- character()
    place_parts <- character()
    ev_text <- NA_character_

    # ---- RELI value → text ----
    if (ev == "RELI") {
      reli_parts <- character()
      while (j <= n && !is_block_tag(tokens[j])) {
        reli_parts <- c(reli_parts, tokens[j]); used[j] <- TRUE; j <- j + 1L
      }
      ev_text <- if (length(reli_parts)) str_squish(paste(reli_parts, collapse = " ")) else NA_character_

      events[[length(events) + 1L]] <- list(
        event = ev, date = NA_character_, place = NA_character_, text = ev_text
      )
      i <- j
      next
    }

    # ---- DATE (for non-RELI events) ----
    if (j <= n && is_qual(tokens[j])) {
      used[j] <- TRUE
      q <- tokens[j]; j <- j + 1L
      if (j <= n && is_date_dot(tokens[j])) {
        date_parts <- c(q, tokens[j]); used[j] <- TRUE; j <- j + 1L
      } else if (j + 1L <= n && is_month(tokens[j]) && is_year4(tokens[j+1])) {
        date_parts <- c(q, tokens[j], tokens[j+1]); used[j:(j+1L)] <- TRUE; j <- j + 2L
      } else if (j <= n && is_year4(tokens[j])) {
        date_parts <- c(q, tokens[j]); used[j] <- TRUE; j <- j + 1L
      }
    } else {
      if (j <= n && is_date_dot(tokens[j])) {
        date_parts <- c(tokens[j]); used[j] <- TRUE; j <- j + 1L
      } else if (j + 1L <= n && is_month(tokens[j]) && is_year4(tokens[j+1])) {
        date_parts <- c(tokens[j], tokens[j+1]); used[j:(j+1L)] <- TRUE; j <- j + 2L
      }
    }

    # ---- PLACE ----
    if (j <= n && tokens[j] == "PLAC") {
      used[j] <- TRUE; j <- j + 1L
      while (j <= n) {
        if (is_block_tag(tokens[j])) break
        if (ev == "MARR" && .is_probable_name_start(tokens, j)) break
        took <- FALSE
        if (j < n && .is_prefix_place_pair(tokens[j], tokens[j+1])) {
          place_parts <- c(place_parts, tokens[j], tokens[j+1]); used[j:(j+1L)] <- TRUE; j <- j + 2L; took <- TRUE
        } else if (.is_placeish(tokens[j]) || .cap_word(tokens[j])) {
          place_parts <- c(place_parts, tokens[j]); used[j] <- TRUE; j <- j + 1L; took <- TRUE
        }
        if (!took) break
      }
    } else {
      while (j <= n) {
        if (is_block_tag(tokens[j])) break
        if (ev == "MARR" && .is_probable_name_start(tokens, j)) break
        took <- FALSE
        if (j < n && .is_prefix_place_pair(tokens[j], tokens[j+1])) {
          place_parts <- c(place_parts, tokens[j], tokens[j+1]); used[j:(j+1L)] <- TRUE; j <- j + 2L; took <- TRUE
        } else if (.is_placeish(tokens[j])) {
          place_parts <- c(place_parts, tokens[j]); used[j] <- TRUE; j <- j + 1L; took <- TRUE
        }
        if (!took) break
      }
    }

    events[[length(events) + 1L]] <- list(
      event = ev,
      date  = if (length(date_parts)) str_squish(paste(date_parts, collapse = " ")) else NA_character_,
      place = if (length(place_parts)) str_squish(gsub(",$", "", paste(place_parts, collapse = " "))) else NA_character_,
      text  = ev_text
    )

    i <- j
  }

  ev_tbl <- if (!length(events)) {
    tibble(event = character(), date = character(), place = character(), text = character())
  } else bind_rows(events)

  list(events = ev_tbl, used = used)
}

# ------------ MAIN ------------
# Returns a list (one element per input line) with:
#   name   : character
#   events : tibble(event, date, place, text)
parse_person_lines <- function(text_lines) {
  tagged <- tag_text(text_lines)

  cleaned <- tagged %>%
    str_replace_all("^\\s*<\\d+>\\s*", "") %>%
    str_replace_all("^\\s*\\d+\\.\\s*", "") %>%
    str_squish()

  tokenized <- str_split(cleaned, "\\s+")

  map(tokenized, function(tokens) {
    # Check for XREF pattern and extract indiv_id and fam_id
    full_text <- paste(tokens, collapse = " ")
    xref_match <- str_extract(full_text, "XREF ([0-9]+\\.?[0-9]*)")
    extracted_indiv_id <- if (!is.na(xref_match)) {
      str_remove(xref_match, "XREF ")
    } else {
      NA_character_
    }
    extracted_fam_id <- if (!is.na(extracted_indiv_id)) {
      str_extract(extracted_indiv_id, "^[0-9]+")
    } else {
      NA_character_
    }
    
    # Extract name, handling XREF cases differently
    if (!is.na(extracted_indiv_id)) {
      # For XREF records, name comes before the 'XREF' token
      xref_pos <- which(tokens == "XREF")
      if (length(xref_pos) > 0) {
        name_tokens <- tokens[1:(xref_pos[1] - 1)]
        
        # Extract capitalized words from the name part
        cap_tokens <- name_tokens[str_detect(name_tokens, "^[A-ZÄÖÜ][A-Za-zÄÖÜäöüß\\-]+")]
        
        # Handle witnesses (max 2) + spouse name
        if (length(cap_tokens) > 2) {
          # More than 2 capitalized words - likely witnesses + spouse
          # Last surname is the spouse surname, second-to-last might be given name
          spouse_surname <- cap_tokens[length(cap_tokens)]
          
          # Look for a capitalized given name that comes right before the surname
          surname_pos <- which(name_tokens == spouse_surname)
          if (length(surname_pos) > 0 && surname_pos[1] > 1) {
            potential_given <- name_tokens[surname_pos[1] - 1]
            if (str_detect(potential_given, "^[A-ZÄÖÜ][A-Za-zÄÖÜäöüß\\-]+")) {
              spouse_name <- paste(potential_given, spouse_surname)
            } else {
              spouse_name <- spouse_surname
            }
          } else {
            spouse_name <- spouse_surname
          }
          name_res <- list(name = spouse_name, idx = integer())
        } else {
          # 2 or fewer capitalized words, use normal extraction
          name_res <- .extract_name_tokens_mark(tokens)
        }
      } else {
        name_res <- .extract_name_tokens_mark(tokens)
      }
    } else {
      name_res <- .extract_name_tokens_mark(tokens)
    }
    
    used <- rep(FALSE, length(tokens))
    if (length(name_res$idx)) used[name_res$idx] <- TRUE

    ev_res <- .parse_events_mark(tokens, used_init = used)
    events <- ev_res$events
    used   <- ev_res$used

    # leftover → NOTE (exclude XREF pattern)
    leftover_tokens <- tokens[!used]
    leftover_tokens <- leftover_tokens[!str_detect(leftover_tokens, "^XREF$|^[0-9]+\\.[0-9]*$")]
    leftover <- str_squish(paste(leftover_tokens, collapse = " "))
    
    if (!identical(leftover, "") && leftover != "XREF") {
      events <- bind_rows(
        events,
        tibble(event = "NOTE", date = NA_character_, place = NA_character_, text = leftover)
      )
    }

    result <- list(
      name   = name_res$name,
      events = events
    )
    
    # Add extracted XREF information if present
    if (!is.na(extracted_indiv_id)) {
      result$xref_indiv_id <- extracted_indiv_id
      result$xref_fam_id <- extracted_fam_id
    }
    
    result
  })
}

