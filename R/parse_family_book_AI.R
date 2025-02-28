# test genai api
library(tidyverse)
library(httr2)
library(jsonlite)

file_path <- "data/persons_sm.txt"
# Set up authentication (this is crucial and more complexsee below)
# Assume you've obtained a valid API key or access token

api_key <- Sys.getenv("GOOGLE_AI")
model_name <- "gemini-1.5-pro" # or another available model
# model_name <- "gemini-2.0-flash-001"
model_url <- paste0(
  "https://generativelanguage.googleapis.com/v1beta/models/",
  model_name,
  ":generateContent?key=",
  api_key
)

context_prompt <- paste(
  "You are a genealogy expert. Analyze the following text and extract genealogical information.",
  "Identify individualstheir relationshipsbirth dates (or approximate years)death dates,",
  "marriage datesand locations. If information is missing infer it if possible. The text file",
  "is in German.",

  "Format the extracted information into a GEDCOM 5.5.1 format.
                        If no person ID number is detected Create the new INDI tag starting with id number 10000",
  "Recognize that many standard GEDCOM tags are already in the text",
  "Create a 'lineage-linked' GEDCOM output.",
  "Create a FAM record for each marriage",
  "Use the FAMC tag to link children individuals to FAM record.",
  "Use the FAMS tag to spouse or parent individuals to FAM record.",
  "The MARR,CHIL,HUSB and WIFE tags can only be part of a FAM record.",
  "Omit the GEDCOM header and footer.",
  "Omit the explanation text and only return the GEDCOM data.",
  "The cross-references refer to other individual person IDs which my not be in the same file.",
  "Here is the first record.",
)

continue_prompt <- ("Here's some more text to convert to GEDCOM format: ")

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


to_JSON_body <- function(
  prompt = "You are a poet. Write a limerick about fishing."
) {
  toJSON(
    list(
      contents = list(list(parts = list(list(text = prompt))))
    ),
    auto_unbox = TRUE
  )
}

to_body <- function(
  prompt = "You are a poet. Write a limerick about fishing."
) {
  list(
    contents = list(list(parts = list(list(text = prompt))))
  )
}

clean_text <- function(text_vec) {
  # Replace special characters
  text_vec <- gsub("@I(\\d+)@", "@I\\1@ INDI\nNOTE ID=\\1", text_vec)
  # text_vec <- gsub("<(\\d+)>","NOTE ID\\1",text_vec)
  text_vec <- gsub("o‐o", " Unknown_spouse ", text_vec)
  text_vec <- gsub("\\*", " BIRT ", text_vec)
  text_vec <- gsub(" um ", " ABT ", text_vec)
  text_vec <- gsub("oo", " MARR ", text_vec)
  text_vec <- gsub(" TZ:", " WITN: ", text_vec)
  text_vec <- gsub("†mit", "aged ", text_vec)
  text_vec <- gsub("([0-9]{1,2})J", "\\1 years", text_vec)
  text_vec <- gsub("([0-9]{1,2})M", "\\1 months", text_vec)
  text_vec <- gsub("([0-9]{1,2})T", "\\1 days", text_vec)
  text_vec <- gsub("†", " DEAT ", text_vec)
  text_vec <- gsub("b. ", " BURI ", text_vec)
  text_vec <- gsub(" AS", " PLAC Alt Schowe ", text_vec)
  text_vec <- gsub(" NS", " PLAC Neu Schowe ", text_vec)
  text_vec <- gsub("( Lager [a-zA-Z]+)", " PLAC \\1", text_vec)
  text_vec <- gsub(" ev\\.", " RELI Evangelical ", text_vec)
  text_vec <- gsub(" ref\\.", " RELI Reformed ", text_vec)
  text_vec <- gsub(" kath\\.", " RELI Catholic ", text_vec)
  text_vec <- gsub(" TZ:", " WITN: ", text_vec)
  text_vec <- gsub(" TP:", " GODP: ", text_vec)
  text_vec <- gsub("~", " BAPM ", text_vec)
  text_vec <- gsub("# ", "NOTE ", text_vec)
  text_vec <- gsub("[<>] (\\d{1,4}\\.\\d{1,2})", " link to @I\\1@ ", text_vec)
  text_vec <- gsub("[<>] (\\d{1,4})", " link to @I\\1@ ", text_vec)
  return(text_vec)
}

cat(clean_text(text_to_parse))

# function to take a series of lines and return only the lines
# between the start of the chunk and end of the chunk
to_just_gedcom <- function(text) {
  sub(".*```gedcom\\n(.*?)```.*", "\\1", text, perl = FALSE)
}

# submit to api with exponential backoff if rate limited
make_request <- function(
  req,
  max_retries = 5,
  initial_delay = 1,
  max_delay = 60
) {
  for (attempt in 1:max_retries) {
    tryCatch(
      {
        response <- req |> req_perform()
        if (resp_status(response) == 429) {
          # Rate limited
          cat(paste(
            "Attempt",
            attempt,
            "failed: Rate limited (429). Retrying...\n"
          ))
          delay <- min(initial_delay * (2^(attempt - 1)), max_delay) # Exponential backoff
          Sys.sleep(delay)
        } else if (
          resp_status(response) >= 200 && resp_status(response) < 300
        ) {
          # Success!
          cat("Request successful!\n")
          break # Exit the loop
        } else {
          # Other error
          stop(paste(
            "Request failed with status:",
            resp_status(response),
            "\nBody:",
            resp_body_string(response)
          ))
        }
      },
      error = function(e) {
        cat(paste("Attempt", attempt, "failed with error:", e$message, "\n"))
        if (attempt == max_retries) {
          stop("Max retries reached.")
        }
        delay <- min(initial_delay * (2^(attempt - 1)), max_delay)
        Sys.sleep(delay)
      }
    )
  }
  return(response)
}

process_multi_chunk <- function(record_nums, add_context = FALSE) {
  if (length(record_nums) == 0) {
    stop("No record numbers provided.")
  }
  if (!is.integer(record_nums)) {
    stop("Expecting a vector of integers. Sorry.")
  }
  # Read the text from the file
  text_to_parse <- ""
  for (n in record_nums) {
    chunk_file_name <- paste0("data/chunks/record_", sprintf("%04d", n), ".txt")
    print(paste("Reading", chunk_file_name))
    single_record <- read_lines(chunk_file_name) |>
      clean_text() |>
      paste(collapse = " ")
    text_to_parse <- paste(text_to_parse, single_record, "\n")
  }
  # Create the request body
  if (add_context) {
    body <- to_body(paste(context_prompt, paste(text_to_parse, collapse = " ")))
  } else {
    body <- to_body(paste(
      continue_prompt,
      paste(text_to_parse, collapse = " ")
    ))
  }
  # Create the request
  req <- request(base_url = model_url) |>
    req_body_json(body) |>
    req_timeout(120) |>
    req_progress()

  if (DEBUG) {
    paste("RECORDS:", paste(record_nums, collapse = " "))
    print(req)
    req_dry_run(req)
    cat(text_to_parse)
    print(record_nums)
    return(NULL)
  }
  # this won't be executed if DEBUG is TRUE
  # Make the request
  print(paste(
    "Submitting request for records",
    paste(record_nums, collapse = " ")
  ))
  response <- make_request(req)
  # Success!
  cat("Request successful!\n")
  generated_text <- resp_body_json(response)$candidates[[1]]$content$parts[[
    1
  ]]$text |>
    to_just_gedcom()

  return(generated_text)
}

DEBUG <- FALSE
start_index = 403
end_index = 4196
batch_size = 3
# process records in groups of batch_size
for (first_index in seq(start_index, end_index, by = batch_size)) {
  record_nums <- first_index:(first_index + batch_size - 1)
  # Process the chunk. if the chunk is the first one, add the context prompt
  ged_record <- process_multi_chunk(record_nums, {
    start_index == 1
  })
  cat(ged_record)
  if (!is.null(ged_record)) {
    # write the GEDCOM record to a file
    outfile <- paste0(
      "data/gedcom/ged_record_",
      sprintf("%04d", first_index),
      ".ged"
    )
    print(outfile)
    writeLines(ged_record, outfile)
  }
}
