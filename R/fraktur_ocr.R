# fraktur OCR
library(tidyverse)
library(tesseract)
library(magick)
library(tokenizers)
library(tidytext)
library(hunspell)
library(translateR)

# Fraktur training set must be loaded. Only run once.
# tesseract::tesseract_download("frk")
google.api.key = Sys.getenv("GGMAP_GOOGLE_API_KEY")
path = "C:/Users/Arthur/Documents/Scans/"
#path = "C:/Users/Arthur/Pictures/ControlCenter4/Scan/"
file_name = "Neu_Schowe_Church_19.jpg"

#scan at 300dpi, highest brighness setting
ocr_lines <- function(file,engine = "frk"){
   tesseract::ocr(file,engine = engine) %>%
      tokenize_regex(pattern = "\\n") %>%
      unlist() %>%
      enframe(name = "line_num",value="line")
}

ocr_words <- function(file,engine = "frk"){
   tesseract::ocr(file,engine = engine) %>%
      tokenize_words(strip_punct = FALSE)
}
ocr_vector <- function(file,engine = "frk"){
   tesseract::ocr(file,engine = engine) %>%
      str_remove_all("\\n")
}

# PROCESS PAGE IMAGE ----------------------------------------------------
process_page <- function(file_name){
   # IMAGE PROCESSING FOR BETTER OCR, IF NECESSARY -----------------------
   #page_img <- image_read(paste0(path,file_name))
   #page_img <- image_enhance(page_img)
   #page_img <- image_contrast(page_img,sharpen = 10)
   #page_img <- image_quantize(page_img,max = 2,colorspace = "gray")
   #page_img  # Preview image assuming you use RStudio
   #image_write(page_img,paste0(path,file_name))


   # USE TESSERACT OCR ENGINE ----------------------------------------
   lines_raw <- ocr_lines(paste0(path,file_name))

   # convert "ſ" to s
   German <- lines_raw %>%
      mutate(line = str_replace_all(line,"ſ","s")) %>%
      mutate(line = str_remove_all(line,"[\\|\\*<>»]")) %>%
      mutate(line = ifelse(str_detect(line,"[a-zA-Z]3"),str_replace(line,"3","s"),line)) %>%
      mutate(line = str_squish(line)) %>%
      mutate(line = str_replace_all(line,"Ginleitung","Einleitung")) %>%
      mutate(line = str_replace_all(line,"=","-"))

   words <- tokenizers::tokenize_words(lines$German,strip_punct = FALSE) %>%
      unlist(recursive = FALSE)


   # USE HUNSPELL SPELL CHECKER IN GERMAN
   # USE RSTUDIO Tools/Global/Spelling TO SELECT GERMAN LANGUAGE TO ENSURE IT IS LOADED
   #list_dictionaries()
   lang =  "de_DE"
   #dictionary(lang = lang)

   words_df <- enframe(words,name=NULL,value="word") %>%
      mutate(correct=hunspell_check(word,dict = lang)) %>%
      mutate(suggestions=hunspell_suggest(word,dict = lang))

   # get most likely term
   best_suggestion <- words_df$suggestions %>%
      map(pluck,1,.default=NA) %>%
      unlist() %>%
      enframe(name=NULL,value="suggestion")

   # choose alternate word
   words_df2 <- words_df %>%
      bind_cols(best_suggestion) %>%
      select(-suggestions) %>%
      # take first suggestion for "incorrect" words or....
      #  mutate(word_alt = ifelse(correct,word,suggestion)) %>%
      # Only correct capitalization errors beacuse we think Google translate
      # has a bigger word list and is smarter about misspellings
      mutate(suggestion = ifelse(is.na(suggestion)," ",suggestion)) %>%
      mutate(word_alt = word) %>%
      mutate(word_alt = ifelse(str_to_title(word)==suggestion,suggestion,word_alt)) %>%
      {.}

   cleaned <- pull(words_df2,word_alt) %>%
      paste(collapse = " ") %>%
      str_squish() %>%
      str_replace_all("- -","--")

   cleaned %>%
      writeClipboard(format = 13) # format 13 is unicode to allow transfer of special fraktur characters to clip


   # send to google translate
   # getGoogleLanguages()
   # note we use a character vector not a data frame which is slower and provides less context (less accurate).
   translated <- translate(content.vec = cleaned,
                           google.api.key = google.api.key,
                           source.lang = "de",
                           target.lang = "en")

   # paginate translation
   words_trans <- tokenizers::tokenize_words(translated,strip_punct = FALSE) %>%
      unlist(recursive = FALSE)

   words_count <- tokenizers::tokenize_words(translated,strip_punct = FALSE) %>%
      unlist(recursive = FALSE) %>%
      length()
   words_per_line = round(words_count / nrow(lines))
   #build page data frame  do it old school loop since we have to test for punctuation
   word_pos = 1
   cur_page <- list()
   for (i in 1:nrow(lines)){
      cur_line = ""
      line_pos = 1
      while(line_pos < words_per_line + 1){
         cur_word <- ifelse(word_pos > words_count,"",words_trans[word_pos])
         cur_line = paste(cur_line,cur_word)
         word_pos <- word_pos + 1
#         if(str_detect(words_trans[word_pos],"[\\w]")){
#            line_pos <- line_pos + 1
#         }
         line_pos <- line_pos + 1
      }
      cur_page <- c(cur_page,cur_line)
   }

   en_page <- unlist(cur_page,recursive = FALSE) %>% enframe(name=NULL,value="English") %>%
      mutate(English = str_replace_all(English," \\,",",")) %>%
      mutate(English = str_replace_all(English," \\.",".")) %>%
      mutate(English = str_squish(English))

   content <- bind_cols(lines,en_page)

   processed <- tibble(img_file=file_name,
          ocr_engine = "Tesseract",
          translator = "Google",
          lines,
          en_page) %>%
      nest(data = c(line_num,German,English)) %>%
      rename(content = data) %>%
      {.}


      return(processed)
}

# main loop -----------------------------------------------------------------------------
processed <- process_page(file_name)
