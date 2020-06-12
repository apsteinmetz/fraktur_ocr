# fraktur OCR
library(tidyverse)
library(tesseract)
library(magick)
library(tokenizers)
library(tidytext)
library(hunspell)
library(magrittr)

path = "C:/Users/Arthur/Documents/Scans/"

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

lines_raw <- ocr_lines(paste0(path,"frk_clean.jpg"))
words_raw <- ocr_words(paste0(path,"frk_clean.jpg"))

# convert "ſ" to s
words <- words_raw  %>% map(str_replace_all,"ſ","s")
lines <- lines_raw %>%
   mutate(line = str_replace_all(line,"ſ","s")) %>%
   mutate(line = str_remove_all(line,"[|*]"))

list_dictionaries()
lang =  "de_DE"
dictionary(lang = lang)

words_df <- enframe(words[[1]],name=NULL,value="word") %>%
   mutate(correct=hunspell_check(word,dict = lang)) %>%
   mutate(suggestions=hunspell_suggest(word,dict = lang))

# get most likely term
best_suggestion <- words_df$suggestions %>%
   map(pluck,1,.default=NA) %>%
   unlist() %>%
   enframe(name=NULL,value="suggestion")

# substitute first alternate
words_df2 <- words_df %>%
   bind_cols(best_suggestion) %>%
   select(-suggestions) %>%
   # take first suggestion for "incorrect" words or....
   #  mutate(word_alt = ifelse(correct,word,suggestion)) %>%
   # Only correct capitalization errors beacuse we think Google translate
   # has a bigger word list and is smarter about misspellings
   mutate(word_alt = ifelse(str_to_title(word)==suggestion,suggestion,word)) %>%
   mutate(word_alt = ifelse(str_detect(word,".-,"),word,word_alt)) %>%
   mutate(word_alt = ifelse(is.na(word_alt)," ",word_alt)) %>%
   {.}

pull(words_df2,word_alt) %>%
   paste(collapse = " ") %>%
   str_squish() %>%
   str_replace_all("[ ]+\\.","\\.") %>%
   str_replace_all("[ ]+,",",") %>%
   writeClipboard(format = 13) # format 13 is unicode to allow transfer of special fraktur characters to clip
# send to google translate
