# fraktur OCR
library(tidyverse)
library(tesseract)
library(magick)
library(imager)
library(imagerExtra)
library(tokenizers)
library(tidytext)
library(hunspell)
library(googleLanguageR)
library(pdftools)

# Fraktur training set must be loaded. Only run once.
# this training data in the official repo is old an not great
#tesseract::tesseract_download("frk")

#manually install this instead
# dl <- "https://github.com/tesseract-ocr/tessdata_best/raw/master/frk.traineddata"
# this is pretty good
# dl <- #https://github.com/tesseract-ocr/tessdata/blob/master/deu_frak.traineddata

# this crashes RSTudio
#download.file( "https://ub-backup.bib.uni-mannheim.de/~stweil/ocrd-train/data/Fraktur_5000000/tessdata_best/Fraktur_50000000.334_450937.traineddata",
#               dest = "data/Fraktur_50000000.334_450937.traineddata")


google.api.key = Sys.getenv("GGMAP_GOOGLE_API_KEY")
tesseract_info()
ocr_engine = "deu_frak"



#ocr_engine = "frk"
#path = "C:/Users/Arthur/Pictures/ControlCenter4/Scan/"
#file_name = "img_gs/Bericht ueber das hilfswerf 1935-1936 der deutsch-evangelischen gemeinde schutzberg.pdf"
file_name <- "img_gs/Jahresbericht 1934 Glogovac-Schutzberg Sommer F.pdf"
#tesseract(options = list("preserve_interword_spaces"="1",
#                         "tessedit_pageseg_mode"="4"


info <- pdf_info(file_name)
pngfiles <- pdftools::pdf_convert(file_name,dpi = 600,filenames = paste0("img_gs/gs",1:info$pages,".png"))

pre_process <- function(pageimg) {
   # boost brightness to reduce bleed through on two-sided pages
   #   pageimg <- image_modulate(page_img, brightness = 1000)
   #   pageimg <- image_contrast(pageimg,sharpen = 10)
   #pageimg <- image_edge(pageimg)
   pageimg <- pageimg %>%
      image_resize("2000x") %>%
      image_convert(type = 'Grayscale') %>%
      image_trim(fuzz = 40)
   return(pageimg)
}


page_img %>% pre_process()



#scan at 300dpi, highest brighness setting
ocr_lines <- function(file,engine = ocr_engine){
   tesseract::ocr(file,engine = engine) %>%
      tokenize_regex(pattern = "\\n") %>%
      unlist() %>%
      enframe(name = "line_num",value="line")
}

# using imagerExtra instead of magick
ocr_lines_alt <- function(file,engine = ocr_engine){
   imagerExtra::OCR(file,engine = engine) %>%
      tokenize_regex(pattern = "\\n") %>%
      unlist() %>%
      enframe(name = "line_num",value="line")
}
ocr_words <- function(file,engine = ocr_engine){
   tesseract::ocr(file,engine = engine) %>%
      tokenize_words(strip_punct = FALSE)
}
ocr_vector <- function(file,engine = ocr_engine){
   tesseract::ocr(file,engine = engine) %>%
      str_remove_all("\\n")
}


# PROCESS PAGE IMAGE ----------------------------------------------------
process_page <- function(file_name){
   # IMAGE PROCESSING FOR BETTER OCR, IF NECESSARY -----------------------
   # Trial and error
   print(paste0("Processing image ",file_name))

   # # tried imagerExtra for preprocessing. No improvement and slower
    # page_img1 <- imager::load.image(paste0(path,file_name))  %>%
    #    grayscale() %>%
    #    DenoiseDCT(sdn = .01) %>%
    #    ThresholdAdaptive(0.1, range = c(0,1)) %>%
    #    cimg2magick()


   page_img <- image_read(paste0(path,file_name)) ; page_img
   # page_img <- image_quantize(page_img,max = 2,colorspace = "gray") ; page_img
   # page_img <- image_trim(page_img,fuzz = 50) ; page_img
   page_img <- image_modulate(page_img,brightness = 90,saturation = 50); page_img
   page_img <- image_contrast(page_img,sharpen = 100) ; page_img
   page_img <- image_enhance(page_img); page_img
   #page_img <- image_lat(page_img, geometry = '2x2-10%') ; page_img

   # lines_test <- ocr_lines(page_img,engine = "deu_frak") %>%
   #    full_join(ocr_lines_alt(page_img1,engine = "deu_frak"), by = "line_num") %>%
   #    rename(magick = line.x,imager = line.y) %>%
   #    # mutate(frk = str_replace_all(frk,"ſ","s")) %>%
   #    {.}
   # View(lines_test)


   # USE TESSERACT OCR ENGINE ----------------------------------------
   # lines_raw <- ocr_lines(paste0(path,file_name))
   print("Extracting text ")
   lines_raw <- ocr_lines(page_img)

   print("Refining text ")

   # discard first line if it's the page number
   if (str_detect(lines_raw$line[1],"\\- [0-9]{1,2} \\-")){
      de_page <- lines_raw[-1,]
   } else{
      de_page <- lines_raw
   }

   de_page <- de_page %>%
      rename(German = line) %>%
      # convert "ſ" to s
      mutate(German = str_replace_all(German,"ſ","s")) %>%
      # strip characters that don't ever appear so we know they are false
      mutate(German = str_remove_all(German,"[\\|\\*<>«»;:]")) %>%
      mutate(German = ifelse(str_detect(German,"[a-zA-Z]3"),str_replace(German,"3","s"),German)) %>%
      mutate(German = str_squish(German)) %>%
      # mutate(German = str_replace_all(German,"Ginleitung","Einleitung")) %>%
      mutate(German = str_replace_all(German,"=","-"))

   words <- tokenizers::tokenize_words(de_page$German,strip_punct = FALSE) %>%
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
   print("Translating text ")
   translated <- translate(content.vec = cleaned,
                           google.api.key = google.api.key,
                           source.lang = "de",
                           target.lang = "en")

   # paginate translation
   print("Building data frame")
   words_trans <- tokenizers::tokenize_words(translated,strip_punct = FALSE) %>%
      unlist(recursive = FALSE)

   words_count <- tokenizers::tokenize_words(translated,strip_punct = FALSE) %>%
      unlist(recursive = FALSE) %>%
      length()
   words_per_line = round(words_count / nrow(de_page))
   #build page data frame  do it old school loop since we have to test for punctuation
   word_pos = 1
   cur_page <- list()
   for (i in 1:nrow(de_page)){
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

   # content <- bind_cols(German,en_page)

   processed <- tibble(img_file=file_name,
          ocr_engine = "Tesseract",
          translator = "Google",
          de_page,
          en_page) %>%
      nest(data = c(line_num,German,English)) %>%
      rename(content = data) %>%
      {.}


      return(processed)
}

# main loop -----------------------------------------------------------------------------
NS_Church_History <- tibble()
NS_Church_History <- dir("img") %>%
   map(process_page) %>%
   bind_rows() %>%
   rownames_to_column(var = "page_num")

save(NS_Church_History,file = "data/jb_church_history.rdata")
NS_Church_History %>% unnest(content) %>% write_csv("data/jb_church_history.csv")

