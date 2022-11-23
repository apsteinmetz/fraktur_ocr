# English OCR
library(tidyverse)
library(tesseract)
library(magick)
library(imager)
library(imagerExtra)
library(tokenizers)
library(tidytext)
library(hunspell)
library(translateR)
library(pdftools)
library(magick)


google.api.key = Sys.getenv("GGMAP_GOOGLE_API_KEY")
path = "img ENG/"
# set to column extraction
tesseract(options = list("tessedit_pageseg_mode"="1"))

tesseract_info()

#pngfiles <- pdftools::pdf_convert('img ENG/Wilhelm Veh Letters.pdf', dpi = 600)
pngfiles <- pdftools::pdf_convert('img ENG/ohio war dead ww1.pdf',dpi = 250, pages = 1:5)

pre_process <- function(pageimg) {
   # boost brightness to reduce bleed through on two-sided pages
#   pageimg <- image_modulate(page_img, brightness = 1000)
#   pageimg <- image_contrast(pageimg,sharpen = 10)
   #pageimg <- image_edge(pageimg)
   pageimg <- pageimg %>%
      image_quantize(max = 2)
   return(pageimg)
}


writings <- list()
for (n in 2:length(pngfiles)){
   page_img <- image_read(pngfiles[n])
   writings[n] <- page_img %>%
      image_ocr()
}

# anything not in this list
bad_chars <- "[^\\a-zA-Z 0-9\\.\\/\\(\\)]"
temp <-
   writings %>%
   unlist() %>%
   # get rid of odball characters
   str_replace_all(bad_chars," ") %>%
   str_replace_all('"'," ") %>%
   tokenize(tokenizer_delim("\n")) %>%
   unlist() %>%
   enframe %>%
   separate(value,into = c("last_name","first_name"),extra = "merge") %>%
   identity()

temp

writings

#write_lines(unlist(writings),"Wilhelm_Veh_letters.txt")


