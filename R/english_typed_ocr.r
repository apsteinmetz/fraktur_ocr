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
tesseract_info()

pngfiles <- pdftools::pdf_convert('img ENG/Wilhelm Veh Letters.pdf', dpi = 600)

pre_process <- function(page_img) {
   # boost brightness to reduce bleed through on two-sided pages
   page_img <- image_modulate(page_img, brightness = 1000)
   page_img <- image_enhance(page_img)
}


writings <- list()
for (n in 1:length(pngfiles)){
   page_img <- image_read(pngfile[n])
   writings[n] <- page_img %>%
      pre_process() %>%
      tesseract::ocr()
}
write_lines(unlist(writings),"Wilhelm_Veh_letters.txt")


