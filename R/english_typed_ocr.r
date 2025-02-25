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
whitelist <- c(letters, LETTERS, 0:9, "/", "\\", ".", " ", ",")
whitelist

tesseract(
  options = list(
    "preserve_interword_spaces" = "1",
    "tessedit_pageseg_mode" = "4",
    "tessedit_char_whitelist" = whitelist
  )
)


#pngfiles <- pdftools::pdf_convert('img ENG/Wilhelm Veh Letters.pdf', dpi = 600)
pngfiles <- pdftools::pdf_convert(
  'img ENG/ohio war dead ww1.pdf',
  dpi = 250,
  pages = 4:6
)

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

writings <- list()
for (n in 1:length(pngfiles)) {
  page_img <- image_read(pngfiles[n])
  writings[n] <- page_img %>%
    # pre_process() %>%
    image_ocr()
}

# anything not in this list
#bad_chars <- "[^\\a-zA-Z 0-9\\.\\/\\(\\)]"
temp <-
  writings %>%
  unlist() %>%
  tokenize(tokenizer_delim("\n")) %>%
  unlist() %>%
  enframe %>%
  separate(
    value,
    sep = "(, )| ",
    into = c("last_name", "first_name", "m_i"),
    extra = "merge"
  ) %>%
  separate(m_i, sep = " ", into = c("m_i", "unit"), extra = "merge") %>%
  identity()

temp

writings

#write_lines(unlist(writings),"Wilhelm_Veh_letters.txt")

library(tesseract)
tesseract_params("preserve_interword_spaces")
tesseract(options = list("preserve_interword_spaces" = "1"))
tesseract_params("preserve_interword_spaces")
