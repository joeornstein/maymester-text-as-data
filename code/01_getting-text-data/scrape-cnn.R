library(tidyverse)
library(rvest)

scrape_cnn_article <- function(url){
  # 1. Read the HTML from the page
  page <- read_html(url, encoding = 'UTF-8')

  # 2. Get the paragraph elements from the article body
  paragraphs <- html_elements(page, '.article__content p')

  # 3. Extract the raw text
  text <- html_text2(paragraphs)

  # 4. Concatentate all the text into a single string
  text <- paste(text, collapse = ' ')

  return(text)

}

urls <- c('https://www.cnn.com/2026/05/13/economy/us-ppi-wholesale-inflation-april',
          'https://www.cnn.com/2026/05/12/middleeast/israel-iran-us-nuclear-deal-trump-intl',
          'https://www.cnn.com/2026/05/13/politics/taiwan-anxiously-eyes-trumps-summit-in-china-with-usd14-billion-in-us-arms-sales-up-in-the-air')

articles <- map_chr(urls, scrape_cnn_article)




