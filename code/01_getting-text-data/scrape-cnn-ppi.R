library(tidyverse)
library(rvest)

# 1. Read the HTML from the page
page <- read_html('https://www.cnn.com/2026/05/13/economy/us-ppi-wholesale-inflation-april',
                  encoding = 'UTF-8')

# 2. Get the paragraph elements from the article body
paragraphs <- html_elements(page, '.article__content p')

# 3. Extract the raw text
text <- html_text2(paragraphs)

# 4. Concatentate all the text into a single string
text <- paste(text, collapse = ' ')
