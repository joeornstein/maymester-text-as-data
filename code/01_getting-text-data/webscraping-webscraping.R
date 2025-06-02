# Webscraping the Webscraping Tutorial

library(tidyverse)
library(rvest)

# first get the HTML from the webpage
page <- read_html('https://joeornstein.github.io/text-as-data/webscraping.html')

# next, find the element(s) you want to keep
text <- page |>
  html_elements('ol , #practice-problems, #being-polite, #selectorgadget, #getting-the-right-elements, #reading-html, #the-rvest-package, h1, p') |>
  html_text2()

# this sequence of functions helps the text print nicely in a report
text |>
  paste(collapse = ' ') |> # collapse the vector into a single string
  str_wrap() |> # wrap the text by inserting \n (line breaks)
  cat() # print the text so that the line breaks appear correctly
