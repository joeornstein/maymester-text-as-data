library(tidyverse)
library(pdftools)

pdf <- pdf_text('data/budget-pdfs/2025 Appropriation ACT as Passed.pdf')
page <- pdf[2] # page with the State House HQTRS Budget

# remove the header and footer (same on every page, except for the page number)
page <- page |>
  str_remove_all('Federal Republic of Nigeria            2025 APPROPRIATION ACT\n\n                                                                                         2025\n') |>
  str_remove_all('\n\nNATIONAL ASSEMBLY                           Page [0-9]+ of 2185            2025 APPROPRIATION ACT\n')

# parse the plain text into a dataframe
df <- page |>
  # first replace any sequence of two or more whitespace characters with a tab
  str_replace_all('\\s{2,}', '\t') |>
  # the read_tsv() function converts it to a dataframe, assuming tabs denote new columns
  read_tsv(skip = 1) # skip the first line
