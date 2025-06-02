## Get Raw Text of Proposed Rules from Federal Register Bulk Data Repository

# data downloaded from https://www.govinfo.gov/bulkdata/FR
# saved in raw/FR-2025 directory


## Step 1: Get the dates and document numbers of the proposed rules -------------------

library(tidyverse)
library(httr)
library(xml2)

# get metadata on proposed rules from 2025
response <- GET('https://www.federalregister.gov/api/v1/documents.json?fields[]=publication_date&fields[]=document_number&fields[]=raw_text_url&fields[]=title&per_page=1000&conditions[publication_date][year]=2025&conditions[type][]=PRORULE')

# convert the response into an R list
content <- response$content |>
  rawToChar() |>
  jsonlite::fromJSON()

df <- content$results

## Step 2: For each document, find its entry in the bulk data download ----

library(lubridate)

df$publication_date[1]
df$document_number[1]

# get the month folder
month <- month(df$publication_date[1])

# read that xml file
xml_file <- read_xml(paste0('raw/FR-2025/0', month, '/FR-',
                            df$publication_date[1], '.xml'))

# it's a nested structure where every item is surrounded by tags
# start by finding every entry surrounded by a "PRORULES" tag,
# then within those tags, every entry surround by a "PRORULE" tag
proposed_rules <- xml_file |>
  xml_find_all('PRORULES') |>
  xml_find_all('PRORULE')

# now filter the entries that contain the FRDOC tag we want
rule_text <- proposed_rules |>
  keep(~ {
    frdoc <- xml_text(xml_find_first(.x, ".//FRDOC"))
    str_detect(frdoc, df$document_number[1])
  }) |>
  xml_text()

## Put all that into a loop --------------------------

for(i in 1:nrow(df)){

  print(i)

  # get the month
  month <- month(df$publication_date[i])

  # read the xml file
  xml_file <- read_xml(paste0('raw/FR-2025/0', month, '/FR-',
                              df$publication_date[i], '.xml'))

  # keep only the proposed rules
  proposed_rules <- xml_file |>
    xml_find_all('PRORULES') |>
    xml_find_all('PRORULE')

  # keep only rules with matching document numbers
  filtered_rule <- proposed_rules |>
    keep(~ {
      frdoc <- xml_text(xml_find_first(.x, ".//FRDOC"))
      str_detect(frdoc, df$document_number[i])
    })

  if(length(filtered_rule) == 0){
    next
  }

  # extract the text of the rule we want
  df$text[i] <- xml_text(filtered_rule)
}

# test comment