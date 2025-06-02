## Get Raw Text of Documents from the Federal Register API

library(tidyverse)
library(httr)
library(jsonlite)

# Step 1: Get the full text URL from the Federal Register API
base_url <- 'https://www.federalregister.gov/api/v1/documents/'
document_number <- '2025-08464'
full_url <- paste0(base_url, document_number, '.json')

# Use the GET() function to get the JSON file from that URL
response <- GET(full_url)

# get the content from that JSON with the content() function
content <- content(response)

# Now the raw text URL is an entry in the content vector
content$raw_text_url

# Step 2: use read_file() to read in the full text
text <- read_file(content$raw_text_url)

cat(text)


## Create a function to do all that for any document number ---------

get_document <- function(doc_number){

  base_url <- 'https://www.federalregister.gov/api/v1/documents/'
  full_url <- paste0(base_url, doc_number, '.json')

  # Use the GET() function to get the JSON file from that URL
  response <- GET(full_url)

  # get the content from that JSON with the content() function
  content <- content(response)

  # Step 2: use read_file() to read in the full text
  read_file(content$raw_text_url)
}

get_document('2025-08464')

# make sure that works with any document number
get_document('2025-08416')



## Get All Proposed Rules (max 1,000) from 2025 -------------------------

# note: we got this URL from the Federal Register API documentation, after specifying which types of documents we wanted
response <- GET('https://www.federalregister.gov/api/v1/documents.json?fields[]=publication_date&fields[]=document_number&fields[]=raw_text_url&fields[]=title&per_page=1000&conditions[publication_date][year]=2025&conditions[type][]=PRORULE')

# convert the response into an R list
content <- response$content |>
  rawToChar() |>
  fromJSON()

# convert to dataframe
df <- content$results
df$text <- NA # initialize an empty column of text

for(i in 1:nrow(df)){
  print(i)
  response <- GET(df$raw_text_url[i])
  stop_for_status(response)
  df$text[i] <- content(response, as = "text", encoding = "UTF-8")
  # build in a little sleep time
  Sys.sleep(1)
}

save(df, file = 'data/federal-register/proposed-rules-2025.RData')
