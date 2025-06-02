## NYTimes API
library(tidyverse)
library(httr)

# to access the NYTimes API, we need an API Key
# to get one, sign up for a Developer Account, create an App, and copy
# the API key. It took me 2 minutes.

api_key <- read_file('data/nytimes-api-key.txt')

# get articles from the example URL
url <- paste0('https://api.nytimes.com/svc/archive/v1/2025/4.json?api-key=',
              api_key)
response <- GET(url)

# convert that to an R object
content <- response$content |>
  rawToChar() |>
  jsonlite::fromJSON()

# pull out the dataframe
df <- content$response$docs
