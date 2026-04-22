# scrape bill summaries from the Congress API with httr2
library(httr2)
library(tidyverse)

api_key <- read_file('congress-api-key.txt')

req <- request(base_url =
                 paste0('https://api.congress.gov/v3/bill/118/s/25/summaries?api_key=', api_key))
response <- req_perform(req)

content <- resp_body_json(response)

content$summaries[[1]]$text |>
  rvest::read_html() |>
  rvest::html_text()
