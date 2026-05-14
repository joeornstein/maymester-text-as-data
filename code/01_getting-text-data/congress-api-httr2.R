# scrape bill summaries from the Congress API with httr2
library(httr2)
library(tidyverse)

get_bill_summary <- function(congress = 118,
                             bill_type = 's',
                             bill_number = 25){

  # get the API key from R environment
  api_key <- Sys.getenv('CONGRESS_API_KEY')

  # format the API request URL
  req <- request(base_url =
                   paste0('https://api.congress.gov/v3/bill/',
                   congress,'/',bill_type, '/',
                   bill_number, '/summaries?api_key=', api_key))

  # perform the request
  response <- req_perform(req)

  # convert to text
  content <- resp_body_json(response)

  content$summaries[[1]]$text |>
    rvest::read_html() |>
    rvest::html_text()
}

get_bill_summary()

get_bill_summary(congress = 118,
                 bill_type = 'hr',
                 bill_number = 4818)


get_bill_summary(congress = 89,
                 bill_type = 's',
                 bill_number = 1564)



