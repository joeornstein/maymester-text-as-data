# Create dictionaries for science and evidence-based words
# and religion and faith-based words, and count up
# the number of times members of each party use them in
# their floor speeches

library(tidyverse)
library(httr2)

openai_api_key <- Sys.getenv('OPENAI_API_KEY')


## 1. A simple chat -----------------------

text <- 'Give me a list of 1,000 words associated with science and research that a member of Congress might use in a floor speech.'

base_url <- 'https://api.openai.com/v1/responses'

# create a "request" object
req <- request(base_url) |>
  # add "headers" to the base_url
  req_auth_bearer_token(token = openai_api_key)  |>
  req_headers("Content-Type" = "application/json") |>
  # add the "body" of the request
  req_body_json(list(model = 'gpt-4.1',
                     input = text))

resp <- req_perform(req)

contents <- resp |>
  resp_body_json()
contents$output[[1]]
