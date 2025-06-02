# Using the httr2 package to work with the OpenAI API

library(tidyverse)
library(httr2)
library(promptr) # a package I developed for prompting OpenAI models

# you'll need an API key, and it couldn't hurt to put $5 in your developer account

# once you have your API key, you can add it to your R environment
# for safe keeping
promptr::openai_api_key('<YOUR API KEY GOES HERE>')

# once you've done that, you can access your API key like so:
openai_api_key <- Sys.getenv('OPENAI_API_KEY')


## 1. A simple chat -----------------------

text <- 'What is the capital of Japan?'

base_url <- 'https://api.openai.com/v1/responses'

# create a "request" object
req <- request(base_url) |>
  # add "headers" to the base_url
  req_auth_bearer_token(token = openai_api_key)  |>
  req_headers("Content-Type" = "application/json") |>
  # add the "body" of the request
  req_body_json(list(model = 'gpt-4.1',
                     input = text))

# then you can perform the request and receive a response from the API,
resp <- req_perform(req)

# parse the response
contents <- resp |>
  resp_body_string() |>
  jsonlite::fromJSON()

contents$status
contents$error
contents$output
contents$output$content[[1]]$text


## 2. OCR an image ----------------

# https://platform.openai.com/docs/guides/images-vision?api-mode=responses

titanic <- magick::image_read('data/img/titanic.png')
titanic

# to upload an image to API, we need to encode it as Base 64
titanic <- base64enc::base64encode('data/img/titanic.png')
titanic

# create a "URL" from that base 64 encoding
image_data_url <- paste0("data:image/png;base64,", paste0(titanic, collapse = ""))
# note that if the image is online, you can just supply its URL


# create the "request" object
req <- request(base_url) |>
  # add "headers" to the base_url
  req_headers('Authorization' = paste("Bearer", openai_api_key)) |>
  req_headers("Content-Type" = "application/json") |>
  # add the "body" of the request
  req_body_json(list(model = 'gpt-4.1',
                     input = list(
                       list(
                         role = "user",
                         content = list(
                           list(
                             type = "input_text",
                             text = "Convert this article to plain text. Return only the text."
                           ),
                           list(
                             type = "input_image",
                             image_url = image_data_url
                             #image_url = 'https://joeornstein.github.io/text-as-data/img/titanic.png'
                           )
                         )
                       )
                     )))
resp <- req_perform(req)
contents <- resp |> resp_body_string() |> jsonlite::fromJSON()
text <- contents$output$content[[1]]$text
cat(text)
