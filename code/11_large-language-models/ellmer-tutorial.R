# Using the ellmer package to work with the LLM APIs
#
# ellmer wraps the HTTP boilerplate so you can focus on the prompts.
# Compare with code/01_getting-text-data/openai-api.R to see what it replaces.

# install.packages("ellmer")
library(ellmer)

# Store your API key: usethis::edit_r_environ()
# Add a line: OPENAI_API_KEY=sk-...
# ellmer picks it up automatically from the environment — no manual Sys.getenv() needed.


## 1. A simple chat -----------------------

# Create a chat object. It handles the endpoint, auth, and message history.
chat <- chat_openai(model = 'gpt-5.4-mini')

chat$chat('What is the capital of Japan?')

# Unlike the httr2 version, the conversation is stateful —
# follow-up questions work without rebuilding the request from scratch.
chat$chat('What is the population of that city?')


## 2. OCR an image ----------------

# ellmer has built-in helpers for image and file input

chat <- chat_openai(model = 'gpt-5.4')

# from a local file:
chat$chat(
  content_image_file('data/img/titanic.png'),
  'Convert this article to plain text. Ignore advertisements, tables, and anything otherwise not included in the article.'
)

# from a URL (just as easy):
chat$chat(
  content_image_url('https://joeornstein.github.io/text-as-data/img/titanic.png'),
  'Convert this article to plain text. Ignore advertisements, tables, and anything otherwise not included in the article.'
)
