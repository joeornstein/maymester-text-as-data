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
chat <- chat_openai(model = 'gpt-5.4-mini',
                    system_prompt = 'You are an expert political scientist.')

chat$chat('What is the capital of Japan?')

# Unlike the httr2 version, the conversation is stateful —
# follow-up questions work without rebuilding the request from scratch.
chat$chat('What is the population of that city?')


## 2. Submit multiple prompts in parallel -----------------

countries <- c('Japan', 'United States', 'Peru', 'Brazil',
               'Canada', 'Georgia', 'Argentina')

prompts <- interpolate('What is the capital of {{countries}}?')

chat <- chat_openai(model = 'gpt-5.4-mini',
                    system_prompt = 'Return the name of the capital *only*...please.')
capitals <- parallel_chat_text(chat, prompts)
capitals


## 3. OCR an image ----------------

# ellmer has built-in helpers for image and file input

chat <- chat_openai(model = 'gpt-5.4',
                    system_prompt = 'Only return the transcription, please.')

# from a local file:
resp <- chat$chat(
  content_image_file('data/img/titanic.png', resize = 'high'),
  'Please convert this entire article to plain text. Ignore advertisements, insets, tables, and anything otherwise not part of the article.'
)

resp


# from a URL (just as easy):
chat$chat(
  content_image_url('https://joeornstein.github.io/text-as-data/img/titanic.png'),
  'Convert this article to plain text. Ignore advertisements, tables, and anything otherwise not included in the article.'
)
