library(tidyverse)
library(promptr)

# here's how you work with prompts and chats

complete_prompt('My favorite food is')
complete_prompt('My favourite Japanese food is')

# zero-shot prompt
tweet <- 'I am not happy about this.'
prompt <- paste0('Tweet: ', tweet, '\nSentiment:')
cat(prompt)
complete_prompt(prompt)

# we can add few-shot examples to the prompt like so:
prompt <- paste0('Tweet: This is very disappointing.\nSentiment: Unhappy\nTweet: I am overwhelemed with joy!\nSentiment: Happy\nTweet: ', tweet, '\nSentiment:')
cat(prompt)
complete_prompt(prompt)

# alternatively the format_prompt() function makes this easier
prompt <- format_prompt(tweet,
                        examples = scotus_tweets_examples)
cat(prompt)
complete_prompt(prompt)

## Functions for working with Chat Endpoint ------

# let's give our problem to the most advanced chat model
chat <- format_chat(text = tweet,
                    instructions = 'Classify the sentiment of this tweet.',
                    examples = scotus_tweets_examples)

chat
complete_chat(chat, model = 'gpt-4.1')


## Try this approach on all tweets about the Masterpiece Cakeshop decision ----------

tweets <- scotus_tweets |>
  filter(case == 'masterpiece')

chats <- lapply(tweets$text,
                format_chat,
                instructions = 'The following tweet was posted after the Masterpiece Cakeshop decision, in which the US Supreme Court ruled in favor of a baker who refused to bake a wedding cake for a same-sex couple. Return a sentiment classification. Only return the word Positive, Negative, or Netural.'
)
chats

responses <- complete_chat(chats,
                           model = 'gpt-4.1')

responses[[1]]
tweets$text[1]

tweets$gpt_4.1_classification <-
  responses |>
  lapply(slice_max,
         probability,
         n = 1,
         with_ties = FALSE) |>
  lapply(pull,
         token) |>
  unlist()

# compare classification on those tweets where 2/3 authors agreed
tweets <- tweets |>
  filter(expert1 == expert2 |
           expert1 == expert3 |
           expert2 == expert3) |>
  mutate(human_code = case_when(expert1 == expert2 ~ expert1,
                                expert1 == expert3 ~ expert1,
                                expert2 == expert3 ~ expert2)
  ) |>
  mutate(human_code = case_when(
    human_code == -1 ~ 'Negative',
    human_code == 0 ~ 'Neutral',
    human_code == 1 ~ 'Positive'
  ))

table(tweets$human_code,
      tweets$gpt_4.1_classification)

# what percent of the time did humans and GPT-4.1 agree?
mean(tweets$human_code == tweets$gpt_4.1_classification)

# as part of our validation test, we want to explore where
# the humans and the model disagree
tweets |>
  filter(gpt_4.1_classification == 'Positive',
         human_code == 'Negative') |>
  pull(text)


