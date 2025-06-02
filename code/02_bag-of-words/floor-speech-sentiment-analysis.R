library(tidyverse)
library(tidytext)

load("data/congressional-floor-speeches/train_set.RData")
d <- unique(d)

# tokenize the speeches
tidy_speeches <- d |>
  # remove opening line
  mutate(speech = str_remove_all(speech, 'Mr. Speaker, I rise today')) |>
  unnest_tokens(input = 'speech',
                output = 'word') |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # remove numbers
  filter(str_detect(word, '[0-9]', negate = TRUE))

# merge the words with a sentiment lexicon
get_sentiments('bing')

tidy_speeches <- tidy_speeches |>
  inner_join(get_sentiments('bing'))

sentiment <- tidy_speeches |>
  group_by(id, party) |>
  count(sentiment) |>
  pivot_wider(values_from = 'n',
              names_from = 'sentiment',
              values_fill = 0) |>
  mutate(pct_positive = positive / (positive + negative) * 100)
