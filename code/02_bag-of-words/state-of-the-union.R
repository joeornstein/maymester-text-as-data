#' ---
#'
#' title: Bag of words representation of State of the Union speeches over time
#'
#' ---

library(tidyverse)
library(tidytext)
library(sotu)
library(SnowballC)
library(wordcloud2)

## 1. Tokenize to the sentence level ------------------------

df <- sotu_meta |>
  mutate(text = sotu_text)

sotu_sentences <- df |>
  unnest_tokens(output = 'sentence',
                input = 'text',
                token = 'sentences')

# create a unique sentence ID
sotu_sentences$sentence_id <- 1:nrow(sotu_sentences)

## 2. Keep only the sentences that use the word 'manufacturing' ---------

sotu_sentences <- sotu_sentences |>
  filter(str_detect(sentence, 'manufactur'))


## 3. Tokenize to the word level -------------------------

# this sequence of operations will produce a
# dataframe of every word that appears in the
# same sentence as "manufacturing" in the corpus
sotu_words <- sotu_sentences |>
  unnest_tokens(input = 'sentence',
                output = 'word') |>
  # remove stopwords
  anti_join(get_stopwords()) |>
  # convert to word stems
  mutate(word_stem = wordStem(word)) |>
  # remove numeric word stems
  filter(str_detect(word_stem, '[0-9]', negate = TRUE))


# look at the frequency distribution of these words
sotu_words |>
  filter(word_stem != 'manufactur') |>
  count(word_stem) |>
  ggplot(mapping = aes(x=n)) +
  geom_histogram(color = 'black')

# let's remove the words that only appear once
sotu_words <- sotu_words |>
  group_by(word_stem) |>
  filter(n() > 1) |>
  ungroup()


## 4. Create a word cloud -------------------------

sotu_words |>
  filter(word_stem != 'manufactur') |>
  count(word_stem) |>
  wordcloud2()

# only the words that appear at least 10 times:
sotu_words |>
  group_by(word_stem) |>
  filter(n() > 9) |>
  ungroup() |>
  filter(word_stem != 'manufactur') |>
  count(word_stem) |>
  wordcloud2()


sotu_words |>
  filter(word_stem != 'manufactur') |>
  count(word_stem) |>
  arrange(-n)


# compare the 18th/19th century with the 20th/21st century
sotu_words |>
  filter(word_stem != 'manufactur') |>
  mutate(period = if_else(year < 1900,
                          'Before 1900',
                          'After 1900')) |>
  group_by(period) |>
  count(word_stem) |>
  # keep just the top 25 words from each era
  slice_max(n, n = 25) |>
  # plot it
  ggplot(mapping = aes(
    # reorder the word_stem variable by frequency so it looks pretty on a chart
    y = reorder_within(word_stem, n, period),
    x = n)) +
  geom_col() +
  scale_y_reordered() +
  facet_wrap(~period, scales = 'free') +
  labs(x = 'Count', y = 'Word Stem')
