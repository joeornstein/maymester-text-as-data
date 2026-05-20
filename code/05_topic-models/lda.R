#' ---
#'
#' Intro to Latent Dirichlet Allocation topic models
#'
#' ---

library(tidyverse)
library(tidytext)
library(topicmodels)
library(SnowballC)

## Step 1: Load the Lautenberg press releases and tidy up ---------------------------

load('data/press-releases/lautenberg.RData')


tidy_press_releases <- df |>
  mutate(text = str_replace_all(text,
                                pattern = '     Senator Frank R  Lautenberg                                                                                                                      Press Release        of        Senator Lautenberg                                                                                ',
                                replacement = '')) |>
  # replace "New Jersey" with "NJ" so it appears as the same token
  mutate(text = str_replace_all(text, 'New Jersey', 'NJ')) |>
  # tokenize to the word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # remove numerals
  filter_out(str_detect(word, '[0-9]')) |>
  # create word stems
  mutate(word_stem = wordStem(word)) |>
  # count up bag of word stems
  count(id, word_stem) |>
  # # remove the infrequent word stems
  filter(n > 2) |>
  # remove single-character word stems
  filter(nchar(word_stem) != 1) |>
  # compute tf-idf
  bind_tf_idf(term = 'word_stem',
              document = 'id',
              n = 'n') |>
  filter(!is.na(tf_idf))

# create document-term matrix
# NOTE: LDA needs a matrix of word counts, just like the multinomial bag of words model
lautenberg_dtm <- cast_dtm(data = tidy_press_releases,
                           document = 'id',
                           term = 'word_stem',
                           value = 'n')
lautenberg_dtm


## Step 2: Fit the LDA model ----------------------------

lautenberg_lda <- LDA(lautenberg_dtm, k = 30, control = list(seed = 42))


## Step 3: Examine the topic-level probability vectors -----------------------

# use the tidy() function from tidytext to extract the beta vector
lautenberg_topics <- tidy(lautenberg_lda, matrix = 'beta')


# what are the top terms by topic?
lautenberg_topics |>
  group_by(topic) |>
  slice_max(beta, n=10) |>
  arrange(topic, -beta)

# surprise surprise. "lautenberg" is a common word no matter which bag of words you pick.
# let's do the thing we were doing before where we look at the most *over-represented* words in each topic

lautenberg_topics |>
  # get each word's average beta across topics
  group_by(term) |>
  mutate(average_beta = mean(beta)) |>
  ungroup() |>
  # compare beta in that topic with the average beta
  mutate(delta = beta - average_beta) |>
  # get the words with the largest difference in each topic
  group_by(topic) |>
  slice_max(delta, n = 10) |>
  # plot it
  ggplot(mapping = aes(x=delta, y=reorder(term, delta))) +
  geom_col() +
  theme_minimal() +
  facet_wrap(~topic, scales = 'free') +
  labs(x = 'Term Probability Compared to Average',
       y = 'Term')

ggsave('topics.png', width = 10, height = 10)

## Step 4: Examine the document-level probability vectors --------------------------

lautenberg_documents <- tidy(lautenberg_lda, matrix = 'gamma') |>
  mutate(document = as.numeric(document)) |>
  arrange(document, topic)

lautenberg_documents

# what are the most high-probability topics assigned to press release #1?
lautenberg_documents |>
  filter(document == 2) |>
  arrange(-gamma)

