# ---
# Code the "typicality" of the book descriptions (Le Mens et al, 2013)
# ---

library(tidyverse)
library(promptr)
library(glue)

# read the cleaned up data from cleanup-le-mens.R
d <- read_csv('data/goodreads/cleaned-data.csv')

prompt_instructions <- "Here's a book description from Goodreads: \"{text}\" How typical is this book description of a mystery novel? Provide your response as a score between 0 and 100 where 0 means \"Not typical at all\" and 100 means \"Extremely typical\". Structure your response as a single plain-text numeral."
prompt_instructions

# create a version of the dataset with one row per book
books <- d |>
  select(id, text) |>
  unique() |>
  mutate(prompt = glue(prompt_instructions))


# give those formatted prompts to the OpenAI API
prompts <- books$prompt |>
  lapply(format_chat)

resp <- complete_chat(prompts, model = 'gpt-4.1')
save(resp, file = 'data/goodreads/books-gpt-4.1.RData')

# for each book, the score will be a probability-weighted average response
books$score <- resp |>
  # convert all the tokens to numbers
  lapply(mutate, token = as.numeric(token)) |>
  # remove the ones it can't parse as numbers
  lapply(filter, !is.na(token)) |>
  # get the weighted mean
  lapply(summarize, score = weighted.mean(token, probability)) |>
  unlist()

# compare that against the human coders
human_codes <- d |>
  group_by(id) |>
  summarize(human_code = mean(typ_human),
            median_human = median(typ_human))


books <- left_join(books, human_codes)

ggplot(data = books,
       mapping = aes(x = human_code,
                     y = score)) +
  geom_point(alpha = 0.5) +
  labs(x = 'Average Human Typicality Score',
       y = 'GPT-4 Typicality Score') +
  theme_bw()

cor(books$score, books$human_code)


ggplot(data = books,
       mapping = aes(x = median_human,
                     y = score)) +
  geom_point(alpha = 0.5) +
  labs(x = 'Median Human Typicality Score',
       y = 'GPT-4 Typicality Score') +
  theme_bw()

cor(books$score, books$human_code)
cor(books$score, books$median_human)

## Pairwise Comparison Approach -----------------------------

# for each book, let's perform a pairwise comparison with 5
# other randomly chosen books

set.seed(42)
# get all possible combinations
book_combinations <- books$id |>
  combn(m = 2) |>
  # transpose
  t() |>
  as_tibble() |>
  # for each book in column one, pick 5 random books
  group_by(V1) |>
  slice_sample(n = 5) |>
  # merge the text
  left_join(
    books |>
      select(V1 = id,
             text1 = text)
  ) |>
  left_join(
    books |>
      select(V2 = id,
             text2 = text)
  )


# create prompts
prompt_instructions <- "Book Description A: \"{text1}\"\n\nBook Description B: \"{text2}\"\n\nWhich book description from Goodreads is more typical of a mystery novel? Structure your response as a single plain-text character; either A (if the first book description) or B (if the second book description)."
cat(prompt_instructions)

book_combinations <- book_combinations |>
  mutate(prompt = glue(prompt_instructions))

prompts <- book_combinations$prompt |>
  lapply(format_chat)

resp <- complete_chat(prompts, model = 'gpt-4.1')
save(resp, file = 'data/goodreads/book-comparisons-gpt-4.1.RData')

# assign pairwise comparison labels
book_combinations$winner <- resp |>
  lapply(slice_max, probability, n = 1, with_ties = FALSE) |>
  lapply(pull, token) |>
  unlist()

table(book_combinations$winner)

book_combinations <- book_combinations |>
  filter(winner != 'Neither') |>
  mutate(result = as.numeric(winner == 'A'))

library(BradleyTerry2)

mod <- BTm(outcome = result,
           player1 = factor(V1, levels = books$id),
           player2 = factor(V2, levels = books$id),
           data = book_combinations)
