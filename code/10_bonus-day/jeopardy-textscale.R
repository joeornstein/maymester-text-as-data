# measure the difficulty of Jeopardy! questions
# using the textscale approach

library(tidyverse)
library(textscale)
library(glue)


df <- read_tsv('https://raw.githubusercontent.com/jwolle1/jeopardy_clue_dataset/refs/heads/main/combined_season1-41.tsv')

# filter the dataset
df <- df |>
  filter(round == 1,
         air_date > '2002-01-07') |>
  mutate(text = glue('CATEGORY: {category}\nANSWER: {answer}\nQUESTION: {question}') )

# subset for testing purposes
set.seed(151325)
df_subset <- df |>
  slice_sample(n = 1e4)

# textscale
results <- textscale(
  df_subset$text,
  prompt = 'Which Jeopardy! clue requires a more obscure, specialized, or higher level of trivia knowledge to solve?',
  seed = 1234,
  embeddings_cache = 'raw/jeopardy_dataset_seasons_1-41/jeopardy_embeddings.rds',
  annotations_cache = 'raw/jeopardy_dataset_seasons_1-41/jeopardy_annotations.rds',
  parallel = TRUE
)


plot(results)

df_subset <- bind_cols(df_subset, results$scores)


# surprisingly a lot of overlap on question difficulty
# across dollar value.
ggplot(data = df_subset,
       mapping = aes(x=score,
                     y = clue_value,
                     group = factor(clue_value))) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = seq(200,1000,200)) +
  labs(x = 'Difficulty', y = 'Clue Value')

# this seems odd - so it's worth performing some face validity checks

# these are the hardest clues worth $200, plus 95% CIs
df_subset |>
  filter(clue_value == 200) |>
  slice_max(score, n = 3) |>
  mutate(q = glue('{category}: {answer} [{round(lower, 2)}, {round(upper, 2)}]\n\n')) |>
  pull(q)

# these are the easiest clues worth $1,000, plus 95% CIs
df_subset |>
  filter(clue_value == 1000) |>
  slice_min(score, n = 3) |>
  mutate(q = glue('{category}: {answer} [{round(lower, 2)}, {round(upper, 2)}]\n\n')) |>
  pull(q)

# the $200 clues seems harder than the $1,000 clues to me!




# no evidence that questions have become more
# difficult since 2002.
ggplot(df_subset,
       mapping = aes(x=air_date,y=score)) +
  geom_smooth()

