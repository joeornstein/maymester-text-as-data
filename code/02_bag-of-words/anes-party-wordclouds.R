library(tidyverse)
library(tidytext)
library(ggwordcloud)

# --- Load data ---
oe_wide <- read_csv(here::here("data", "anes", "anes_2024_openended_wide.csv"),
                    show_col_types = FALSE)

# --- Reshape to long: party likes/dislikes only ---
party_oe <- oe_wide |>
  select(case_id, starts_with("pre_") & contains("party")) |>
  pivot_longer(
    cols      = -case_id,
    names_to  = "question",
    values_to = "text"
  ) |>
  filter(!is.na(text)) |>
  mutate(
    party     = if_else(str_detect(question, "dem"), "Democratic", "Republican"),
    sentiment = if_else(str_detect(question, "dislike"), "Dislikes", "Likes")
  )

# --- Tokenize ---
custom_stop_words <- tibble(word = c(
  "party", "democratic", "democrat", "democrats",
  "republican", "republicans", "people", "like", "don't", "just",
  "ae", "ao", "char"
))

tokens <- party_oe |>
  mutate(text = str_to_lower(text)) |>
  # remove "I think" phrases
  mutate(text = str_remove_all(text, "i think|i don't think|i dont think")) |>
  unnest_tokens(word, text) |>
  anti_join(get_stopwords(), by = "word") |>
  anti_join(custom_stop_words, by = "word") |>
  filter(!str_detect(word, "^[0-9]+$"))

# --- Count words per panel ---
word_counts <- tokens |>
  count(party, sentiment, word, sort = TRUE) |>
  group_by(party, sentiment) |>
  slice_max(n, n = 30, with_ties = FALSE) |>
  ungroup()

# --- Word clouds ---
# One panel per party × sentiment combination
word_counts |>
  mutate(
    panel = factor(
      paste(party, "Party —", sentiment),
      levels = c(
        "Democratic Party — Likes", "Republican Party — Likes",
        "Democratic Party — Dislikes", "Republican Party — Dislikes"
      )
    ),
    color = case_when(
      party == "Democratic" & sentiment == "Likes"    ~ "#2166ac",
      party == "Democratic" & sentiment == "Dislikes" ~ "#92c5de",
      party == "Republican" & sentiment == "Likes"    ~ "#d6604d",
      party == "Republican" & sentiment == "Dislikes" ~ "#f4a582"
    )
  ) |>
  ggplot(aes(label = word, size = n, color = color)) +
  geom_text_wordcloud(seed = 4273) +
  scale_color_identity() +
  scale_size_area(max_size = 10) +
  facet_wrap(~panel, nrow = 2) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))


## Instad of stripping stop words, lt's only kp words in th sntiment dictionary ------

custom_sentiment_lexicon <- get_sentiments() |>
  filter(!(word %in% c('progressive', 'abort', 'aborted', 'conservative',
                       'right', 'left-leaning', 'trump')))

tokens <- party_oe |>
  unnest_tokens(word, text) |>
  inner_join(custom_sentiment_lexicon, by = 'word')


word_counts <- tokens |>
  count(party, sentiment.x, word, sort = TRUE) |>
  group_by(party, sentiment.x) |>
  slice_max(n, n = 30, with_ties = FALSE) |>
  ungroup()

word_counts |>
  mutate(
    panel = factor(
      paste(party, "Party —", sentiment.x),
      levels = c(
        "Democratic Party — Likes", "Republican Party — Likes",
        "Democratic Party — Dislikes", "Republican Party — Dislikes"
      )
    ),
    color = case_when(
      party == "Democratic" & sentiment.x == "Likes"    ~ "#2166ac",
      party == "Democratic" & sentiment.x == "Dislikes" ~ "#92c5de",
      party == "Republican" & sentiment.x == "Likes"    ~ "#d6604d",
      party == "Republican" & sentiment.x == "Dislikes" ~ "#f4a582"
    )
  ) |>
  ggplot(aes(label = word, size = n, color = color)) +
  geom_text_wordcloud(seed = 4273) +
  scale_color_identity() +
  scale_size_area(max_size = 10) +
  facet_wrap(~panel, nrow = 2) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))
