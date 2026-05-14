## tokenize the Federalist papers
## and conduct a word frequency analysis
## to infer the authorship of the disputed papers

library(tidyverse)
library(tidytext)

papers <- read_csv('data/federalist-papers.csv')

# tokenize to the word level
df <- papers |>
  unnest_tokens(input = 'text',
                output = 'word')


count(df,author)

freqs <- df |>
  group_by(author) |>
  count(word) |>
  mutate(prop = n/ sum(n)) |>
  filter(author %in% c("James Madison", "Alexander Hamilton or James Madison", "Alexander Hamilton")) |>
  select(-n) |>
  pivot_wider(names_from = 'author',
              values_from = 'prop',
              values_fill = 0) |>
  rename(ham = `Alexander Hamilton`, mad = `James Madison`,
         disp = `Alexander Hamilton or James Madison`) |>
  filter(ham > 0.001 |
         disp > 0.001 |
         mad > 0.001) |>
  mutate(ham_ratio = (ham + 0.0001) / (mad + 0.0001),
         mad_ratio = (mad + 0.0001) / (ham + 0.0001))

freqs |> arrange(-ham_ratio)
freqs |> arrange(-mad_ratio)

