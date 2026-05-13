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



