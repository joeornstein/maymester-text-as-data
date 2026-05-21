#' ---
#' Applying the kmeans clustering algorithm to
#' OpenAI text embeddings
#' ---

library(tidyverse)
library(fuzzylink)
library(readxl)

word <- read_xlsx('data/word-embeddings/three-clusters.xlsx') |>
  mutate(word = str_to_lower(word)) #|>
  # bind_rows(tribble(~cluster, ~word,
  #                   'football', 'Ravens',
  #                   'football', 'Falcons',
  #                   'football', 'Patriots',
  #                   'football', 'Baltimore Ravens football team',
  #                   'football', 'cowboys'))

# get embeddings
emb <- get_embeddings(word$word)

# assign them to three different clusters
km <- kmeans(emb, centers = 3)

word$cluster_assignment <- km$cluster


## Animal Clusters -----------------------

library(ellmer)
chat <- chat_claude()
chat$chat('Please give me a list of 200 animals, separated by commas.')
animals <- chat$get_turns()[[2]]

animals <- strsplit(animals@text, ", ")[[1]]

emb <- get_embeddings(animals)

km <- kmeans(emb, centers = 12)
df <- data.frame(
  name = animals,
  cluster_assignment = km$cluster)


## Try with longer documents --------------------

df <- read_csv('data/federalist-papers.csv')
load('data/federalist-embeddings.RData')

km <- kmeans(federalist_embeddings,
             centers = 10)

df$cluster_assignment <- km$cluster

