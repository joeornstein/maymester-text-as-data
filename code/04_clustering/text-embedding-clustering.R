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

set.seed(4515)
km <- kmeans(federalist_embeddings,
             centers = 10)

df$cluster_assignment <- km$cluster
# looks sensible!

cluster_labels <- tribble(~cluster_assignment, ~cluster,
                          1, 'The Executive',
                          2, 'Utility of the Union (Revenue & Commerce)',
                          3, 'Congress',
                          4, 'General Comments, Insufficiency of Articles of Confederation',
                          5, 'Common Defense / Militia',
                          6, 'Judiciary',
                          7, 'Apportionment',
                          8, 'Dangers of Dissension and Foreign Force',
                          9, 'Separation of Powers',
                          10, 'Taxation and Revenue')
df <- left_join(df, cluster_labels)
