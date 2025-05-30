#' ---
#'
#' title: Press release clustering
#'
#' ---

library(tidyverse)
library(tidytext)
library(SnowballC)

set.seed(42) # set a random seed so that we get the same results from run to run

## Step 1: Load in the 558 press releases -----------------------

files_in_folder <- list.files('data/press-releases/Lautenberg/')

# create an empty dataframe to fill up
df <- tibble(
  id = 1:length(files_in_folder),
  date = rep(NA, length(files_in_folder)),
  text = rep(NA, length(files_in_folder))
  )

# loop through the text files and pull the text
for(i in 1:length(files_in_folder)){

  # pull the date
  df$date[i] <- files_in_folder[i] |>
    str_sub(1, 9)

  # pull the text
  df$text[i] <- read_file(
    paste0('data/press-releases/Lautenberg/',
           files_in_folder[i])
  )

}

# save the completed dataset
save(df, file = 'data/press-releases/lautenberg.RData')


## Step 2: Tidy up the text -------------------------------

tidy_press_releases <- df |>
  mutate(text = str_replace_all(text,
                                pattern = '     Senator Frank R  Lautenberg                                                                                                                      Press Release        of        Senator Lautenberg                                                                                ',
                                replacement = '')) |>
  # tokenize to the word level
  unnest_tokens(input = 'text',
                output = 'word') |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # remove numerals
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # create word stems
  mutate(word_stem = wordStem(word)) |>
  # remove "blank space" token
  filter(word_stem != '') |>
  # count up bag of word stems
  count(id, word_stem) |>
  # # remove the infrequent word stems
  # filter(n > 2) |>
  # remove the single character word stems (like "D" and "R")
  filter(nchar(word_stem) != 1) |>
  # compute tf-idf
  bind_tf_idf(term = 'word_stem',
              document = 'id',
              n = 'n') |>
  filter(!is.na(tf_idf))

# create document-term matrix
lautenberg_dtm <- cast_dtm(data = tidy_press_releases,
                           document = 'id',
                           term = 'word_stem',
                           value = 'tf')
lautenberg_dtm


## Step 3: K-means clustering -------------------------

km <- kmeans(x = lautenberg_dtm,
             centers = 6,
             nstart = 100)

table(km$cluster)


# function to find the words that are most overrepresented in the cluster mean for a given cluster
get_top_words <- function(centers, cluster_of_interest, n = 10){
  (centers[cluster_of_interest,] - colMeans(centers[-cluster_of_interest,])) |>
    sort(decreasing = TRUE) |>
    head(n)
}

# cluster 1
get_top_words(km$centers, 1)

# cluster 2
get_top_words(km$centers, 2)

# cluster 3
get_top_words(km$centers, 3)

# cluster 4
get_top_words(km$centers, 4)


## Step 4: Remerge with original dataset and see if our topic labels make sense ---------

cluster_assignments <- tibble(id = km$cluster |> names() |> as.numeric(),
                              cluster = km$cluster)

df <- df |>
  left_join(cluster_assignments,
            by = 'id')

# this one should be about partisan taunting
df |>
  filter(cluster == 1) |>
  slice_sample(n = 1) |>
  pull(text)

# this one should be about legislation / Senate business
df |>
  filter(cluster == 2) |>
  slice_sample(n = 1) |>
  pull(text)

# this one should be "partisan taunting"
df |>
  filter(cluster == 3) |>
  slice_sample(n = 1) |>
  pull(text)


# this one should be credit claiming / New Jersey related
df |>
  filter(cluster == 4) |>
  slice_sample(n = 1) |>
  pull(text)


## Total within sum of squares is a good metric for how
## effectively your kmeans clustering explains variation
## A good rule of thumb when selecting k is to find the "elbow"
## where adding more clusters doesn't significantly reduce
## the residual sum of squared distances
k_values_to_try <- 1:9
ss <- rep(NA, length(k_values_to_try))
for(k in k_values_to_try){
  print(paste0('Trying k = ', k, '...'))
  km <- kmeans(x = lautenberg_dtm,
               centers = k,
               nstart = 100)
  ss[k] <- km$tot.withinss
}

ggplot(mapping = aes(x = k_values_to_try,
                     y = ss)) +
  geom_point() +
  geom_line() +
  labs(x = 'k', y = 'Residual Sum of Squares')


## Step 5: Let's try that again, but represent each document with an embedding from OpenAI ------------------

load('data/press-releases/lautenberg.RData')
library(fuzzylink)
openai_api_key("", install = TRUE)
embeddings <- get_embeddings(df$text)

set.seed(42)
km <- kmeans(embeddings,
             centers = 9,
             nstart = 100)

# assign the clusters to the original dataframe
df$cluster <- as.numeric(km$cluster)

# a function to get the most over-represented words by cluster
get_top_words <- function(df, cluster_of_interest, num_words = 10){
  df |>
    mutate(in_cluster = if_else(cluster == cluster_of_interest,
                                'within_cluster', 'outside_cluster')) |>
    unnest_tokens(output = 'word',
                  input = 'text') |>
    # remove stop words
    anti_join(get_stopwords()) |>
    # remove numerals
    filter(str_detect(word, '[0-9]', negate = TRUE)) |>
    # create word stems
    mutate(word_stem = wordStem(word)) |>
    # remove "blank space" token
    filter(word_stem != '') |>
    # count the words in each cluster
    count(in_cluster, word_stem) |>
    pivot_wider(names_from = 'in_cluster',
                values_from = 'n',
                values_fill = 0) |>
    # compute word shares
    mutate(within_cluster = within_cluster / sum(within_cluster),
           outside_cluster = outside_cluster / sum(outside_cluster)) |>
    mutate(delta = within_cluster - outside_cluster) |>
    arrange(-delta) |>
    head(num_words) |>
    pull(word_stem)
}

get_top_words(df, 1) # trains / ports / transportation
get_top_words(df, 2) # the military / foreign affairs
get_top_words(df, 3) # terrorism?
get_top_words(df, 4) # NFL and also courts? and also New Jersey?
get_top_words(df, 5) # healthcare and health insurance
get_top_words(df, 6) # environmental protection
get_top_words(df, 7) # credit claiming for federal funding in New Jersey
get_top_words(df, 8) # tobacco and firearms; public safety
get_top_words(df, 9) # taunting President Bush

# validation exercise by drawing random press releases
# and making sure that they seem to fit with our category labels
df |>
  filter(cluster == 2) |>
  slice_sample(n = 1) |>
  pull(text)
