library(tidyverse)
library(tidytext)
library(SnowballC)

load("data/congressional-floor-speeches/train_set.RData")
load("data/congressional-floor-speeches/test_set.RData")

## Preprocess the texts -------------------

# weirdly, there are a number of repeated observations
# in the dataset....
d <- unique(d)

tidy_speeches <- d |>
  # remove opening line
  mutate(speech = str_remove_all(speech, 'Mr. Speaker, I rise today')) |>
  unnest_tokens(input = 'speech',
                output = 'word') |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # remove numbers
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # stem
  mutate(word_stem = wordStem(word))

# look for words that help distinguish between Democrats
# and Republicans
dtm <- tidy_speeches |>
  count(party,word_stem) |>
  pivot_wider(names_from = 'party', values_from = 'n',
              values_fill = 0) |>
  filter(word_stem != '') |>
  filter(R + D > 50) |>
  mutate(ratio = (D+0.1)/(R+0.1)) |>
  filter(ratio < 0.5 | ratio > 2)

vocabulary <- dtm$word_stem

# function to predict whether a speech is Democratic or Republican
predict_party <- function(doc, vocabulary, method = 'multinomial'){

  # filter our dataset to just include those vocabulary
  # words
  df <- tidy_speeches |>
    filter(word_stem %in% vocabulary) |>
    mutate(word_stem = factor(word_stem))

  # create two bags of words, one per party
  # (not including the speech we're trying to predict!)
  democratic_bag <- table(df$word_stem[df$party == 'D' &
                                  df$id != doc])
  republican_bag <-  table(df$word_stem[df$party == 'R' &
                                          df$id != doc])

  # create bag of words for speech whose authorship
  # we're trying to predict
  doc_bag <- table(df$word_stem[df$id == doc])

  if(sum(doc_bag) == 0){
    return(NA)
  }

  if(method == 'multinomial'){
    # Laplace Smoothed Likelihoods
    dem_likelihood <- dmultinom(doc_bag,
                                prob = democratic_bag + 0.1)

    rep_likelihood <- dmultinom(doc_bag,
                                prob = republican_bag + 0.1)

    if(rep_likelihood > dem_likelihood){
      return('R')
    } else{
      return('D')
    }
  } else if (method == 'cosine'){
    cosine_similarity <- function(vec1, vec2){
      sum(vec1 * vec2) /
        sqrt(sum(vec1^2)) / sqrt(sum(vec2^2))
    }

    dem_sim <- cosine_similarity(democratic_bag, doc_bag)
    rep_sim <- cosine_similarity(republican_bag, doc_bag)

    if(dem_sim > rep_sim){
      return('D')
    } else{
      return('R')
    }
  } else{
    stop('For the method argument, choose either "multinomial" or "cosine".')
  }




}

predict_party(d$id[1], vocabulary)
predict_party(d$id[2], vocabulary)

d$prediction <- NA
for(i in 1:nrow(d)){
  print(i)
  d$prediction[i] <- predict_party(d$id[i],
                                   vocabulary)
}

table(d$party, d$prediction)
# accuracy
mean(d$party == d$prediction, na.rm = TRUE)


## Predict the test set ------------

load('data/congressional-floor-speeches/truth.RData')

# tidy up the test set
tidy_speeches <- truth |>
  # remove opening line
  mutate(speech = str_remove_all(speech, 'Mr. Speaker, I rise today')) |>
  unnest_tokens(input = 'speech',
                output = 'word') |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # remove numbers
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # stem
  mutate(word_stem = wordStem(word))

truth$prediction <- NA
for(i in 1:nrow(truth)){
  print(i)
  truth$prediction[i] <- predict_party(truth$id[i],
                                       vocabulary)
}

table(truth$party, truth$prediction)
# accuracy
mean(truth$party == truth$prediction, na.rm = TRUE)
