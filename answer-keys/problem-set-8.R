library(tidyverse)
library(tidytext)
library(tidymodels)
library(SnowballC)

load("data/congressional-floor-speeches/train_set.RData")
load("data/congressional-floor-speeches/test_set.RData")

## Preprocess the texts -------------------

# weirdly, there are a number of repeated observations
# in the dataset....
d <- unique(d) |>
  select(.id = id, .speech = speech, .party = party)

# convert to a document-term matrix
speeches_dtm <- d |>
  # remove opening line
  mutate(.speech = str_remove_all(.speech, 'Mr. Speaker, I rise today')) |>
  unnest_tokens(input = '.speech',
                output = 'word') |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # remove numbers
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # stem
  mutate(word_stem = wordStem(word)) |>
  count(.id, word_stem) |>
  filter(n > 2) |>
  bind_tf_idf(term = 'word_stem',
              document = '.id',
              n = 'n') |>
  select(.id, word_stem, tf) |>
  filter(word_stem != '') |>
  pivot_wider(id_cols = '.id',
              names_from = 'word_stem',
              values_from = 'tf',
              values_fill = 0) |>
  right_join(d) |>
  select(-.speech, -.id) |>
  mutate(.party = factor(.party))


# split into a training and test set
speech_split <- initial_split(speeches_dtm, prop = 0.8)
train <- training(speech_split)
test <- testing(speech_split)

# fit regularized logistic regression
mod <- logistic_reg(penalty = 0.003, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = .party ~ .,
      data = train)


tidy(mod)

tidy(mod) |>
  filter(estimate != 0) |>
  nrow()

# in-sample accuracy
train |>
  bind_cols(predict(mod, train)) |>
  accuracy(truth = .party, estimate = .pred_class)

# out-of-sample fit
test <- testing(speech_split)
test |>
  bind_cols(predict(mod, test)) |>
  accuracy(truth = .party, estimate = .pred_class)

test |>
  bind_cols(predict(mod, test)) |>
  conf_mat(truth = .party, estimate = .pred_class) |>
  autoplot(type = 'heatmap')

test <- test |>
  bind_cols(predict(mod, test, type = 'prob'))

ggplot(data = test,
       mapping = aes(x=.pred_D,
                     y=as.numeric(.party == 'D'))) +
  geom_jitter() +
  geom_smooth() +
  labs(x = 'Predicted Probability Democratic',
       y = 'Democrat = 1') +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')

ggplot(data = test,
       mapping = aes(x=.pred_D,
                     fill=.party)) +
  geom_density(alpha = 0.6) +
  labs(x = 'Predicted Probability Democratic',
       fill = 'Party',
       title = 'Test Set') +
  theme_bw() +
  scale_fill_manual(values = c('blue', 'red'))

## Try with document embeddings ------------------------------

library(fuzzylink)

# remove the very longest speeches
d <- d |>
  filter(nchar(.speech) < 1e4)

from_file <- TRUE
if(!from_file){
  embeddings <- get_embeddings(d$.speech)
  colnames(embeddings) <- paste0('dim', 1:ncol(embeddings))
  d <- bind_cols(d, embeddings) |>
    mutate(.party = factor(.party))
  save(embeddings, d, file = 'answer-keys/ps8-embeddings.RData')
} else{
  load('answer-keys/ps8-embeddings.RData')
}

speech_split <- initial_split(d, prop = 0.8)
train <- training(speech_split) |> select(-.id, -.speech)

# fit regularized logistic regression
mod <- logistic_reg(penalty = 0.001, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = .party ~ .,
      data = train)

tidy(mod)

tidy(mod) |>
  filter(estimate != 0) |>
  nrow()

# in-sample accuracy
train |>
  bind_cols(predict(mod, train)) |>
  accuracy(truth = .party, estimate = .pred_class)

# out-of-sample fit
test <- testing(speech_split)
test |>
  bind_cols(predict(mod, test)) |>
  accuracy(truth = .party, estimate = .pred_class)

test |>
  bind_cols(predict(mod, test)) |>
  conf_mat(truth = .party, estimate = .pred_class) |>
  autoplot(type = 'heatmap')

test <- test |>
  bind_cols(predict(mod, test, type = 'prob'))

ggplot(data = test,
       mapping = aes(x=.pred_D,
                     y=as.numeric(.party == 'D'))) +
  geom_jitter() +
  geom_smooth() +
  labs(x = 'Predicted Probability Democratic',
       y = 'Democrat = 1') +
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed')

ggplot(data = test,
       mapping = aes(x=.pred_D,
                     fill=.party)) +
  geom_density(alpha = 0.6) +
  labs(x = 'Predicted Probability Democratic',
       fill = 'Party',
       title = 'Test Set') +
  theme_bw() +
  scale_fill_manual(values = c('blue', 'red'))


# pull a speech that the model thought was definitely a Democrat,
# but turned out to be a Republican
test |>
  filter(.pred_D > 0.9, .party == 'R') |>
  pull(.speech)



## Fit random forest ------------------

# first, assign each observation in train to a cross-validation fold
folds <- vfold_cv(train,
                  v = 10)
folds

rf_specification <- rand_forest(mode = 'classification',
                                mtry = tune(),
                                trees = 1001,
                                min_n = 10)


rf_grid <- grid_regular(mtry(range = c(5, 100)),
                        levels = 5)
rf_grid

rf_workflow <- workflow() |>
  add_model(rf_specification) |>
  add_formula(.party ~ .)

rf_tune <- tune_grid(
  rf_workflow,
  folds,
  grid = rf_grid
)

collect_metrics(rf_tune)
