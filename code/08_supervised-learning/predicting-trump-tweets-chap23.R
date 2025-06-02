# predicting Trump vs. staff tweets (chapter 23)

library(tidyverse)
library(tidytext)
library(tidymodels)


# training set from David Robinson
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))


# clean up
tweets <- trump_tweets_df |>
  select(.id = id,
         .source = statusSource,
         .text = text,
         .created = created) |>
  extract(.source, '.source', "Twitter for (.*?)<") |>
  filter(.source %in% c('iPhone', 'Android')) |>
  mutate(.source = factor(.source)) |>
  # remove multibyte (non-ASCII) characters
  mutate(.text = str_remove_all(.text,
                                "[^\x01-\x7F]"))

# (notice that I'm putting a dot in front of all these
# column names, on the off chance that words like "source"
# or "id" appear in the corpus)

library(lubridate)

tweets |>
  count(.source, hour = hour(with_tz(.created, "EST"))) |>
  mutate(percent = n / sum(n)) |>
  ggplot(aes(hour, percent, color = .source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "") +
  theme_minimal()



# pick the words to keep as predictors
words_to_keep <- tweets |>
  unnest_tokens(input = '.text',
                output = 'word') |>
  count(word) |>
  # remove numerals, URLs
  filter(str_detect(word, '.co|.com|.net|.edu|.gov|http', negate = TRUE)) |>
  filter(str_detect(word, '[0-9]', negate = TRUE)) |>
  # remove rare words
  filter(n > 2) |>
  pull(word)

# tokenize
tidy_tweets <- tweets |>
  unnest_tokens(input = '.text',
                output = 'word') |>
  filter(word %in% words_to_keep) |>
  count(.id, word) |>
  bind_tf_idf(term = 'word',
              document = '.id',
              n = 'n') |>
  select(.id, word, tf) |>
  pivot_wider(id_cols = '.id',
              names_from = 'word',
              values_from = 'tf',
              values_fill = 0)

tidy_tweets <- tweets |>
  select(.id, .source, .created) |>
  right_join(tidy_tweets, by = '.id')


# split into train and test sets
set.seed(42)
tweet_split <- initial_split(tidy_tweets,
                             prop = 0.8)

train <- training(tweet_split)
test <- testing(tweet_split)


## quick logistic regression primer
train$y <- as.numeric(train$.source == 'Android')

ggplot(data = train,
       mapping = aes(x=makeamericagreatagain,
                     y=y)) +
  geom_point() +
  geom_smooth(method = "lm")
# this is bad. it goes too far below zero

ggplot(data = train,
       mapping = aes(x=makeamericagreatagain,
                     y=y)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = FALSE)
# use a model that restricts the probability between zero and one.

# remove the 'y' variable
train <- train |>
  select(-y)


# fit three logistic regression models:
# underfit, overfit, and regularized

# underfit model

model1 <- logistic_reg() |>
  fit(formula = .source ~ crooked + dumb + emails +
        crowds + hillary + winning + weak,
      data = train)

tidy(model1)

# in-sample fit
train |>
  bind_cols(predict(model1, train)) |>
  accuracy(truth = .source, estimate = .pred_class)


# out-of-sample fit
test |>
  bind_cols(predict(model1, test)) |>
  accuracy(truth = .source, estimate = .pred_class)

test |>
  bind_cols(predict(model1, test)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')


# overfit
model2 <- logistic_reg() |>
  fit(formula = .source ~ .,
      data = train |>
        select(-.id, -.created))

tidy(model2)

# in-sample fit
train |>
  bind_cols(predict(model2, train)) |>
  accuracy(truth = .source, estimate = .pred_class)

train |>
  bind_cols(predict(model2, train)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')

# out-of-sample fit
test |>
  bind_cols(predict(model2, test)) |>
  accuracy(truth = .source, estimate = .pred_class)

test |>
  bind_cols(predict(model2, test)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')

# does just as bad as the underfit model

# fit a regularized model (LASSO)
model3 <- logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = .source ~ .,
      data = train |>
        select(-.id, -.created))


tidy(model3)

tidy(model3) |>
  filter(estimate != 0) |>
  View()


# in-sample fit
train |>
  bind_cols(predict(model3, train)) |>
  accuracy(truth = .source, estimate = .pred_class)

# out-of-sample fit
test |>
  bind_cols(predict(model3, test)) |>
  accuracy(truth = .source, estimate = .pred_class)

test |>
  bind_cols(predict(model3, test)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')

# probability density plot
test |>
  bind_cols(predict(model3, test, type = 'prob')) |>
  ggplot(mapping = aes(x=.pred_Android, fill = .source)) +
  geom_density(alpha = 0.6) +
  labs(x = 'Predicted Probability Tweet Came From Trump')

## Out-of-sample prediction on the tweets from web client ----------------------


tweets <- trump_tweets_df |>
  select(.id = id,
         .source = statusSource,
         .text = text,
         .created = created) |>
  extract(.source, '.source', "Twitter for (.*?)<") |>
  filter(is.na(.source))


# tokenize
pc_tweets <- tweets |>
  unnest_tokens(input = '.text',
                output = 'word') |>
  mutate(word = factor(word, levels = words_to_keep)) |>
  filter(word %in% words_to_keep) |>
  count(.id, word, .drop = FALSE) |>
  bind_tf_idf(term = 'word',
              document = '.id',
              n = 'n') |>
  select(.id, word, tf) |>
  pivot_wider(id_cols = '.id',
              names_from = 'word',
              values_from = 'tf',
              values_fill = 0)

# make predictions
pc_tweets |>
  bind_cols(predict(model3, pc_tweets, type = 'prob')) |>
  ggplot(mapping = aes(x=.pred_Android)) +
  geom_histogram(color = 'black') +
  labs(x = 'Prob(Trump)')
# seems like the web client tweets came from both Trump and staffers









# cross-validation: ------------

# first, assign each observation in train to a "fold"
folds <- vfold_cv(train |>
                    select(-.id, -.created),
                  v = 10)
folds

# now, fit the model on each training fold, assessing performance
# on the out-of-sample fold
lasso_specification <-
  logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet')

lasso_cv <- workflow() |>
  add_model(lasso_specification) |>
  add_formula(.source ~ .) |>
  fit_resamples(folds)

collect_metrics(lasso_cv)

## Tune the hyperparameters through cross validation -----

lasso_specification <- logistic_reg(mode = 'classification',
                                    penalty = tune(),
                                    mixture = 1) |>
  set_engine('glmnet')

penalty_grid <- grid_regular(penalty(), levels = 10)

lasso_workflow <-
  workflow() |>
  add_model(lasso_specification) |>
  add_formula(.source ~ .)

lasso_tune <- tune_grid(
  lasso_workflow,
  folds,
  grid = penalty_grid
)

collect_metrics(lasso_tune)

autoplot(lasso_tune) +
  labs(
    title = "LASSO performance across regularization penalties",
  )

best_penalty <- lasso_tune |>
  select_best(metric = 'accuracy') |>
  pull(penalty)

# fit that best model
model4 <- logistic_reg(penalty = best_penalty, mixture = 1) |>
  set_engine('glmnet') |>
  fit(formula = .source ~ .,
      data = train |>
        select(-.id, -.created))


# nonzero coefficients
model4 |> tidy() |> filter(estimate != 0) |> nrow()


# in-sample fit
train |>
  bind_cols(predict(model4, train)) |>
  accuracy(truth = .source, estimate = .pred_class)

# out-of-sample fit
test |>
  bind_cols(predict(model4, test)) |>
  accuracy(truth = .source, estimate = .pred_class)

test |>
  bind_cols(predict(model4, test)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')



## Random Forest ---------------------------

rf_specification <-
  rand_forest(mode = 'classification', mtry = 25, min_n = 10, trees = 1001) |>
  set_engine('ranger')

rf_cv <- workflow() |>
  add_model(rf_specification) |>
  add_formula(.source ~ .) |>
  fit_resamples(folds)

collect_metrics(rf_cv)


rf_specification <- rand_forest(mode = 'classification',
                                mtry = tune(),
                                trees = 1001,
                                min_n = 10)


rf_grid <- grid_regular(mtry(range = c(5, 100)),
                        levels = 5)
rf_grid

rf_workflow <- workflow() |>
  add_model(rf_specification) |>
  add_formula(.source ~ .)

rf_tune <- tune_grid(
  rf_workflow,
  folds,
  grid = rf_grid
)

collect_metrics(rf_tune)

autoplot(rf_tune) +
  labs(
    title = "Random forest performance across regularization penalties",
  )











## tesing naive Bayes -----------------

library(naivebayes)
nb <- naive_bayes(
  formula = .source ~ .,
  data = train |>
    select(-.id, -.created),
  Laplace = 0.1)

summary(nb)

# in-sample fit
train |>
  bind_cols(.pred_class = predict(nb, train)) |>
  accuracy(truth = .source, estimate = .pred_class)

# out-of-sample fit
test |>
  bind_cols(.pred_class = predict(nb, test)) |>
  accuracy(truth = .source, estimate = .pred_class)


# Further Reading:
# http://varianceexplained.org/r/trump-tweets/


## Document Embeddings as predictors ------------------------------

library(fuzzylink)
embeddings <- get_embeddings(tweets$.text)
colnames(embeddings) <- paste0('dim', 1:256)

tweets <- bind_cols(tweets, embeddings)

# split into train and test sets
set.seed(42)
tweet_split <- initial_split(tweets,
                             prop = 0.8)

train <- training(tweet_split)
test <- testing(tweet_split)

model4 <- logistic_reg(penalty = 0.01, mixture = 1) |>
  set_engine('glmnet') |>
  fit(.source ~ .,
      data = train |>
        select(-.id, -.text, -.created))

tidy(model4)
tidy(model4) |> filter(estimate != 0) |> nrow()

# in-sample fit
train |>
  bind_cols(predict(model4, train)) |>
  accuracy(truth = .source, estimate = .pred_class)

# out-of-sample fit
test |>
  bind_cols(predict(model4, test)) |>
  accuracy(truth = .source, estimate = .pred_class)

test |>
  bind_cols(predict(model4, test)) |>
  conf_mat(truth = .source, estimate = .pred_class) |>
  autoplot(type = 'heatmap')
