library(tidyverse)
library(tidytext)
library(tidymodels)
library(lubridate)
library(fuzzylink)

load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
tweets <- trump_tweets_df |>
  select(.id = id,
         .source = statusSource,
         .text = text,
         .created = created) |>
  extract(.source, '.source', "Twitter for (.*?)<") |>
  filter(.source %in% c('iPhone', 'Android')) |>
  mutate(.source = factor(.source))



clean_text <- iconv(tweets$.text, from = "UTF-8", to = "UTF-8", sub = "")
emb <- get_embeddings(clean_text)

# ------- Attach embeddings ---------------------------------------------------
tweets_emb <- bind_cols(
  tweets,
  as_tibble(emb)
)

# ------- Split ---------------------------------------------------------------
set.seed(6271)
tweet_split <- initial_split(tweets_emb, prop = 0.8, strata = .source)
train <- training(tweet_split)
test  <- testing(tweet_split)

# ------- Recipe --------------------------------------------------------------
# Embedding columns are already numeric; demote metadata cols, normalize embeddings
rec <- recipe(.source ~ ., data = train) |>
  update_role(.id, .text, .created, new_role = "id") |>
  step_normalize(all_numeric_predictors())

# ------- Model spec: LASSO logistic regression -------------------------------
lr_spec <- logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("classification")

# ------- Workflow ------------------------------------------------------------
wf <- workflow() |>
  add_recipe(rec) |>
  add_model(lr_spec)

# ------- Tune penalty with 5-fold CV -----------------------------------------
set.seed(3847)
folds <- vfold_cv(train, v = 5, strata = .source)

metrics <- metric_set(roc_auc, accuracy)

tune_res <- tune_grid(
  wf,
  resamples = folds,
  grid = 20,
  metrics = metrics
)

autoplot(tune_res)
show_best(tune_res, metric = "roc_auc")

# ------- Finalize and evaluate on test set -----------------------------------
best_penalty <- select_best(tune_res, metric = "roc_auc")

final_wf <- finalize_workflow(wf, best_penalty)

final_fit <- last_fit(final_wf, tweet_split, metrics = metrics)
collect_metrics(final_fit)

test_pred <- collect_predictions(final_fit) |>
  bind_cols(test$.text)

collect_predictions(final_fit) |>
  conf_mat(truth = .source, estimate = .pred_class)
