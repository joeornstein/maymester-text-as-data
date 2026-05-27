library(tidyverse)
library(tidytext)
library(tidymodels)
library(lubridate)
library(fuzzylink)

# Same data loading and cleaning as in predicting-trump-tweets-embeddings.R.
# We keep this self-contained so the script can be run independently.
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
# We use the same seed as the LASSO script so that the train/test split is
# identical --- this is essential for a fair comparison between models.
set.seed(6271)
tweet_split <- initial_split(tweets_emb, prop = 0.8, strata = .source)
train <- training(tweet_split)
test  <- testing(tweet_split)

# ------- Recipe --------------------------------------------------------------
# The recipe is identical to the LASSO script. Random forests don't require
# normalization or any other preprocessing for numeric predictors, so there's
# nothing extra to add.
rec <- recipe(.source ~ ., data = train) |>
  update_role(.id, .text, .created, new_role = "id")

# ------- Model spec: random forest -------------------------------------------
# rand_forest() sets up a random forest. We tune two key hyperparameters:
#   mtry   -- number of predictors randomly sampled at each split. Lower values
#              increase diversity among trees; higher values make each tree
#              stronger but more correlated with its neighbors.
#   min_n  -- minimum number of observations required to split a node. Higher
#              values produce shallower, less overfit trees.
# trees = 500 is a fixed, generous number; more trees rarely hurts but slows
# fitting. set_engine("ranger") uses the fast ranger package under the hood.
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
  set_engine("ranger") |>
  set_mode("classification")

# ------- Workflow ------------------------------------------------------------
wf <- workflow() |>
  add_recipe(rec) |>
  add_model(rf_spec)

# ------- Tune hyperparameters with 5-fold CV ---------------------------------
# Same folds seed as the LASSO script for a consistent comparison.
# We use a Latin hypercube grid (grid_latin_hypercube) instead of a random
# grid: it spaces the 20 candidate combinations evenly across the parameter
# space, so we get better coverage with the same number of fits.
set.seed(3847)
folds <- vfold_cv(train, v = 5, strata = .source)

metrics <- metric_set(roc_auc, accuracy)

tune_res <- tune_grid(
  wf,
  resamples = folds,
  grid = grid_latin_hypercube(
    mtry(range = c(5, 50)),
    min_n(),
    size = 20
  ),
  metrics = metrics,
  control = control_grid(save_pred = TRUE)  # needed to plot the ROC curve later
)

autoplot(tune_res)
show_best(tune_res, metric = "roc_auc")

# ------- ROC curve (cross-validation) ----------------------------------------
# As in the LASSO script, we plot the CV ROC curve at the best hyperparameter
# combination. Compare this curve to the one from the LASSO script to get a
# visual sense of which model separates the two classes more effectively.
collect_predictions(tune_res, parameters = select_best(tune_res, metric = "roc_auc")) |>
  roc_curve(truth = .source, .pred_Android) |>
  autoplot() +
  labs(x = "False Positive Rate", y = "True Positive Rate")

# ------- Finalize and evaluate on test set -----------------------------------
best_params <- select_best(tune_res, metric = "roc_auc")

final_wf <- finalize_workflow(wf, best_params)

# last_fit() retrains on the full training set and evaluates once on the test
# set. Compare collect_metrics() here to the LASSO test-set results.
final_fit <- last_fit(final_wf, tweet_split, metrics = metrics)
collect_metrics(final_fit)

# Bind the raw tweet text back on so we can inspect misclassified tweets.
test_pred <- collect_predictions(final_fit) |>
  bind_cols(test$.text)

# Confusion matrix for the random forest.
collect_predictions(final_fit) |>
  conf_mat(truth = .source, estimate = .pred_class)
