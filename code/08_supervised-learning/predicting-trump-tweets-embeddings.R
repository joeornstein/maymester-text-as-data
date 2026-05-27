library(tidyverse)
library(tidytext)
library(tidymodels)
library(lubridate)
library(fuzzylink)

# Load Trump tweet data hosted online and keep only the columns we need.
# We also extract the device (iPhone vs Android) from the raw HTML source
# string and convert it to a factor --- this will be our outcome variable.
load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))
tweets <- trump_tweets_df |>
  select(.id = id,
         .source = statusSource,
         .text = text,
         .created = created) |>
  extract(.source, '.source', "Twitter for (.*?)<") |>
  filter(.source %in% c('iPhone', 'Android')) |>
  mutate(.source = factor(.source))

# get_embeddings() (from fuzzylink) sends each tweet to an embedding model and
# returns a numeric matrix: one row per tweet, one column per embedding
# dimension. We strip non-UTF-8 characters first to avoid encoding errors.
clean_text <- iconv(tweets$.text, from = "UTF-8", to = "UTF-8", sub = "")
emb <- get_embeddings(clean_text)

# ------- Attach embeddings ---------------------------------------------------
# Combine the original tweet metadata with the embedding columns so that
# tidymodels can treat the whole thing as a single data frame.
tweets_emb <- bind_cols(
  tweets,
  as_tibble(emb)
)

# ------- Split ---------------------------------------------------------------
# initial_split() creates a single train/test partition. strata = .source
# ensures both splits have the same iPhone/Android ratio (stratified sampling).
set.seed(6271)
tweet_split <- initial_split(tweets_emb, prop = 0.8, strata = .source)
train <- training(tweet_split)
test  <- testing(tweet_split)

# ------- Recipe --------------------------------------------------------------
# A recipe defines the preprocessing steps. The formula .source ~ . means
# "predict .source from all other columns". update_role() marks the three
# metadata columns as "id" variables so they are kept in the data but never
# used as predictors. The embedding columns (V1, V2, ...) are already numeric,
# so no further transformations are needed here.
rec <- recipe(.source ~ ., data = train) |>
  update_role(.id, .text, .created, new_role = "id")

# ------- Model spec: LASSO logistic regression -------------------------------
# logistic_reg() specifies the model family. Setting mixture = 1 gives a pure
# LASSO penalty (vs. mixture = 0 for ridge, values in between for elastic net).
# penalty = tune() tells tidymodels to treat the regularization strength as a
# hyperparameter to be selected by cross-validation rather than fixed by us.
# set_engine("glmnet") names the R package that will fit the model.
lr_spec <- logistic_reg(penalty = tune(), mixture = 1) |>
  set_engine("glmnet") |>
  set_mode("classification")

# ------- Workflow ------------------------------------------------------------
# A workflow bundles the recipe and the model spec together. This makes it easy
# to pass the whole pipeline into tuning and final-fit functions as a unit.
wf <- workflow() |>
  add_recipe(rec) |>
  add_model(lr_spec)

# ------- Tune penalty with 5-fold CV -----------------------------------------
# vfold_cv() splits the training data into 5 folds. tune_grid() then fits the
# workflow on each fold for each of 20 candidate penalty values (sampled
# automatically from a log-uniform grid), and evaluates with ROC-AUC and
# accuracy. autoplot() and show_best() help us inspect the results.
set.seed(3847)
folds <- vfold_cv(train, v = 5, strata = .source)

metrics <- metric_set(roc_auc, accuracy)

tune_res <- tune_grid(
  wf,
  resamples = folds,
  grid = 20,       # number of penalty values to try
  metrics = metrics,
  control = control_grid(save_pred = TRUE)  # needed to plot the ROC curve later
)

autoplot(tune_res)
show_best(tune_res, metric = "roc_auc")

# ------- ROC curve (cross-validation) ----------------------------------------
# The ROC curve plots the true positive rate against the false
# positive rate  at every possible classification threshold.
# A perfect classifier hugs the top-left corner; a random classifier follows
# the diagonal dashed line. The area under this curve is the ROC-AUC reported
# above --- so this plot gives intuition for what that single number summarizes.
#
# collect_predictions() here retrieves the held-out fold predictions from
# cross-validation at the best penalty value. This is still "training data"
# (the test set hasn't been touched yet), but because these predictions came
# from held-out folds, they give an honest estimate of generalization.
collect_predictions(tune_res, parameters = select_best(tune_res, metric = "roc_auc")) |>
  roc_curve(truth = .source, .pred_Android) |>
  autoplot() +
  labs(x = "False Positive Rate", y = "True Positive Rate")

# ------- Finalize and evaluate on test set -----------------------------------
# select_best() picks the penalty value with the highest mean ROC-AUC across
# folds. finalize_workflow() plugs that value back into the workflow so that
# penalty is no longer marked as tune().
best_penalty <- select_best(tune_res, metric = "roc_auc")

final_wf <- finalize_workflow(wf, best_penalty)

# last_fit() re-trains the finalized workflow on the *entire* training set and
# then evaluates it once on the held-out test set --- the only time we touch
# the test data. collect_metrics() returns the test-set ROC-AUC and accuracy.
final_fit <- last_fit(final_wf, tweet_split, metrics = metrics)
collect_metrics(final_fit)

# collect_predictions() returns the test-set predictions (predicted class and
# class probabilities). We bind on the raw tweet text so we can inspect which
# tweets were misclassified.
test_pred <- collect_predictions(final_fit) |>
  bind_cols(test$.text)

# conf_mat() shows the confusion matrix: rows = true label, columns = predicted
# label. Off-diagonal counts are misclassifications.
collect_predictions(final_fit) |>
  conf_mat(truth = .source, estimate = .pred_class)
