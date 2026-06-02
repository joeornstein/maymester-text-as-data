#' -----
#'
#' title: Text as Outcome
#' reference: Egami et al. (2022) "How To Make Causal Inferences Using Text"
#'
#' ---

library(tidyverse)
library(tidytext)
library(tidymodels)
library(fuzzylink)
library(glue)

# In this experiment, the authors ask survey respondents
# what they think should be done about a person who entered
# the United States illegally. The "treatment" is whether
# the experimenters tell the respondent that the subject
# has a criminal history.

# Research question: how does the treatment affect
# respondents' preferences?


## Step 1: split the data into a set for measurement, and a set for estimation --------------

# choosing how to measure "respondent preferences"
# inevitably will require an iterative process of
# discovery and codebook refinement. The danger is that
# our choices at this stage will be subtly influenced by
# our knowledge of treatment assignment, and we might bias
# ourselves into finding an effect that isn't there!

# The principled solution to this problem is to split
# the data into a "measurement set", which we can explore
# at our leisure to create our codebook, and an "estimation
# set", which we only look at once we've settled on the
# codebook (the authors call this the "g function"),
# and use to estimate our causal effect.

d <- read_csv('data/egami-2018/Experiment2.csv') |>
  filter(!is.na(text)) |>
  # remove some garbled text
  mutate(text = iconv(text, from = "latin1", to = "ASCII", sub = ""))

set.seed(42)

d_split <- initial_split(data = d,
                         prop = 0.5,
                         strata = 'treat')

measurement_set <- training(d_split)

estimation_set <- testing(d_split)


## Step 2: Estimate the "g function" -------------------

# we'll assign categories to responses based on
# k-means clustering of document embeddings
embedding_path <- "data/egami-2018/embeddings.RData"
if(!file.exists(embedding_path)){
  emb_measurement_set <- get_embeddings(measurement_set$text)
  emb_estimation_set <- get_embeddings(estimation_set$text)
  save(emb_measurement_set, emb_estimation_set, file = embedding_path)
} else{
  load(embedding_path)
}

# k-means clustering; they use 11 in the paper,
# so we'll copy them
set.seed(1067569437)
km <- kmeans(emb_measurement_set,
             centers = 11)
measurement_set$cluster <- km$cluster

# label based on prototypical response by cluster
dists <- sqrt(rowSums((emb_measurement_set - km$centers[km$cluster, ])^2))

measurement_set |>
  mutate(
    dist_to_center = dists
  ) |>
  slice_min(dist_to_center, n = 3, by = cluster) |>
  select(cluster, text) |>
  arrange(cluster) |>
  View()


# after reviewing, assign topic labels here
cluster_labels <- c(
  "1"  = "Uncertain, Nonsensical, or Vaguely Positive",
  "2"  = "Path to Citizenship",
  "3"  = "Straight to Jail",
  "4"  = "No Prison",
  "5"  = "Prison And/Or Deported",
  "6"  = "Deported",
  "7"  = "Concerns about costs",
  "8"  = "Deported",
  "9"  = "Punish to full extent of the law",
  "10" = "Deported",
  "11" = "Repeat Offender, Danger To Society"
)


# draw a few at random to ensure face validity
measurement_set$cluster_label <- cluster_labels[as.character(measurement_set$cluster)]
set.seed(602185)
measurement_set |>
  mutate(to_print = glue('{cluster_label}: {text}\n\n')) |>
  group_by(cluster) |>
  slice_sample(n = 2) |>
  pull(to_print)


## Step 3: Assign labels to estimation set ----------

# Perform this step *only* once you are satisfied with the labeling
# procedure you developed with the measurement set. It's pefectly
# okay to iterate while developing the codebook, but not once
# you start estimating causal effects. That would be "p-hacking".

# In our case, we'll assign labels based on the closest
# cluster center
dist_to_centers <- apply(km$centers, 1, function(center) {
  sqrt(rowSums(sweep(emb_estimation_set, 2, center, "-")^2))
})

estimation_set <- estimation_set |>
  mutate(cluster_label = cluster_labels[apply(dist_to_centers, 1, which.min)])

## *now* we can estimate average treatment effects

library(nnet)
library(marginaleffects)

fit <- multinom(cluster_label ~ treat, data = estimation_set, trace = FALSE)

avg_comparisons(fit, variables = "treat") |>
  as_tibble() |>
  ggplot(aes(x = estimate, y = reorder(group, estimate))) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Average treatment effect (probability)", y = NULL,
       title = "Effect of treatment on response category probability")
