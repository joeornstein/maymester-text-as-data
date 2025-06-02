#' ---
#' K-means tutorial
#' ---

library(tidyverse)

# create a random set of data
df <- tibble(
  x = c(1,2,1,2,4,2, 2.5, 8, 6, 5,4, 7, 9, 6.7, 8, 9),
  y = c(2,1,2,2.5, 1,4.4, 1, 3, 10, 10, 7, 9, 6, 7, 8, 10)
)

ggplot(data = df,
       mapping = aes(x=x, y=y)) +
  geom_point(alpha = 0.7)

# kmeans is included in the base stats package
km <- kmeans(x = df,
             centers = 2)

km

# visualize the cluster assignments
df |>
  mutate(cluster_assignment = km$cluster) |>
  ggplot(mapping = aes(x=x, y=y, color = factor(cluster_assignment))) +
  geom_point() +
  theme_minimal()



# palmer penguins -------

library(palmerpenguins)

# remove missing flipper and bill length data
df <- penguins |>
  filter(!is.na(bill_length_mm), !is.na(flipper_length_mm))

ggplot(data = df,
       mapping = aes(x=bill_length_mm, y=flipper_length_mm)) +
  geom_point()



# get those three clusters using kmeans
km <- kmeans(x = df |>
               select(bill_length_mm, flipper_length_mm),
             centers = 3,
             nstart = 100)
df |>
  mutate(cluster_assignment = km$cluster) |>
  ggplot(mapping = aes(x=bill_length_mm, y=flipper_length_mm, color = factor(cluster_assignment))) +
  geom_point() +
  theme_minimal()

# compare to species
df |>
  mutate(cluster_assignment = km$cluster) |>
  ggplot(mapping = aes(x=bill_length_mm, y=flipper_length_mm, color = species)) +
  geom_point() +
  theme_minimal()


# I can't visualize it, but what if we included more information in the
# clustering algorithm?
km <- kmeans(x = df |>
               select(bill_length_mm, flipper_length_mm, bill_depth_mm),
             centers = 3)

# does this do a good job predicting species?
df |>
  mutate(cluster_assignment = km$cluster) |>
  count(species, cluster_assignment)
