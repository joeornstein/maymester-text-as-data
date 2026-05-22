library(tidyverse)
library(fuzzylink)

# load ANES open-ended responses that we pulled together in 01/
load("data/anes/anes_2024_open-ended-cleaned.RData")

df <- oe_wide |>
  filter(post_mip_most_important != 'SK',
         !is.na(post_mip_most_important))

mip <- paste0(
  'I think the most important problem facing our country is: ',
  df$post_mip_most_important)

# approximate embedding costs (~13 cents per million)
sum(nchar(mip)/4) / 1e6 * 0.13

if(file.exists('data/anes/mip-embeddings.RData')){
  load('data/anes/mip-embeddings.RData')
} else{
  emb <- get_embeddings(mip)
  save(emb, file = 'data/anes/mip-embeddings.RData')
}


# fit k-means with a variety of k values ------------

set.seed(6201)
k_range <- 2:20

km_fits <- map(k_range, \(k) kmeans(emb, centers = k))

tibble(
  k        = k_range,
  withinss = map_dbl(km_fits, \(fit) fit$tot.withinss)
) |>
  ggplot(aes(x = k, y = withinss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = k_range) +
  labs(x = "Number of clusters (k)", y = "Total within-cluster SS",
       title = "Elbow plot for k-means on MIP embeddings")

# pick k and extract cluster assignments
set.seed(1315)
km <- kmeans(emb, centers = 12,
             nstart = 30, # this runs the algorithm 30 times and chooses the best
             iter.max = 25)
head(as.numeric(km$cluster))

df$cluster <- km$cluster

# sample the responses that are closest to the centroid in each cluster to label topics ------------

dists <- sqrt(rowSums((emb - km$centers[km$cluster, ])^2))

df |>
  mutate(
    response = post_mip_most_important,
    dist_to_center = dists
  ) |>
  slice_min(dist_to_center, n = 3, by = cluster) |>
  select(cluster, dist_to_center, response) |>
  arrange(cluster, dist_to_center) |>
  print(n = Inf)

# now sample some at random to make sure those topic labels are coherent

df |>
  slice_sample(n = 8, by = cluster) |>
  select(cluster, post_mip_most_important) |>
  arrange(cluster) |>
  print(n = Inf)


# after reviewing, assign topic labels here
cluster_labels <- c(
  "1"  = "",
  "2"  = "",
  "3"  = "",
  "4"  = "",
  "5"  = "",
  "6"  = "",
  "7"  = "",
  "8"  = "",
  "9"  = "",
  "10" = "",
  "11" = "",
  "12" = ""
)

df$cluster_label <- cluster_labels[as.character(df$cluster)]



