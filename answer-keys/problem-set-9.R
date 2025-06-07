library(tidyverse)
library(fuzzylink)

d <- read_csv('data/egami-2018/Experiment2.csv') |>
  # remove missing text
  filter(!is.na(text)) |>
  # remove multibyte strings (there are a few nonsense responses e.g. USA Copiers)
  # filter(str_detect(text, 'USA Copiers', negate = TRUE)) |>
  mutate(text = str_remove_all(text, '[^\x20-\x7E]'))


table(d$treat)

from_file <- TRUE
if(!from_file){
  embeddings <- get_embeddings(d$text)
  save(embeddings, d, file = 'answer-keys/problem-set-9.RData')
} else{
  load('answer-keys/problem-set-9.RData')
}
dim(embeddings)

# representative texts for "maximal punishment"
punitive_ids <- c(437, 579, 23, 392, 637, 1122, 1008, 336, 635, 815)
target1 <- embeddings[punitive_ids,]

# representative texts for "no prison"
lenient_ids <- c(1e3, 440, 687, 329, 1144, 218, 370, 446, 1015, 970, 612)
target0 <- embeddings[lenient_ids,]

sims1 <- embeddings %*% t(target1)
sims0 <- embeddings %*% t(target0)

d$score1 <- rowMeans(sims1)
d$score0 <- rowMeans(sims0)
d$score <- d$score1 - d$score0
d$score_sum <- d$score1 + d$score0

# remove the most irrelevant responses
d <- d |>
  filter(score_sum >= 0.4)

ggplot(data = d,
       mapping = aes(x = score,
                     fill = if_else(treat == 1, 'Treated', 'Control'))) +
  geom_density(alpha = 0.5) +
  labs(x = 'Respondent\'s Punitiveness',
       fill = NULL,
       y = NULL) +
  theme_bw() +
  scale_fill_manual(values = c('black', 'red'))

d |>
  group_by(treat) |>
  summarize(mean_punitiveness = mean(score))

mod <- lm(score ~ treat, data = d)
summary(mod)

summary(d$score)
sd(d$score)
mean(d$score)

# treatment increases punitiveness score a full standard deviation
# on average


## K-means cluster the embeddings to discover different topics -------------
# load('answer-keys/problem-set-9.RData')
# set.seed(42)
# km <- kmeans(embeddings, centers = 10)
# table(km$cluster)
# d$cluster <- km$cluster

# Cluster 1 = "no prison, deport"
# Cluster 2 = not cohesive
# Cluster 3 = "illegally entering the country is a crime; you should go to prison for crimes"
# Cluster 4 = ...