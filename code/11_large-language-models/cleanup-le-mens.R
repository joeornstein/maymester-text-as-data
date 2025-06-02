# ---
# Script to clean up the Goodreads data from Le Mens et al. (2023)
# ----

library(tidyverse)

goodreads <- read_csv('raw/mystery_vs_other_train_set_v4.csv')
human_codes <- read_csv('data/goodreads/human_data_Mystery.csv')

# merge in the text
d <- left_join(human_codes, goodreads |>
                 select(id = workid, text),
               by = 'id')

write_csv(d, file = 'data/goodreads/cleaned-data.csv')
