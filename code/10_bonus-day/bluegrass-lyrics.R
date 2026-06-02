# unsupervised exploration of the major themes
# of bluegrass lyrics

# --- Scraper ----------------------------------------------------------------

library(tidyverse)
library(rvest)

# Step 1: collect all song URLs from the homepage
homepage <- read_html("https://www.bluegrasslyrics.com/")

song_links <- homepage |>
  html_elements("a[href*='/song/']") |>
  (\(x) tibble(
    title = html_text(x, trim = TRUE),
    url   = html_attr(x, "href")
  ))() |>
  filter(nchar(title) > 0) |>
  distinct(url, .keep_all = TRUE)

# Step 2: function to scrape lyrics from a single song page
scrape_song <- function(url) {
  page <- read_html(url)

  title <- page |>
    html_element("h1.entry-title, h1") |>
    html_text(trim = TRUE)

  # Lyrics live in .entry-content; grab all <p> nodes before any <h2>
  content <- page |> html_element(".entry-content")

  # Collect paragraph nodes in document order, stopping at the first <h2>
  # (which marks the "Ad-Free Bluegrass Lyrics" section)
  paragraphs <- content |>
    html_elements("p") |>
    html_text(trim = TRUE)

  # Drop empty paragraphs and anything that looks like footer boilerplate
  lyrics <- paragraphs |>
    keep(\(x) nchar(x) > 0) |>
    discard(\(x) str_detect(x, "^(Ad-Free|Copyright|Bluegrasslyrics)")) |>
    paste(collapse = "\n\n")

  tibble(title = title, lyrics = lyrics)
}

# Step 3: scrape every song with a 0.5 s delay to be polite,
# or load from disk if already scraped
data_path <- here::here("data", "bluegrass_lyrics.csv")

if (file.exists(data_path)) {
  bluegrass_df <- read_csv(data_path)
} else {
  bluegrass_df <- song_links$url |>
    map(\(url) {
      # Sys.sleep(0.5)
      tryCatch(
        scrape_song(url),
        error = \(e) tibble(title = NA_character_, lyrics = NA_character_)
      )
    }, .progress = TRUE) |>
    list_rbind()

  dir.create(dirname(data_path), showWarnings = FALSE, recursive = TRUE)
  write_csv(bluegrass_df, data_path)
}

# check for missing or poorly scraped pages
sum(is.na(bluegrass_df$lyrics))
sum(is.na(bluegrass_df$title))

summary(nchar(bluegrass_df$lyrics))
sum(nchar(bluegrass_df$lyrics) == 0)
bluegrass_df |>
  arrange(nchar(lyrics))

bluegrass_df <- bluegrass_df |>
  filter(!is.na(lyrics))

max(nchar(bluegrass_df$lyrics))


## get embeddings
library(fuzzylink)
emb_path <- 'data/bluegrass-embeddings.RData'
if(!file.exists(emb_path)){
  emb <- get_embeddings(bluegrass_df$lyrics)
  save(emb, file = emb_path)
} else{
  load(emb_path)
}

## k-means clustering

set.seed(135061)
km <- kmeans(emb, centers = 20)
bluegrass_df$cluster <- km$cluster

# train songs
bluegrass_df |>
  filter(cluster == 11)

# heartache
bluegrass_df |>
  filter(cluster == 17)

# someone left home and it was a mistake
bluegrass_df |>
  filter(cluster == 12)
