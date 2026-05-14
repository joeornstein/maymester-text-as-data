#' ---
#' Pull posts from the Reddit API
#' and conduct a sentiment analysis
#'
#' Replaces the old twitter-api.R tutorial. The Twitter/X API was
#' essentially closed to researchers in 2023 -- the free tier is
#' write-only and the cheapest read tier is $200/month. Reddit's
#' public JSON endpoints, by contrast, are free and require no
#' authentication for read-only access (just a polite User-Agent
#' string and a willingness to live with ~10 requests/minute).
#'
#' author: Joe Ornstein
#' date: 2026-05-12
#' ---

library(tidyverse)
library(tidytext)
library(httr)
library(jsonlite)


# Step 1: Hit a public Reddit endpoint ----------------------

# Reddit exposes most of its read endpoints as JSON: just append
# ".json" to almost any reddit.com URL. No API key required for
# read-only access, but Reddit blocks the default R user-agent,
# so we have to identify ourselves. Use something descriptive --
# Reddit's API rules ask for "<platform>:<app-name>:<version> (by /u/<your-username>)".

ua <- user_agent('r-script:uga-maymester-text-as-data:v1 (by /u/Confident_Hold_6863)')

# Let's grab the 100 most recent posts mentioning "inflation"
# from r/economics. The query string mirrors the search box on
# the site:
#   q            = search term
#   restrict_sr  = restrict to this subreddit
#   sort         = new | hot | top | relevance
#   limit        = up to 100 per request

url <- 'https://www.reddit.com/r/economics/search.json?q=inflation&restrict_sr=on&sort=new&limit=100'

response <- GET(url, ua)

# Pull the JSON content and convert to an R object
content <- response$content |>
  rawToChar() |>
  fromJSON(flatten = TRUE)

# The interesting stuff lives in content$data$children.
# Each row is one post; flatten=TRUE adds a "data." prefix to every column.
posts <- content$data$children |>
  rename_with(~ sub("^data\\.", "", .x)) |>
  as_tibble() |>
  select(id, created_utc, author, subreddit, title, selftext, score, num_comments, permalink) |>
  mutate(created = as.POSIXct(created_utc, origin = '1970-01-01', tz = 'UTC'),
         # posts have both a title and a body (selftext); paste them together
         text = ifelse(selftext == "" | selftext == "[removed]", title, paste(title, selftext)))

# save the raw pull so we don't have to re-hit the API every time
save(posts, file = 'data/raw/reddit-inflation.RData')


# Step 2: Wrap it in a function -----------------------------

get_reddit_posts <- function(query,
                             subreddit = 'all',
                             sort = 'new',
                             limit = 100,
                             ua = user_agent('r-script:uga-maymester:v1 (by /u/Confident_Hold_6863)')){

  url <- paste0('https://www.reddit.com/r/', subreddit,
                '/search.json?q=', URLencode(query),
                '&restrict_sr=on&sort=', sort,
                '&limit=', limit)

  response <- GET(url, ua)

  # be polite to Reddit's rate limiter
  Sys.sleep(1)

  content <- response$content |>
    rawToChar() |>
    fromJSON(flatten = TRUE)

  children <- content$data$children
  if (!is.data.frame(children)) stop("Unexpected response structure -- possibly rate-limited.")

  children |>
    rename_with(~ sub("^data\\.", "", .x)) |>
    as_tibble() |>
    select(id, created_utc, author, subreddit, title, selftext, score, num_comments, permalink) |>
    mutate(created = as.POSIXct(created_utc, origin = '1970-01-01', tz = 'UTC'),
           text = ifelse(selftext == "" | selftext == "[removed]", title, paste(title, selftext)))
}

# now we can pull from anywhere with one line
econ_posts     <- get_reddit_posts('inflation', subreddit = 'economics')
politics_posts <- get_reddit_posts('inflation', subreddit = 'politics')


# Step 3: Pull comments for a post -----------------------------

# Appending ".json" to any post permalink returns a two-element list:
#   [[1]] the post itself
#   [[2]] the comment tree
# kind == "t1" rows are actual comments; kind == "more" are stubs for
# collapsed threads that would need extra requests to expand.

get_comments <- function(permalink, ua) {
  empty <- tibble(id = character(), author = character(), body = character(),
                  score = integer(), created_utc = numeric(),
                  created = as.POSIXct(numeric(), origin = '1970-01-01'),
                  permalink = character())

  tryCatch({
    url <- paste0('https://www.reddit.com', permalink, '.json')
    response <- GET(url, ua)
    Sys.sleep(1)

    # fromJSON(flatten=TRUE) combines the two listings (post + comments) into
    # a 2-row data frame; $data.children[[2]] is the comment listing
    content <- response$content |>
      rawToChar() |>
      fromJSON(flatten = TRUE)

    content$data.children[[2]] |>
      rename_with(~ sub("^data\\.", "", .x)) |>
      as_tibble() |>
      filter(kind == 't1') |>
      select(id, author, body, score, created_utc) |>
      mutate(created = as.POSIXct(created_utc, origin = '1970-01-01', tz = 'UTC'),
             permalink = permalink)
  }, error = function(e) empty)
}

# pull comments for every post, then stack into one data frame
comments <- map(econ_posts$permalink, get_comments, ua = ua) |>
  list_rbind()


# Step 4: A note on the OAuth API ---------------------------

# The public .json endpoints are great for classroom demos but
# capped at ~10 requests/minute. For a research project you'll
# probably want the authenticated API:
#
#   1. Go to https://www.reddit.com/prefs/apps
#   2. Click "create another app...", choose "script"
#   3. You'll get a client_id and client_secret
#   4. Use httr::oauth2.0_token() or the `RedditExtractoR` package
#      (which wraps a lot of this) to authenticate
#
# Authenticated requests get 100 QPM and access to a few endpoints
# (like /api/morechildren for deep comment trees) that the public
# JSON doesn't expose.
