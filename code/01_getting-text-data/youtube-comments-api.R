#' ---
#' Collect YouTube comments with the YouTube Data API v3 and httr2
#'
#' The YouTube Data API requires a free API key from Google Cloud Console.
#' Steps to get one:
#'   1. Go to https://console.cloud.google.com/
#'   2. Create a project, then enable "YouTube Data API v3"
#'   3. Under "Credentials", create an API key and copy it
#'   4. Save it to a plain-text file (e.g. youtube-api-key.txt) -- do NOT
#'      hard-code it in your script or commit it to git
#'
#' The free quota is 10,000 units/day. A commentThreads.list call costs
#' 1 unit, so you can make thousands of requests before hitting the cap.
#'
#' API key setup:
#'   Store your key in ~/.Renviron so it never touches the project directory:
#'     1. Run usethis::edit_r_environ() to open the file
#'     2. Add a line: YOUTUBE_API_KEY=AIza...
#'     3. Save and restart R
#'   Then retrieve it in any script with Sys.getenv() (see below).
#'
#' author: Joe Ornstein
#' date: 2026-05-14
#' ---

library(httr2)
library(tidyverse)

api_key <- Sys.getenv('YOUTUBE_API_KEY')


# Step 1: Fetch one page of comments for a video --------------------

# The commentThreads endpoint returns top-level comments (and a reply count).
# Each page holds up to 100 comments; the response includes a nextPageToken
# when more pages exist.

video_id <- 'dQw4w9WgXcQ'   # replace with any public YouTube video ID

req <- request('https://www.googleapis.com/youtube/v3/commentThreads') |>
  req_url_query(
    part       = 'snippet',
    videoId    = video_id,
    maxResults = 100,
    key        = api_key
  )

response <- req_perform(req)
content  <- resp_body_json(response)

# Each item wraps a topLevelComment; drill into the snippet to get the text
parse_comment_page <- function(content) {
  map(content$items, \(item) {
    s <- item$snippet$topLevelComment$snippet
    tibble(
      id          = item$id,
      author      = s$authorDisplayName,
      text        = s$textDisplay,
      likes       = s$likeCount,
      reply_count = item$snippet$totalReplyCount,
      published   = s$publishedAt
    )
  }) |> list_rbind()
}

comments <- parse_comment_page(content)
comments


# Step 2: Paginate to collect more comments -------------------------

# YouTube returns up to 100 comments per request. To get more, pass the
# nextPageToken from each response back as the pageToken query parameter.

get_youtube_comments <- function(video_id, api_key, max_comments = 500) {

  all_comments <- list()
  page_token   <- NULL
  collected    <- 0

  repeat {
    req <- request('https://www.googleapis.com/youtube/v3/commentThreads') |>
      req_url_query(
        part       = 'snippet',
        videoId    = video_id,
        maxResults = 100,
        key        = api_key,
        pageToken  = page_token   # NULL is silently dropped by req_url_query
      )

    response <- req_perform(req)
    content  <- resp_body_json(response)

    page          <- parse_comment_page(content)
    all_comments  <- c(all_comments, list(page))
    collected     <- collected + nrow(page)

    page_token <- content$nextPageToken

    # stop when we've hit our limit or there are no more pages
    if (is.null(page_token) || collected >= max_comments) break

    Sys.sleep(0.5)   # stay well within the API quota
  }

  list_rbind(all_comments) |> slice_head(n = max_comments)
}

comments <- get_youtube_comments(video_id, api_key, max_comments = 300)
glimpse(comments)


# Step 3: Compare comments across multiple videos -------------------

video_ids <- c(
  rickroll  = 'dQw4w9WgXcQ',
  gangnam   = '9bZkp7q19f0'
)

all_comments <- map(video_ids, get_youtube_comments,
                    api_key      = api_key,
                    max_comments = 200) |>
  list_rbind(names_to = 'video')

# quick look at comment length by video
all_comments |>
  mutate(nchar = nchar(text)) |>
  group_by(video) |>
  summarise(mean_length = mean(nchar), median_length = median(nchar), n = n())
