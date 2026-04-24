library(rvest)
library(httr2)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
  "AppleWebKit/537.36 (KHTML, like Gecko) ",
  "Chrome/120.0.0.0 Safari/537.36"
)

fetch_html <- function(url, delay = 1.5) {
  Sys.sleep(delay)
  resp <- request(url) |>
    req_user_agent(UA) |>
    req_headers(
      "Accept"          = "text/html,application/xhtml+xml",
      "Accept-Language" = "en-US,en;q=0.9"
    ) |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_perform()
  read_html(resp_body_string(resp))
}

parse_paper_box <- function(box) {
  number <- box |>
    html_element("h2.s-lib-box-title") |>
    html_text(trim = TRUE) |>
    str_squish()

  content <- box |> html_element(".s-lib-box-content")
  if (is.na(content)) return(NULL)

  paras <- content |> html_elements("p")
  para_texts <- html_text(paras, trim = TRUE)

  # Subtitle: text of the first <strong> (e.g. "General Introduction")
  subtitle <- content |>
    html_element("p strong") |>
    html_text(trim = TRUE)

  # Author: paragraph that starts with "Author:"
  author_raw <- para_texts[str_detect(para_texts, "^Author:")]
  author <- if (length(author_raw) > 0) {
    str_trim(str_remove(author_raw[1], "^Author:\\s*"))
  } else {
    NA_character_
  }

  # Full text: all paragraphs joined, stripping footnote-only paragraphs
  full_text <- para_texts |>
    str_subset(".+") |>         # drop empty strings
    str_c(collapse = "\n\n")

  tibble(
    number   = number,
    subtitle = subtitle,
    author   = author,
    text     = full_text
  )
}

parse_subpage <- function(url) {
  message("  Fetching: ", url)
  page <- fetch_html(url)
  boxes <- page |> html_elements("[id^='s-lg-box-wrapper-']")
  map(boxes, parse_paper_box) |> compact() |> list_rbind()
}

# ── 1. Discover sub-page URLs from the index ──────────────────────────────────
message("Fetching index page...")
index <- fetch_html("https://guides.loc.gov/federalist-papers/full-text")

sub_pages <- index |>
  html_elements("a[href*='federalist-papers/text-']") |>
  html_attr("href") |>
  str_extract("https://guides\\.loc\\.gov/federalist-papers/text-[^#\"\\s]+") |>
  na.omit() |>
  unique()

message("Found ", length(sub_pages), " sub-pages.")

# ── 2. Scrape every sub-page ───────────────────────────────────────────────────
papers <- map(sub_pages, parse_subpage) |> list_rbind()

# ── 3. Tidy up ────────────────────────────────────────────────────────────────
papers <- papers |>
  mutate(
    number    = str_squish(number),
    paper_num = as.integer(str_extract(number, "\\d+")),
    author    = str_squish(author)
  ) |>
  filter(!is.na(paper_num)) |>   # drop Table of Contents and other nav boxes
  arrange(paper_num) |>
  select(paper_num, number, subtitle, author, text)

# ── 4. Trim preamble — keep only text after the salutation ───────────────────
papers <- papers |>
  mutate(
    text = str_remove(
      text,
      regex("^.*?To the People of the State of New York:[^\n]*\n\n", dotall = TRUE)
    )
  )

message("\nDone! Scraped ", nrow(papers), " Federalist Papers.")
print(select(papers, paper_num, number, subtitle, author))

papers

save(papers, file = 'data/papers.RData')
