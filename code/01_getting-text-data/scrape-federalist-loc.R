library(rvest)
library(httr2)
library(dplyr)
library(purrr)
library(stringr)

# robots.txt for guides.loc.gov:
#   User-agent: *
#   Crawl-delay: 10
#   Disallow: /az.php?, /az/databases?, /er.php, /err.php,
#             /friendly.php, /go.php, /ld.php, /srch.php
#
# /federalist-papers/ is NOT disallowed, so we're good.
# We must respect the 10-second crawl delay.

BASE_URL <- "https://guides.loc.gov/federalist-papers/full-text"
CRAWL_DELAY <- 10  # seconds, per robots.txt

UA <- "Mozilla/5.0 (educational research bot; text-as-data course)"

# ── Helper: fetch a page politely ─────────────────────────────────────────────
fetch_page <- function(url) {
  message("Fetching: ", url)
  Sys.sleep(CRAWL_DELAY)
  resp <- request(url) |>
    req_user_agent(UA) |>
    req_headers(
      Accept          = "text/html,application/xhtml+xml",
      `Accept-Language` = "en-US,en;q=0.9"
    ) |>
    req_retry(max_tries = 3, backoff = ~ 15) |>
    req_perform()
  read_html(resp_body_string(resp))
}

# ── Helper: parse one paper box into a row ────────────────────────────────────
parse_box <- function(box) {
  number <- box |>
    html_element("h2.s-lib-box-title") |>
    html_text(trim = TRUE) |>
    str_squish()

  content <- box |> html_element(".s-lib-box-content")
  if (is.na(content)) return(NULL)

  paras <- content |> html_elements("p") |> html_text(trim = TRUE)

  subtitle <- content |>
    html_element("p strong") |>
    html_text(trim = TRUE)

  author_line <- paras[str_detect(paras, "^Author:")]
  author <- if (length(author_line) > 0) {
    str_trim(str_remove(author_line[1], "^Author:\\s*"))
  } else {
    NA_character_
  }

  full_text <- paras |>
    str_subset(".+") |>
    str_c(collapse = "\n\n")

  tibble(number, subtitle, author, text = full_text)
}

# ── Step 1: get sub-page URLs from the index ──────────────────────────────────
message("Fetching index page...")
index_page <- fetch_page(BASE_URL)

sub_page_urls <- index_page |>
  html_elements("a[href*='federalist-papers/text-']") |>
  html_attr("href") |>
  str_extract("https://guides\\.loc\\.gov/federalist-papers/text-[^#\"\\s]+") |>
  na.omit() |>
  unique()

message("Found ", length(sub_page_urls), " sub-pages.")

# ── Step 2: scrape each sub-page ──────────────────────────────────────────────
scrape_subpage <- function(url) {
  page <- fetch_page(url)
  page |>
    html_elements("[id^='s-lg-box-wrapper-']") |>
    map(parse_box) |>
    compact() |>
    list_rbind()
}

papers <- map(sub_page_urls, scrape_subpage) |> list_rbind()

# ── Step 3: tidy ──────────────────────────────────────────────────────────────
papers <- papers |>
  mutate(
    paper_num = as.integer(str_extract(number, "\\d+")),
    number    = str_squish(number),
    author    = str_squish(author)
  ) |>
  filter(!is.na(paper_num)) |>
  arrange(paper_num) |>
  select(paper_num, number, subtitle, author, text)

# Strip the preamble up through the salutation
papers <- papers |>
  mutate(
    text = str_remove(
      text,
      regex("^.*?To the People of the State of New York:[^\n]*\n\n", dotall = TRUE)
    )
  )

message("Scraped ", nrow(papers), " papers.")
print(select(papers, paper_num, subtitle, author))

write.csv(papers, "data/federalist-papers.csv", row.names = FALSE)
message("Saved to data/federalist-papers.csv")
