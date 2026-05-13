library(pdftools)
library(tidyverse)
library(udpipe)

# ── URLs to process ───────────────────────────────────────────────────────────

urls <- c(
  "https://www.cafc.uscourts.gov/opinions-orders/25-2155.OPINION.4-15-2026_2676705.pdf",
  "https://www.cafc.uscourts.gov/opinions-orders/24-1641.OPINION.5-13-2026_2693032.pdf",
  "https://www.cafc.uscourts.gov/opinions-orders/25-1944.OPINION.4-14-2026_2675911.pdf",
  "https://www.cafc.uscourts.gov/opinions-orders/24-1772.OPINION.4-14-2026_2675876.pdf"
)

# ── Load udpipe model (download once, cache in data/) ────────────────────────

model_dir  <- "data"
model_path <- file.path(model_dir, "english-ewt-ud-2.5-191206.udpipe")
if (!file.exists(model_path)) {
  udpipe_download_model(language = "english", model_dir = model_dir)
}
ud_model <- udpipe_load_model(model_path)

# ── Stoplist: functional adverbs that don't constitute editorializing ─────────

adverb_stoplist <- c(
  # negation
  "not", "never", "nor", "neither",
  # additive / conjunctive
  "also", "too", "either", "furthermore", "moreover", "additionally", "besides",
  # discourse / connective
  "therefore", "thus", "hence", "accordingly", "however", "nevertheless",
  "nonetheless", "otherwise", "meanwhile", "instead", "indeed", "anyway",
  "consequently", "subsequently", "meanwhile",
  # temporal
  "then", "now", "already", "still", "yet", "soon", "previously", "finally",
  "eventually", "immediately", "once", "again", "always", "often", "sometimes",
  "rarely", "ever", "recently", "thereafter", "theretofore", "hereinafter",
  # spatial / directional
  "here", "there", "back", "away", "out", "up", "down", "forward", "ahead",
  "along", "around", "together", "above", "below",
  # degree particles (non-editorializing)
  "just", "only", "even", "enough", "almost", "quite", "rather",
  "so", "more", "most", "less", "least", "much", "well", "far", "further",
  # temporal adjuncts used structurally
  "long", "later", "next", "before", "early",
  # interrogative / relative adverbs
  "when", "where", "how", "why", "wherein",
  # ordinal discourse markers
  "first", "second", "third",
  # discourse / focus markers
  "namely", "respectively", "regardless", "specifically", "pursuant",
  # legal latin fragments (e.g. "de novo" split)
  "novo",
  # citation markers
  "e.g.", "i.e."
)

# ── Function: parse one opinion PDF from a URL ────────────────────────────────

parse_opinion <- function(url) {
  message("Processing: ", url)

  tmp <- tempfile(fileext = ".pdf")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  pages     <- pdf_text(tmp)
  full_text <- paste(pages, collapse = "\n")

  # Case number
  case_number <- str_extract(full_text, "\\d{2}-\\d{4}")

  # Party names from first two pages
  header <- paste(pages[1:min(2, length(pages))], collapse = "\n")

  appellant <- str_match(header,
    "([A-Z][A-Z ,.'&-]+),\\s*\n\\s*[A-Za-z-]*Appellant")[, 2] |> str_squish()

  appellee <- str_match(header,
    "([A-Z][A-Z ,.'&-]+),\\s*\n\\s*[A-Za-z-]*Appellee")[, 2] |> str_squish()

  # Pro se
  pro_se <- str_detect(full_text, regex("pro se", ignore_case = TRUE))

  # Isolate opinion body
  body_start <- str_locate(full_text, regex("\\bOPINION\\b"))[1, "end"]
  body_text  <- if (!is.na(body_start)) str_sub(full_text, body_start + 1) else full_text

  # Clean up page artifacts and hyphenated line breaks
  body_text <- str_replace_all(body_text, "(?m)^\\s*\\d+\\s*$", "")
  body_text <- str_replace_all(body_text, "-\\n\\s*", "")
  # Unwrap legal citation brackets e.g. "[W]hen" -> "When", so udpipe
  # doesn't see orphan fragments like "hen" or "ell" as separate tokens
  body_text <- str_replace_all(body_text, "\\[([A-Z])\\]", "\\1")

  # Annotate with udpipe
  chunks <- str_sub(body_text,
    seq(1, nchar(body_text), by = 5000),
    pmin(seq(5000, nchar(body_text) + 5000, by = 5000), nchar(body_text)))

  annotation <- udpipe_annotate(ud_model, x = chunks, doc_id = seq_along(chunks)) |>
    as_tibble()

  n_words <- sum(!annotation$upos %in% c("PUNCT", "SYM", NA), na.rm = TRUE)

  adverb_rows <- annotation |>
    filter(
      upos == "ADV",
      !str_to_lower(token) %in% adverb_stoplist,
      nchar(token) > 2
    )

  adverb_detail <- adverb_rows |>
    mutate(case_number = case_number, sentence = str_squish(sentence)) |>
    select(case_number, doc_id, sentence_id, token_id, token, sentence)

  list(
    summary = tibble(
      case_number      = case_number,
      appellant        = appellant,
      appellee         = appellee,
      pro_se           = pro_se,
      n_words          = n_words,
      n_adverbs        = nrow(adverb_rows),
      adverbs_per_100w = round(nrow(adverb_rows) / n_words * 100, 2)
    ),
    adverbs = adverb_detail
  )
}

# ── Run across all URLs ───────────────────────────────────────────────────────

results <- map(urls, parse_opinion)

summary_tbl <- map(results, "summary") |> list_rbind()
adverbs_tbl <- map(results, "adverbs") |> list_rbind()

# ── Print & save ──────────────────────────────────────────────────────────────

print(summary_tbl)
cat("\n── Adverbs in context ──────────────────────────────────────────────────────\n")
print(adverbs_tbl, n = Inf)

write_csv(summary_tbl, "data/cafc-opinions.csv")
write_csv(adverbs_tbl, "data/cafc-opinions-adverbs.csv")
message("\nSaved summary to data/cafc-opinions.csv")
message("Saved adverb detail to data/cafc-opinions-adverbs.csv")
