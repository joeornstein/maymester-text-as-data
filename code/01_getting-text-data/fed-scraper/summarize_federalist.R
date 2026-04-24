library(ellmer)
library(dplyr)
library(purrr)

load("data/papers.RData")

SYSTEM_PROMPT <- paste(
  "You are a concise political-science assistant.",
  "When given the text of a Federalist Paper, write a 2-3 sentence summary",
  "of its central argument. Plain prose, no bullet points."
)

chat_template <- chat_openai(
  model         = "gpt-5.4-mini",
  system_prompt = SYSTEM_PROMPT,
  echo          = "none"
)

prompts <- as.list(papers$text)

message("Summarizing ", nrow(papers), " papers in parallel...")

results <- parallel_chat(
  chat_template,
  prompts,
  max_active = 20,   # concurrent requests; lower if you hit rate limits
  on_error   = "return"
)

# Extract text from each result; failed turns come back as Turn objects, not Chat
papers <- papers |>
  mutate(
    summary = map_chr(results, function(r) {
      if (inherits(r, "Chat")) r$last_turn()@text else NA_character_
    })
  )

n_failed <- sum(is.na(papers$summary))
if (n_failed > 0) warning(n_failed, " paper(s) failed — summary is NA for those rows.")

save(papers, file = "data/papers.RData")
message("Done. Summaries saved to data/papers.RData.")

select(papers, number, author, summary)
