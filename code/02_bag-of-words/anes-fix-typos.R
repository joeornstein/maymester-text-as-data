library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(ellmer)

# --- Load data ---
oe_wide <- read_csv(here::here("data", "anes", "anes_2024_openended_wide.csv"),
                    show_col_types = FALSE)


# format an ellmer prompt ---------

chat <- chat_openai(model = 'gpt-5.4-mini',
                    system_prompt = 'Read this ANES open-ended response, in which the respondent says what they like about Kamala Harris before the 2024 election. Return the text verbatim, but with typos and grammatical errors fixed.')

oe_wide$pre_like_dem_candidate_cleaned <- NA
oe_wide$pre_like_dem_candidate_cleaned[!is.na(oe_wide$pre_like_dem_candidate)] <-
  parallel_chat_text(chat,
                     prompts = interpolate('{{oe_wide$pre_like_dem_candidate[!is.na(oe_wide$pre_like_dem_candidate)]}}'))


save(oe_wide, file = 'data/anes/anes_2024_open-ended-cleaned.RData')
