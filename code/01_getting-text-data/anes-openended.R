# Download and process ANES 2024 open-ended survey responses
#
# Data: ANES 2024 Time Series Study — Redacted Open Ends
#       (separate release, September 23, 2025)
# Source: https://electionstudies.org/data-center/2024-time-series-study/
#
# IMPORTANT — Terms of Use:
#   You must register and agree to the ANES Terms of Use before downloading.
#   Register/log in at: https://electionstudies.org/register/
#   Then visit the data center page, accept the terms, and download:
#     "Open-End Responses" (Excel format)
#
# Citation:
#   American National Election Studies. 2025. ANES 2024 Time Series Study
#   Full Release [dataset and documentation]. August 8, 2025 version.
#   www.electionstudies.org

library(tidyverse)
library(readxl)
library(httr2)

# --- Paths ---
data_dir  <- here::here("data", "anes")
xlsx_file <- "anes_timeseries_2024_redactedopenends_excel_20250923.xlsx"
xlsx_path <- file.path(data_dir, xlsx_file)

dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# --- Step 1: Download ---
# The open-ends are distributed as a separate Excel file from the main dataset.
# The script attempts a direct download; if it fails you must download manually.

if (!file.exists(xlsx_path)) {

  anes_url <- paste0(
    "https://electionstudies.org/wp-content/uploads/2025/09/", xlsx_file
  )

  message("Attempting download from ANES...")

  result <- tryCatch({
    request(anes_url) |>
      req_headers(
        `User-Agent` = "Mozilla/5.0",
        `Referer`    = "https://electionstudies.org/data-center/2024-time-series-study/"
      ) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
  }, error = function(e) NULL)

  if (!is.null(result) && resp_status(result) == 200) {
    writeBin(resp_body_raw(result), xlsx_path)
    message("Download complete: ", xlsx_path)
  } else {
    status <- if (!is.null(result)) resp_status(result) else "connection error"
    message(
      "\nAutomatic download failed (status: ", status, ").\n\n",
      "To download manually:\n",
      "  1. Register at https://electionstudies.org/register/\n",
      "  2. Go to https://electionstudies.org/data-center/2024-time-series-study/\n",
      "  3. Accept the terms of use\n",
      "  4. Download the 'Open-End Responses' Excel file\n",
      "  5. Save it to: ", xlsx_path, "\n\n",
      "Then re-run this script."
    )
    stop("Manual download required.", call. = FALSE)
  }
}

# --- Step 2: Read all sheets ---
# The Excel file has one sheet per open-ended variable.
# Each sheet has two columns: V240001 (case ID) and the OE response.
# The column name encodes the variable label, e.g. "V241170 - PRE: What does R like..."

message("Reading open-ended responses from Excel...")
sheets <- excel_sheets(xlsx_path)
message("Found ", length(sheets), " variables across ", length(sheets), " sheets.")

oe_long <- map(sheets, function(s) {
  df <- read_excel(xlsx_path, sheet = s, col_types = "text")
  oe_colname <- names(df)[2]
  tibble(
    V240001  = df[[1]],
    response = df[[2]],
    variable = s,
    label    = str_remove(oe_colname, paste0(s, " - "))
  )
}) |>
  list_rbind() |>
  mutate(V240001 = as.integer(V240001))

# --- Step 3: Variable inventory ---
var_inventory <- oe_long |>
  distinct(variable, label) |>
  mutate(
    wave = if_else(str_starts(variable, "V241"), "pre-election", "post-election"),
    # Tag the substantive text-as-data variables
    substantive = variable %in% c(
      "V241110", "V241112", "V241114", "V241116",   # candidate likes/dislikes
      "V241170", "V241172", "V241174", "V241176",   # party likes/dislikes
      "V242165", "V242167", "V242169", "V242171"    # most important problem
    )
  )

message("\nVariable inventory (", nrow(var_inventory), " total):")
print(var_inventory, n = Inf)

# --- Step 4: Key substantive variables ---
# For text-as-data analysis, the most useful variables are:
#
#   Candidate likes/dislikes (pre-election):
#     V241110  What R likes about Harris
#     V241112  What R dislikes about Harris
#     V241114  What R likes about Trump
#     V241116  What R dislikes about Trump
#
#   Party likes/dislikes (pre-election):
#     V241170  What R likes about the Democratic Party
#     V241172  What R dislikes about the Democratic Party
#     V241174  What R likes about the Republican Party
#     V241176  What R dislikes about the Republican Party
#
#   Most Important Problem (post-election):
#     V242165  First mention
#     V242167  Second mention
#     V242169  Third mention
#     V242171  Which is the single most important

substantive_vars <- var_inventory |>
  filter(substantive) |>
  pull(variable)

oe_substantive <- oe_long |>
  filter(variable %in% substantive_vars, !is.na(response), nzchar(trimws(response)))

message("\nSubstantive OE responses (non-missing): ", nrow(oe_substantive))
message("Respondents with at least one response: ",
        n_distinct(oe_substantive$V240001))

# Response counts per variable
oe_substantive |>
  count(variable, label) |>
  arrange(desc(n)) |>
  print()

# --- Step 5: Clean column names and save ---

# Mapping from ANES variable codes to informative snake_case names.
# Long format: rename the four structural columns.
# Wide format: rename each substantive variable column.
# rename() expects c(new_name = "old_name")
col_names_long <- c(
  case_id       = "V240001",
  text          = "response",
  variable_code = "variable",
  question      = "label"
)

col_names_wide <- c(
  case_id                   = "V240001",
  # Candidate likes/dislikes (pre-election)
  pre_like_dem_candidate    = "V241110",
  pre_dislike_dem_candidate = "V241112",
  pre_like_rep_candidate    = "V241114",
  pre_dislike_rep_candidate = "V241116",
  # Party likes/dislikes (pre-election)
  pre_like_dem_party        = "V241170",
  pre_dislike_dem_party     = "V241172",
  pre_like_rep_party        = "V241174",
  pre_dislike_rep_party     = "V241176",
  # Most important problem (post-election)
  post_mip_mention_1        = "V242165",
  post_mip_mention_2        = "V242167",
  post_mip_mention_3        = "V242169",
  post_mip_most_important   = "V242171"
)

# Long format — all 43 variables, one row per respondent-variable pair
out_long <- file.path(data_dir, "anes_2024_openended_long.csv")

oe_long_out <- oe_long |>
  rename(any_of(col_names_long))

write_csv(oe_long_out, out_long)
message("\nFull long-format data saved to: ", out_long)

# Wide format — one row per respondent, substantive variables as columns
out_wide <- file.path(data_dir, "anes_2024_openended_wide.csv")

oe_wide <- oe_long |>
  filter(variable %in% substantive_vars) |>
  select(V240001, variable, response) |>
  pivot_wider(names_from = variable, values_from = response) |>
  rename(any_of(col_names_wide))

write_csv(oe_wide, out_wide)

# Confirm final column names
message("Wide-format substantive data saved to: ", out_wide)
message("Wide columns: ", paste(names(read_csv(out_wide, n_max = 0, show_col_types = FALSE)), collapse = ", "))
