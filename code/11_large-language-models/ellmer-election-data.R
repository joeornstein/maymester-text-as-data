library(ellmer)

url <- 'https://www.sos.state.ms.us/elections/2008/Primary/DC%20Statewide/2008%20Statewide%20Democratic%20Primary%20Results.pdf'


## 1. Single-cell lookup (temperature = 0 for determinism) ----------------

chat <- chat_openai(model = 'gpt-5.4',
                    params = params(temperature = 0),
                    system_prompt = 'You are a precise data entry assistant. Return only what is asked.')

resp <- chat$chat(
  content_pdf_url(url),
  'How many total votes did Hillary Clinton receive in Yazoo county?'
)

resp


## 2. Structured extraction of the full table ----------------
# type_object() enforces a strict JSON schema so the model must return
# valid integers — no formatting variation, no free-text commentary.

county_type <- type_object(
  county      = type_string('County name'),
  biden       = type_integer('Joseph Biden vote total'),
  clinton     = type_integer('Hillary Clinton vote total'),
  dodd        = type_integer('Chris Dodd vote total'),
  edwards     = type_integer('John Edwards vote total'),
  gravel      = type_integer('Mike Gravel vote total'),
  kucinich    = type_integer('Dennis Kucinich vote total'),
  obama       = type_integer('Barack Obama vote total'),
  richardson  = type_integer('Bill Richardson vote total'),
  uncommitted = type_integer('Undecided vote total')
)

chat2 <- chat_openai(model = 'gpt-5.4',
                    params = params(temperature = 0),
                    system_prompt = 'You are a precise data entry assistant. Return only what is asked.')

results <- chat2$chat_structured(
  content_pdf_url(url),
  'Extract every row from the county-level vote totals table on page 2 of this PDF.
   Include all candidates as columns. Use 0 if a candidate has no votes listed.',
  type = type_array(county_type)
)


## 3. Render page 2 as a high-res image and send that instead ----------------
# content_pdf_url() lets the API decide how to render the PDF.
# Rendering at 300 DPI ourselves gives the model a much cleaner image to read.

library(pdftools)
library(png)

pdf_path <- tempfile(fileext = '.pdf')
img_path <- tempfile(fileext = '.png')

download.file(url, destfile = pdf_path, mode = 'wb', quiet = TRUE)

# Render page 2 at 300 DPI (default is ~72 DPI)
page_img <- pdf_render_page(pdf_path, page = 2, dpi = 300)
writePNG(page_img, img_path)

chat3 <- chat_openai(model = 'gpt-5.4',
                     params = params(temperature = 0),
                     system_prompt = 'You are a precise data entry assistant. Return only what is asked.')

results_hires <- chat3$chat_structured(
  content_image_file(img_path, resize = 'high'),
  'Extract every row from the county-level vote totals table in this image.
   Include all candidates as columns. Use 0 if a candidate has no votes listed.',
  type = type_array(county_type)
)


## 5. Switch to Claude ----------------
# According to Claude, "claude-sonnet-4-6 tends to be stronger on structured document understanding."
# We reuse the same high-res image from section 3.

chat5 <- chat_anthropic(model = 'claude-sonnet-4-6',
                        params = params(temperature = 0),
                        system_prompt = 'You are a precise data entry assistant. Return only what is asked.')

results_claude <- chat5$chat_structured(
  content_image_file(img_path, resize = 'high'),
  'Extract every row from the county-level vote totals table in this image.
   Include all candidates as columns. Use 0 if a candidate has no votes listed.',
  type = type_array(county_type)
)


## 4. One county at a time (most reliable, most expensive) ----------------
# Each call is a simpler task: find one row instead of the whole table.
# We use the county names extracted above as the query list.

counties <- sapply(results_claude, `[[`, 'county')

prompts <- paste0(
  'This image shows Mississippi 2008 Democratic Primary results. ',
  'How many votes did each candidate receive in ', counties, ' County? ',
  'Return only the numbers for that single row.'
)

# Create one fresh chat per county so histories don't bleed across calls
chats <- lapply(seq_along(counties), function(i) {
  chat_anthropic(model = 'claude-sonnet-4-6',
                 params = params(temperature = 0))
})

# parallel_chat_text() sends all prompts concurrently
# We attach the image to each prompt via a list of content objects
prompts_with_image <- lapply(seq_along(counties), function(i) {
  list(content_image_file(img_path, resize = 'high'), prompts[i])
})

results_parallel <- mapply(function(ch, prompt_list) {
  ch$chat_structured(
    prompt_list[[1]],
    prompt_list[[2]],
    type = county_type
  )
}, chats, prompts_with_image, SIMPLIFY = FALSE)

results_parallel_df <- do.call(rbind, lapply(results_parallel, as.data.frame))
results_parallel_df
