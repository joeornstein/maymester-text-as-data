## NYTimes API
library(tidyverse)
library(httr)

# to access the NYTimes API, we need an API Key
# to get one, sign up for a Developer Account, create an App, and copy
# the API key. It took me 2 minutes.

api_key <- read_file('data/nytimes-api-key.txt')

# get articles from the example URL
url <- paste0('https://api.nytimes.com/svc/archive/v1/2025/4.json?api-key=',
              api_key)
response <- GET(url)

# convert that to an R object
content <- response$content |>
  rawToChar() |>
  jsonlite::fromJSON()

# pull out the dataframe
df <- content$response$docs

# compute the percent of lead paragraphs that contain the word "inflation"
df <- df |>
  mutate(inflation = str_detect(lead_paragraph, 'inflation'))

pct_inflation <- mean(df$inflation)


## Loop so that we get that pct_inflation measure for every
## month in the last year

dates <- c('2024/5', '2024/6', '2024/7',
           '2024/8', '2024/9', '2024/10',
           '2024/11', '2024/12', '2025/1',
           '2025/2', '2025/3', '2025/4')

base_url <- 'https://api.nytimes.com/svc/archive/v1/'

urls <- paste0(base_url, dates, '.json?api-key=', api_key)

get_inflation_pct <- function(url){

  response <- GET(url)

  # convert that to an R object
  content <- response$content |>
    rawToChar() |>
    jsonlite::fromJSON()

  # pull out the dataframe
  df <- content$response$docs

  # compute the percent of lead paragraphs that contain the word "inflation"
  df <- df |>
    mutate(inflation = str_detect(lead_paragraph, 'inflation'))

  mean(df$inflation)
}

get_inflation_pct(urls[12]) # should be 0.3%

# apply that function to all URLs in the list
inflation_pcts <- rep(NA, length(dates))
for(i in 1:length(dates)){
  print(i)
  inflation_pcts[i] <- get_inflation_pct(urls[i])
  Sys.sleep(10)
}

library(lubridate)

df <- data.frame(dates, inflation_pcts)
df$dates <- as.Date(paste0(df$dates, '/1'),
                    format = '%Y/%m/%d')

ggplot(data = df,
       mapping = aes(x = dates,
                     y = inflation_pcts*100)) +
  geom_point() +
  geom_line() +
  labs(x = 'Month',
       y = 'Pct. of NYTimes Articles Mentioning Inflation') +
  theme_bw()



