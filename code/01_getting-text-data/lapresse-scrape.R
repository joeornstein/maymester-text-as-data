# scrape the text of an article from La Presse

library(stringr)
library(rvest)

# read_html() function reads in the HTML code from a provided URL
# set encoding = 'UTF-8' because the site is in French (Thanks, ChatGPT!)
page <- read_html('https://www.lapresse.ca/actualites/politique/2025-05-13/cabinet-carney/joly-perd-les-affaires-etrangeres-champagne-reste-aux-finances.php',
                  encoding = "UTF-8")

# it's a list with a <head> and a <body> entry
page

# hopefully the information we want is in the <body>

# create a new object called paragraphs which contains
# every bit of text surrounded by a <p> tag
paragraphs <- html_elements(page, 'p')

# convert that HTML code to plaintext using the html_text() function
text <- html_text(paragraphs)

text[1]
text[7]


# there are some elements here that we *don't* want, like the
# image credits, and the headers at the top.

# using the SelectorGadget, I've determined that the CSS selector
# I want is called .textModule--type-paragraph
text <- page |>
  html_elements('.textModule--type-paragraph') |>
  html_text2()

text



## Step 2. Automate this process -----------------------------------

# suppose I wanted to scrape the text of *several* articles on La Presse,
# and I have the URLs for each article.

# write a function to do what we did above
get_la_presse <- function(url){
  # first read the URL
  read_html(url, encoding = "UTF-8") |>
    # then get the article body elements
    html_elements('.textModule--type-paragraph') |>
    # convert the HTML into text
    html_text2()
}

# very first test: make sure it works on the first URL
get_la_presse('https://www.lapresse.ca/actualites/politique/2025-05-13/cabinet-carney/joly-perd-les-affaires-etrangeres-champagne-reste-aux-finances.php')

# second test: try another article
get_la_presse('https://www.lapresse.ca/international/etats-unis/2025-05-13/congres/la-grande-et-belle-loi-budgetaire-voulue-par-trump-a-l-epreuve-des-commissions.php')

# third test: on the website I only get 10 free articles. Can I get more than this way?

# read a list of URLs
df <- readxl::read_xlsx('data/la-presse/urls.xlsx')

# apply the get_la_presse() function to every element in df$URL
df$text <- sapply(df$URL, get_la_presse)

df$text[2]