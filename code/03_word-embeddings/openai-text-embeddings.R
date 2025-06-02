## Get text embeddings from OpenAI


# devtools::install_github('joeornstein/fuzzylink')

library(fuzzylink)

# copy-paste your API key here:
openai_api_key('<MY API KEY>', install = TRUE)

# get the text embeddings for a bunch of animals and their babies
animals <- c('cat', 'kitten', 'dog', 'puppy', 'horse', 'foal',
             'duck', 'duckling', 'moose', 'calf', 'guinea pigs')

animal_embeddings <- get_embeddings(animals)
animal_embeddings

# extract some rows from that matrix
cat_vector <- animal_embeddings['cat',]
dog_vector <- animal_embeddings['dog',]
kitten_vector <- animal_embeddings['kitten',]
duckling_vector <- animal_embeddings['duckling',]
puppy_vector <- animal_embeddings['puppy',]

# cosine similarity = dot product divided by length
# nice thing about OpenAI embeddings is that they're always
# length 1, so we just take the dot product
sum(cat_vector * dog_vector)
sum(cat_vector * kitten_vector)
sum(cat_vector * puppy_vector)
sum(kitten_vector * duckling_vector)
sum(cat_vector * duckling_vector)

## matrix multiplication is just a series of dot products
cosine_similarities <- animal_embeddings %*% t(animal_embeddings)


## embed the federalist papers -----------------------


# read the raw HTML
page <- read_html('https://www.gutenberg.org/cache/epub/18/pg18-images.html')

# get all the chapters
paragraphs <- html_elements(page, '.chapter')

# get just the text from the element we want
text <- html_text2(paragraphs)

d <- tibble(text)

# get rid of the slightly different version of Federalist 70
d <- d |>
  filter(str_detect(text, 'slightly different version', negate = TRUE))

# create a column for the title and attributed author
d <- d |>
  mutate(author = text |>
           str_extract('HAMILTON AND MADISON|HAMILTON OR MADISON|HAMILTON|MADISON|JAY') |>
           str_to_title(),
         title = str_extract(text, 'No. [A-Z].*'))


federalist_embeddings <- get_embeddings(d$text)
cosine_similarities <- federalist_embeddings %*% t(federalist_embeddings)


cosine_similarities[1,2] # Hamilton and Jay
cosine_similarities[1,3] # Hamilton and Jay
cosine_similarities[2,3] # Jay and Jay
cosine_similarities[3,4] # Jay and Jay



names <- c('Professor', 'Smith', 'Ornstein', 'Tislenko',
           'Egghead')

name_embeddings <- get_embeddings(names)
name_embeddings %*% t(name_embeddings)

name_embeddings <- get_embeddings(names, dimensions = 2)
name_embeddings

df <- tibble(x = name_embeddings[,1],
             y = name_embeddings[,2],
             name = names)

ggplot(data = df,
       mapping = aes(x=x,
                     y=y,
                     label = name)) +
  geom_text()



