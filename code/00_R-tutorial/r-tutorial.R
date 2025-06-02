# R Tutorial

# creating and using numeric variables
var <- 1
var + 3

# creating string (text) variables
text <- 'This is some text.'
text

# useful functions for working with strings in
# the stringr package
library(stringr)


str_remove(text, 'is some')


text <- 'Hello, my name is Joe. Hello, my name is Maria.'

# remove the first, or all, instances of a substring
str_remove(text, 'Hello')
str_remove_all(text, 'Hello')

# *replace* the first, or all, instances of a substring
str_replace(text, 'Hello', 'Bonjour')
str_replace_all(text, 'Hello', 'Bonjour')

# if you want to save the output of a function,
# create an object with left arrow!
french_text <- str_replace_all(text, 'Hello', 'Bonjour')

stringr::sentences

# vectorized functions are functions that work
# on a list of objects

# suppose I want to remove all the commas from
# these 720 sentences
str_remove_all(stringr::sentences, ',')

# what's the 412th sentence in this vector?
stringr::sentences[412]

# The ? character allows you to lookup help files
# for any object or function that has one.
?stringr::sentences

# If I want to know which setences in this vector
# contain the word "restless", str_detect() helps
str_detect(stringr::sentences, 'restless')

# how many contain the word restless?
sum(str_detect(stringr::sentences, 'restless'))
sum(str_detect(stringr::sentences, 'square peg'))

# which setence contains that phrase?
which(str_detect(stringr::sentences, 'square peg'))

# what is the sentence that contains that phrase?
stringr::sentences[which(str_detect(stringr::sentences, 'square peg'))]

# for long sequences of functions like this, the PIPE is very useful
# for making your code readable.
stringr::sentences |>
  str_detect('square peg') |>
  which()

# functions for working with whitespace:

# remove leading and trailing whitespace
str_squish('        Here   is   a     string           ')

str_trim('        Here    is    a    string           ')

# three functions for working with character cases
str_to_upper('hello there')
str_to_lower('HELLO THERE')
str_to_title('HELLO THERE!')
