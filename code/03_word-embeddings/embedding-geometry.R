
library(fuzzylink)

words <- c('Paris', 'London', 'Berlin', 'Moscow',
           'Lima', 'Santiago', 'Havana',
           'France', 'England', 'Germany', 'Russia',
           'Peru', 'Chile', 'Cuba')

emb <- get_embeddings(words)

get_similarity_matrix(emb)

# What is the capital of Cuba?
havana_maybe <- emb['Paris',] - emb['France',] + emb['Cuba',]

# cosine similarity with the actual Havana embedding
sum(emb['Havana',] * havana_maybe)

# is this closer to Havana than to anything else in words?
emb %*% havana_maybe


# what's the capital of Chile?
x <- emb['London',] - emb['England',] + emb['Chile',]

emb %*% x


# misspellings and their correct spellings

words <- c('proffesor', 'professor',
           'nambia', 'namibia',
           'dog', 'dawg',
           'purple', 'pruple')
emb <- get_embeddings(words)


# correct the misspelling "proffesor"
x <- emb['purple',] - emb['pruple',] + emb['proffesor',]

emb %*% x

# correct the misspelling of "dawg"
x <- emb['purple',] - emb['pruple',] + emb['dawg',]
emb %*% x


# family relations?
words <- c('niece', 'nephew', 'aunt', 'uncle', 'man', 'woman', 'girl', 'boy')
emb <- get_embeddings(words)

x <- emb['niece',] - emb['girl',] + emb['boy',]
emb %*% x # if this works, it should be close to "nephew"
