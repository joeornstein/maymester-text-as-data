
# two containers

vocabulary <- c('probability', 'is', 'overrated')

jar <- c(3, 2, 1)
mug <- c(2, 2, 2)

names(jar) <- vocabulary
names(mug) <- vocabulary

# "probability overrated is"
document1 <- c(1,1,1)

# P(document1 | mug) = 6 * 1/3 * 1/3 * 1/3 = 2/9
# P(document1 | jar) = 6 * 1/2 * 1/6 * 1/3 = 1/6

dmultinom(document1, prob = mug)
dmultinom(document1, prob = jar)

# likelihood ratio
dmultinom(document1, prob = mug) / dmultinom(document1, prob = jar)

# "probability overrated is overrated probability"
document2 <- c(2,1,2)

dmultinom(document2, prob = mug)
dmultinom(document2, prob = jar)
dmultinom(document2, prob = mug) / dmultinom(document2, prob = jar)


document3 <- c(3, 2, 4)
dmultinom(document3, prob = mug)
dmultinom(document3, prob = jar)
