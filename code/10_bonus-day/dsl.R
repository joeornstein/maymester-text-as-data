# Design-Based Supervised Learning
# How to make inferences with potentially-noisy LLM labels

# devtools::install_github('naoki-egami/dsl')
library(dsl)

data("PanChen")

# countyWrong is the "expert annotation"
# pred_countyWrong is the GPT-4 annotation
# the former is gold-standard, but we only have it for a subset
# the latter is more widely available, but measured with error.

## Three potential approaches ---------------

# Option1. Ignore the noisy GPT-4 labels.
# benefit of this approach is that it eliminates measurement error
# drawback is that it reduces statistical power

mod1 <- glm(SendOrNot ~ countyWrong + prefecWrong +
              connect2b + prevalence + regionj + groupIssue,
            data = PanChen,
            family = 'binomial')
summary(mod1)

# problem with using a relatively small set of annotations
# is that confidence intervals are much wider (n=500)

# Option 2. Ignore the expert labels. There's too few!
# maybe do a little validation check ahead of time,
# but once we're satisfied, just fit our model on the
# GPT-4 labels

mean(PanChen$countyWrong == PanChen$pred_countyWrong,
     na.rm = TRUE) # 80.4% accuracy!!

mod2 <- glm(SendOrNot ~ pred_countyWrong + prefecWrong +
              connect2b + prevalence + regionj + groupIssue,
            data = PanChen,
            family = 'binomial')
summary(mod2)
# problem here, potentially, is that those errors in 20%
# of the documents may be systematic (i.e. correlated with
# our outcome).


## Option 3. Combine information from both!
# idea is that we'll use the expert annotations
# to estimate the direction of the bias from GPT-4,
# and we'll use *that* estimate to bias-correct our
# downstream regression.

mod3 <- dsl(model = "logit",
            formula = SendOrNot ~ countyWrong + prefecWrong +
                    connect2b + prevalence + regionj + groupIssue,
            predicted_var = "countyWrong",
            prediction = "pred_countyWrong",
            data = PanChen)
summary(mod3)







