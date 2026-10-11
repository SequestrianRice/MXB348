# Logistic Regression Code Example

library(tidyverse)

set.seed(1)

# Lung cancer from vaping...

data = tibble(
  vaping = c(rep(0,20), rep(1,20)),  # Binary covariate
  logodds = 0.5 + 0.5 * vaping,
  p = 1 / (1 + exp(-logodds)),       # Inverse-logit function
  y = rbinom(40, size = 1, prob = p) # Generate outcome
)

# Model
fit = glm(y ~ vaping, data = data, family = binomial)
  # y      = regression formula
  # data   = data we observed
  # family = how the outcome is distributed

summary(fit)
