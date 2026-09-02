## ATTRIBUTION METHOD
library(ChannelAttribution)
library(tidyverse)
library(markovchain)

## DATA
journey <- read_csv("data/customer_journey-4.csv")
results <- read_csv("data/results-3.csv")

# feature engineering
  # path with same order id into 1 column.
  # can look at frequency of each path.
  # group into journey length too.
  # duration time + grouped too.
  # compare with collapsed group.
  # looking at total conversion rate and then provide weighting to EACH CHANNEL.

# direction
  # no grouping
  # grouping - time, channels, collapsing.
