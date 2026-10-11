## Attribution2 -- Bagged Logistic Regression

# Packages Used
library(tidyverse)
library(ChannelAttribution)

# Data
ac2 <- attribution_v2

## Updating attribution model
attribution_v3 <- attribution_v2

# Count for each channel in path
for (channel in channels) {
  attribution_v3[[channel]] <- str_count(attribution_v3$path, paste0("\\b", channel, "\\b"))
}

## touchpoint grouping
# Short = 1-5
# Medium = 6-10
# Long = 11-15
# Mental > 15

attribution_v3 <- attribution_v3 %>%
  mutate(path_length = case_when(
    touchpoint_count < 5 ~ "short",
    touchpoint_count < 10 ~ "medium",
    touchpoint_count < 15 ~ "long",
    touchpoint_count >= 15 ~ "extreme"))

# Coding the math first
# x_(i,k) = total number of touches a media channel k has had in customer journey i.
modeltest <- glm(conversion ~ GD + GS + FO + MI + MF + FD + FE + GY + 
                   returning + customer_journey_count + path_length,
                 data = attribution_v3,
                 family = binomial)
summary(modeltest)

data(PathData)
