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
modeltest <- glm(conversion ~ GD + GS + FO + MI + MF + FD + FE + GY,
                 data = attribution_v3,
                 family = binomial(link = "logit"))
summary(modeltest)

predict(modeltest, type = "response")

par(mfrow = c(2,2))
plot(modeltest)

data(PathData)

# ================
# HEURISTIC MODELS
# ================

heuristic <- heuristic_models(Data = attribution_v3,
                 var_path = "path",
                 var_conv = "conversion",
                 sep =">")

totalconversions <- sum(attribution_v3$conversion == "1")
totalconversions

heuristic$conversions <- totalconversions
# First Touch
heuristic$first_touch_perc <- heuristic$first_touch / heuristic$conversions
# Last Touch
heuristic$last_touch_perc <- heuristic$last_touch / heuristic$conversions
# Linear Touch
heuristic$linear_touch_perc <- heuristic$linear_touch / heuristic$conversions

# ==========
# DATA-DRIVEN: BAGGED LOGISTIC -- CHANNELS ONLY
# ==========

# p = 1 / (1 + exp(-eta))



# ==========
# DATA-DRIVEN: BAGGED LOGISTIC -- JOURNEY LENGTH
# ==========

# ==========
# DATA-DRIVEN: BAGGED LOGISTIC -- COLLAPSED JOURNEYS
# ==========

# ==========
# DATA-DRIVEN: BAGGED LOGISTIC -- CHANNEL GROUPING
# ==========
