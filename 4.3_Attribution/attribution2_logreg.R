## Attribution2 -- Bagged Logistic Regression

# Packages Used
library(tidyverse)
library(ChannelAttribution)
library(markovchain)

# Data
attribution_v3 <- read_csv("C:/Uni26_Sem2/MXB348/WIL/MXB348/data/attribution_full.csv")


# ------------------------------
# 4.3.1a Overall Conversion Rate
# ------------------------------
total_conversions <- sum(attribution_v3$conversion == "1")
conversion_rate <- sum(attribution_v3$conversion == "1") / length(attribution_v3$conversion)
  # = 0.5876 = conversion rate of 58.76%

# -------------------------------------
# 4.3.1b Channel Conversion Attribution
# -------------------------------------

modeltest <- glm(conversion ~ GD + GS + FO + MI + MF + FD + FE + GY,
                 data = attribution_v3,
                 family = binomial(link = "logit"))
summary(modeltest)

# ++ HEURISTIC ++
aHeuristic <- heuristic_models(Data = attribution_v3,
                 var_path = "path",
                 var_conv = "conversion",
                 sep =">")

# Channel Credit
aHeuristic$first_touch <- aHeuristic$first_touch / total_conversions * 100
aHeuristic$last_touch <- aHeuristic$last_touch / total_conversions * 100
aHeuristic$linear_touch <- aHeuristic$linear_touch / total_conversions * 100

# ++ MARKOV ++

markov_array <- markov_model(attribution_v3,
                        var_path = 'path',
                        var_conv = 'conversion',
                        var_null = 'null_conversion',
                        out_more = TRUE)

aMarkov <- markov_array$result
aMarkov$all_conversions <- total_conversions
aMarkov$rate <- aMarkov$total_conversions / aMarkov$all_conversions * 100

aModel <- cbind(aHeuristic, aMarkov$rate)

aModel <- aModel %>%
  rename(markov_touch = `aMarkov$rate`)

aModel %>%
ggplot()

aTrans <- markov_array$transition_matrix

aTrans$transition_probability <- aTrans$transition_probability * 100

# p = 1 / (1 + exp(-eta))
# x_(i,k) = total number of touches a media channel k has had in customer journey i.

# aLogistic <- glm(family = binomial(link = "logit"))

# --------------------------------------------
# 4.3.2a Journey Length Impact - Short vs Long
# --------------------------------------------

touchpoint_groups <- attribution_v3 %>%
  filter(conversion == "1") %>%
  count(touchpoint_length, name = "conversion_count") %>%
  rename(Length = touchpoint_length)

lengths <- c("Singular", "Tiny", "Short", "Medium", "Long", "Very Long")

markov_array_lengths <- set_names(lengths) %>%
  map(\(category) {
    attribution_v3 %>%
      filter(touchpoint_length == category) %>%
      markov_model(
        var_path = "path",
        var_conv = "conversion",
        var_null = "null_conversion",
        out_more = TRUE
      )
  })

aMarkov_sin <- markov_array_lengths[["Singular"]]$result
aMarkov_tin <- markov_array_lengths[["Tiny"]]$result
aMarkov_sho <- markov_array_lengths[["Short"]]$result
aMarkov_med <- markov_array_lengths[["Medium"]]$result
aMarkov_lon <- markov_array_lengths[["Long"]]$result
aMarkov_vlo <- markov_array_lengths[["Very Long"]]$result

aMarkov_sin$Length <- "Singular"
aMarkov_tin$Length <- "Tiny"
aMarkov_sho$Length <- "Short"
aMarkov_med$Length <- "Medium"
aMarkov_lon$Length <- "Long"
aMarkov_vlo$Length <- "Very Long"

aMarkov_tpLengths <- rbind(aMarkov_sin, aMarkov_tin, aMarkov_sho, 
                           aMarkov_med, aMarkov_lon, aMarkov_vlo)

aMarkov_tpLengths <- aMarkov_tpLengths %>%
  left_join(touchpoint_groups, by = "Length")

aMarkov_tpLengths$all_conversions <- total_conversions
aMarkov_tpLengths$rate <- aMarkov_tpLengths$total_conversions / aMarkov_tpLengths$conversion_count * 100

aMarkov_tpLengths$Length <- factor(aMarkov_tpLengths$Length,
                                   levels = c("Singular", "Tiny", "Short", "Medium", "Long", "Very Long"))

aMarkov_tpLengths %>%
  ggplot(aes(y = rate, x = Length, colour = channel_name)) +
  geom_point(size = 3) +
  labs(title = "Full Journey Grouped Length Comparison",
       y = "Channel Credit %",
       x = "Journey Length Group")

# -------------------------
# 4.3.2b Collapsed Journeys
# -------------------------

attcol <- read_csv("C:/Uni26_Sem2/MXB348/WIL/MXB348/data/attribution_collapsed.csv")

# ---------------------------
# 4.3.3 Levels of Granularity
# ---------------------------

attgran <- read_csv("C:/Uni26_Sem2/MXB348/WIL/MXB348/data/attribution_granulated.csv")

