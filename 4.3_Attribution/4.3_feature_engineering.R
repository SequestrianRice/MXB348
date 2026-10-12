# DATA CLEANING
# =============

# --------
# Packages
# --------
library(ChannelAttribution)
library(tidyverse)
library(markovchain)
library(data.table)
library(reshape2)
library(igraph)
library(RColorBrewer)

# ----
# Data
# ----
journey <- read_csv("C:/Uni26_Sem2/MXB348/WIL/MXB348/data/customer_journey-4.csv")
results <- read_csv("C:/Uni26_Sem2/MXB348/WIL/MXB348/data/results-3.csv")

## FEATURE ENGINEERING
# path with same order id into 1 column.
# can look at frequency of each path.
# group into journey length too.
# duration time + grouped too.
# compare with collapsed group.
# looking at total conversion rate and then provide weighting to EACH CHANNEL.

# direction
# no grouping
# grouping - time, channels, collapsing.

# -------------------
# Feature Engineering
# -------------------

# CHANNEL NAME SYSTEM
journeyA <- journey %>%
  mutate(channel = fct_recode(channel,
                              "GD" = "Google Display",
                              "GS" = "Google Search",
                              "FO" = "Organic",
                              "FD" = "Direct",
                              "GY" = "Youtube",
                              "MI" = "Instagram",
                              "MF" = "Facebook",
                              "FE" = "Email Marketing")) # Codename: 1st Letter = Group, 2nd Letter = Channel

# CHANNEL JOURNEY (WHOLE)
journeyA <- setDT(journeyA)
journeyA_wide = data.table::dcast(data = journeyA, formula = user_id + session_id ~ order, value.var = "channel")
# Clarify that dcast is from "data.table". Assumes "reshape2" if both installed.
journeyA_wide = journeyA_wide[, path:=do.call(paste,c(.SD, sep=" > ")), .SDcols=-c(1,2)]
journeyA_wide = journeyA_wide[, path:=word(path, 1, sep = " > NA")]
journeyA_wide <- journeyA_wide %>% select(-c("0":"25"))

# FIRST + LAST CHANNEL
journeyA_wide <- journeyA_wide %>% 
  mutate("first_touchpoint" = str_extract(path, "^.."),
         "last_touchpoint" = str_extract(path, "..$"))

# TOTAL JOURNEY TIME
journeyA <- journeyA %>%
  filter(order == 0) %>%
  select(-c("order", "channel"))

merge1 <- inner_join(journeyA_wide, journeyA, by = c("user_id", "session_id"))
attribution_v1 <- inner_join(merge1, results, by = c("user_id", "session_id"))

# TOTAL TOUCHPOINT COUNT
n_touchpoint <- attribution_v1$path %>%
  str_count(">") + 1

# Make a new column in our dataset
attribution_v1$touchpoint_count <- n_touchpoint

# TOTAL CONVERSIONS (from first to current customer journey)
attribution_v1 <- attribution_v1 %>%
  group_by(user_id) %>%
  mutate(customer_conversion_count = cumsum(conversion == 1),
         customer_journey_count = session_id + 1,
         customer_conversion_rate = round(customer_conversion_count/customer_journey_count, 3),
         returning = n() > 1) %>%
  ungroup()

# NO CONVERSION (For Markov Model)
attribution_v2 <- attribution_v1 %>%
  mutate(null_conversion = 1 - conversion)

# --------------
# ATTRIBUTION V3
# --------------
  # NEW COLUMNS: count for each touchpoint per customer journey
  # FACTORISATION: touch point count factorised + grouping
  attribution_v3 <- attribution_v2

# Count for each channel in path
channels <- c("GD", "GS", "FO", "MI", "MF", "FD", "FE", "GY")

for (channel in channels) {
  attribution_v3[[channel]] <- str_count(attribution_v3$path, paste0("\\b", channel, "\\b"))
}

# ----------------
# ATTRIBUTION V3.1
# ----------------
  # NEW COLUMNS: categories for journey length
  # FACTORISATION: removed factorisation for journey length

attribution_v3.1 <- attribution_v3 %>%
  mutate(touchpoint_length = case_when(
    touchpoint_count ==   1                 ~ "Singular",
    touchpoint_count %in% c(2,3)            ~ "Tiny",
    touchpoint_count %in% c(4,5,6)          ~ "Short",
    touchpoint_count %in% c(7,8,9,10)       ~ "Medium",
    touchpoint_count %in% c(11,12,13,14,15) ~ "Long",
    touchpoint_count >    15                ~ "Very Long"))

lengths <- c("Singular", "Tiny", "Short", "Medium", "Long", "Very Long")

# REMOVE THINGS
rm(attribution_v1, attribution_v2, attribution_v3, journeyA, journeyA_wide, merge1)

# -----------------------------------------------------------
# ALTERNATIVE DATAFRAMES (for collapsed and granulated paths)
# -----------------------------------------------------------

# Function for collapsing runs (AI Generated)
collapse_runs <- function(x) {
  x <- str_trim(x)
  x <- x[x != ""]
  x[c(TRUE, x[-1] != x[-length(x)])]
}

# COLLAPSED
# ---------

attcol_v3.1 <- attribution_v3.1 %>%
  mutate(path_col = map_chr(
    str_split(path, "\\s*>\\s*"),
    ~ str_c(collapse_runs(.x), collapse = " > "))) %>%
  select(-path, -touchpoint_count,
         -GD, -GS, -FO, -MI, -MF, -FD, -FE, -GY,
         -touchpoint_length)

n_touchpoint <- attcol_v3.1$path_col %>%
  str_count(">") + 1

attcol_v3.1$touchpoint_count <- n_touchpoint

for (channel in channels) {
  attcol_v3.1[[channel]] <- str_count(attcol_v3.1$path_col, paste0("\\b", channel, "\\b"))
}

attcol_v3.1 <- attcol_v3.1 %>%
  mutate(touchpoint_length = case_when(
    touchpoint_count ==   1                 ~ "Singular",
    touchpoint_count %in% c(2,3)            ~ "Tiny",
    touchpoint_count %in% c(4,5,6)          ~ "Short",
    touchpoint_count %in% c(7,8,9,10)       ~ "Medium",
    touchpoint_count %in% c(11,12,13,14,15) ~ "Long",
    touchpoint_count >    15                ~ "Very Long"))

attcol_v3.1 <- attcol_v3.1 %>%
  relocate(path_col, .before = first_touchpoint) %>%
  relocate(touchpoint_count, .before = customer_conversion_count) %>%
  relocate(touchpoint_length, .before = customer_conversion_count)

# GRANULATED
# ----------

channel_groups <- c(GD = "Google",
                    GS = "Google",
                    FO = "Free",
                    MI = "Meta",
                    MF = "Meta",
                    FD = "Free",
                    FE = "Free",
                    GY = "Google")

channels_granulated <- c("Free", "Google", "Meta")

attgran_v3.1 <- attribution_v3.1 %>%
  mutate(path_gran = map_chr(
  str_split(path, "\\s*>\\s*"),
  ~ str_c(collapse_runs(unname(channel_groups[str_trim(.x)])),
          collapse = " > "))) %>%
  select(-path, -touchpoint_count,
         -GD, -GS, -FO, -MI, -MF, -FD, -FE, -GY,
         -touchpoint_length)

n_touchpoint <- attgran_v3.1$path_gran %>%
  str_count(">") + 1

attgran_v3.1$touchpoint_count <- n_touchpoint

for (channel in channels_granulated) {
  attgran_v3.1[[channel]] <- str_count(attgran_v3.1$path_gran, paste0("\\b", channel, "\\b"))
}

attgran_v3.1 <- attgran_v3.1 %>%
  mutate(touchpoint_length = case_when(
    touchpoint_count ==   1                 ~ "Singular",
    touchpoint_count %in% c(2,3)            ~ "Tiny",
    touchpoint_count %in% c(4,5,6)          ~ "Short",
    touchpoint_count %in% c(7,8,9,10)       ~ "Medium",
    touchpoint_count %in% c(11,12,13,14,15) ~ "Long",
    touchpoint_count >    15                ~ "Very Long"))

attgran_v3.1 <- attgran_v3.1 %>%
  relocate(path_gran, .before = first_touchpoint) %>%
  relocate(touchpoint_count, .before = customer_conversion_count) %>%
  relocate(touchpoint_length, .before = customer_conversion_count)

# things i know need to be improved -- renaming first and last touchpoints

# =========
# SAVE DATA
# =========

write.csv(attribution_v3.1, "C:/Uni26_Sem2/MXB348/WIL/MXB348/data/attribution_full.csv", row.names=FALSE)
write.csv(attcol_v3.1, "C:/Uni26_Sem2/MXB348/WIL/MXB348/data/attribution_collapsed.csv", row.names=FALSE)
write.csv(attgran_v3.1, "C:/Uni26_Sem2/MXB348/WIL/MXB348/data/attribution_granulated.csv", row.names=FALSE)

print("All done, stay swag")
