## Attribution1 -- Probabilistic Model

# Packages Used
library(tidyverse)

# Data
df <- attribution_v2

# GOAL
  # C_(x_i) =
    # P(y|x_i) + 1/2N_(i =/= j) SUM(P(y|x_i =/= j_i) - P(y|x_i) - P(y|x_j))

## PROBABILISTIC MODEL
# Step 1 -- calculate probabilties
channels <- c("GD", "GS", "FO", "MI", "MF", "FD", "FE", "GY")

# Data Frame setup
prob <- data.frame(
  channeli = character(),
  channelj = character(),
  num = integer(),
  den = integer(),
  P = integer(),
  stringsAsFactors = FALSE
)

for (channeli in channels) {        # Main channel
  for (channelj in channels) {      # Pair-wise channel
    
    # One channel
    if (channeli == channelj) {
      
      # Numerator
      num <- df %>%
        summarise(
          count = sum(
            grepl(channeli, path) & conversion == 1, na.rm = TRUE)) %>%
        pull(count)
      
      # Denominator
      den <- df %>%
        summarise(
          count = sum(
            grepl(channeli, path), na.rm = TRUE)) %>%
        pull(count)
      
      # Probability of a conversion, y = 1
      P <- num/den
      
      # Update dataframe
      prob <- rbind(
        prob,
        data.frame(
          channeli = channeli,
          channelj = channelj,
          num = num,
          den = den,
          P = P
        )
      )
    }
    
    # Pair-wise channels
    else {
      
      # Numerator
      num <- df %>%
        summarise(
          count = sum(
            grepl(channeli, path) &
              grepl(channelj, path) &
              conversion == 1,
            na.rm = TRUE
          )
        ) %>%
        pull(count)
      
      # Denominator
      den <- df %>%
        summarise(
          count = sum(
            grepl(channeli, path) &
              grepl(channelj, path),
            na.rm = TRUE
          )
        ) %>%
        pull(count)
      
      # Probability of a conversion, y = 1
      P <- num/den
      
      # Update dataframe
      prob <- rbind(
        prob,
        data.frame(
          channeli = channeli,
          channelj = channelj,
          num = num,
          den = den,
          P = P
        )
      )
    }
  }
}

# Step 2 -- contribution of channel i

# Dataframe manipulation
prob <- prob %>%
  select(-num, -den)

forC <- pivot_wider(
  prob,
  names_from = channelj,
  values_from = P
)

# Calculating P(y|x_i)
forC$P_i <- mapply(
  function(row, col) forC[[col]][row],
  seq_len(nrow(forC)),
  as.character(forC$channeli)
)

# Calculating SUM(P(y|x_i, x_j))
forC$sumP_ij <- abs(rowSums(forC[c(
  "GD", "GS", "FO", "MI", "MF", "FD", "FE", "GY")]) - forC$P_i)

# Calculating SUM(P(y|x_j))
forC$sumP_j <- sum(forC$P_i) - forC$P_i

# Calculating C -- channel contribution
forC$C <-
  forC$P_i + 1/(2 * (nrow(forC) - 1)) * (forC$sumP_ij -
                                     (nrow(forC) - 1) * forC$P_i -
                                     forC$sumP_j)

# Ensuring C column add up to 1
forC$C_fixed <- forC$C / sum(forC$C)







