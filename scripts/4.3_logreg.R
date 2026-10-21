## ATTRIBUTION -- LOGISTIC REGRESSION

# Packages Used
library(tidyverse)
library(ChannelAttribution)

# -------------------------------------
# Dataset Formation
# -------------------------------------
Data <- read_csv("C:/Uni26_Sem2/MXB348/WIL/MXB348/data/attribution_full.csv")

Data_Log <- Data %>%
  select(-first_touchpoint,
         -last_touchpoint,
         -time_till_session_end,
         -customer_conversion_count,
         -customer_journey_count,
         -customer_conversion_rate,
         -null_conversion,
         -returning,
         -touchpoint_length)

# -------------------------------------
# 4.3.1b Channel Conversion Attribution
# -------------------------------------
aLog <- glm(conversion ~ GD + GS + FO + MI + MF + FD + FE + GY,
                 data = Data,
                 family = binomial(link = "logit"))
summary(aLog)

# Significant channels: Search, Organic, Instagram, Facebook, Email

coefficient_names <- c("Intercept", "Display", "Seach",
                       "Organic", "Instagram", "Facebook",
                       "Direct", "Email", "Youtube")

coefficient_names_abv <- c("B0", "GD", "GS", "FO", "MI", "MF", "FD", "FE", "GY")

significant <- c(1, 0, 1, 1, 1 ,1, 0, 1, 0)

aLog_coefficients <- data.frame(name = coefficient_names,
                                name_abv = coefficient_names_abv,
                                coefficient = aLog$coefficients, 
                                significant)

aLog_coefficients <- aLog_coefficients[-1, ] # Removes Intercept Row

# Insignificant channels do not have a coefficient [Display, Direct, Youtube]
aLog_coefficients$coefficient[aLog_coefficients$significant == 0] <- 0 

# Breakdown of math
  # Use journey 5: Display > Search > Facebook [Conversion, y_i = 1]

B_0 = 0.392660 # Intercept coefficient

# x = B_0 + B_Ck
# p_ij_hat == path channel count * (1 + exp(-x))^(-1)
Data_Log$GD <- Data_Log$GD * (1 + exp(-(B_0 + aLog_coefficients$coefficient[1])))^(-1)
Data_Log$GS <- Data_Log$GS * (1 + exp(-(B_0 + aLog_coefficients$coefficient[2])))^(-1)
Data_Log$FO <- Data_Log$FO * (1 + exp(-(B_0 + aLog_coefficients$coefficient[3])))^(-1)
Data_Log$MI <- Data_Log$MI * (1 + exp(-(B_0 + aLog_coefficients$coefficient[4])))^(-1)
Data_Log$MF <- Data_Log$MF * (1 + exp(-(B_0 + aLog_coefficients$coefficient[5])))^(-1)
Data_Log$FD <- Data_Log$FD * (1 + exp(-(B_0 + aLog_coefficients$coefficient[6])))^(-1)
Data_Log$FE <- Data_Log$FE * (1 + exp(-(B_0 + aLog_coefficients$coefficient[7])))^(-1)
Data_Log$GY <- Data_Log$GY * (1 + exp(-(B_0 + aLog_coefficients$coefficient[8])))^(-1)

Data_Log <- Data_Log %>%
  mutate(attribution_sum = rowSums(.[6:13]))

# a_ij_hat == p_ij_hat / SUM(p_ir_hat) * y_i [= 1 or 0]
Data_Log$GD <- Data_Log$GD / Data_Log$attribution_sum * Data_Log$conversion
Data_Log$GS <- Data_Log$GS / Data_Log$attribution_sum * Data_Log$conversion
Data_Log$FO <- Data_Log$FO / Data_Log$attribution_sum * Data_Log$conversion
Data_Log$MI <- Data_Log$MI / Data_Log$attribution_sum * Data_Log$conversion
Data_Log$MF <- Data_Log$MF / Data_Log$attribution_sum * Data_Log$conversion
Data_Log$FD <- Data_Log$FD / Data_Log$attribution_sum * Data_Log$conversion
Data_Log$FE <- Data_Log$FE / Data_Log$attribution_sum * Data_Log$conversion
Data_Log$GY <- Data_Log$GY / Data_Log$attribution_sum * Data_Log$conversion

aaa <- colSums(Data_Log[,6:13])
aaa <- aaa / sum(aaa)
show(aaa)
