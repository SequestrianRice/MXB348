
# Exploratory Analysis

# load packages
library(dplyr)
library(ggplot2)
library(scales)

# load customer data
journey <- read.csv("customer_journey-4.csv")
results <- read.csv("results-3.csv")

# combine journey and results datasets
journeys <- journey %>%
  left_join(results,
            by = c("user_id", "session_id"));
journeys <- journeys %>%
  arrange(user_id,
          session_id,
          order)

# session-level dataset
sessions <- journeys %>%
  group_by(user_id, session_id) %>%
  summarise(touchpoints = max(order) + 1,
            conversion = max(conversion),
            .groups = "drop")

# customer level dataset
users <- sessions %>%
  group_by(user_id) %>%
  summarise(sessions = n(),
            conversions = sum(conversion),
            .groups = "drop") %>%
  mutate(conversion_group = case_when(
    conversions == 0 ~ "0 conversions",
    conversions <= 4 ~ "1–4 conversions",
    TRUE ~ "5+ conversions"))

# proportion of return session that make conversion
return_sessions <- sessions %>%
  filter(session_id > 0)
return_conversion_rate <- mean(return_sessions$conversion)
# 58.47%

# proportion of first session that make conversion
first_sessions <- sessions %>%
  filter(session_id == 0)
first_conversion_rate <- mean(first_sessions$conversion)
# 60.50%

# session count vs touchpoint count stacked bar chart
ggplot(sessions, aes(x = touchpoints, fill = factor(conversion))) +
  geom_bar() +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"),
                    labels = c("0" = "No-conversion", "1" = "Conversion"),
                    name = "Session Outcome") +
  labs(x = "Number of Touchpoints",
       y = "Number of Sessions",
       title = "Session Count by Number of Touchpoints") +
  theme_minimal()

# make dataset containing steps until conversion for converting sessions
conversion_steps <- journeys %>%
  group_by(user_id, session_id) %>%
  filter(max(conversion) == 1) %>%
  mutate(conversion_order = max(order),
         steps_to_conversion = conversion_order - order) %>%
  ungroup()

# calculate empirical mean steps until conversion by channel
empirical_steps <- conversion_steps %>%
  group_by(channel) %>%
  summarise(visits = n(),
            mean_steps = mean(steps_to_conversion),
            sd_steps = sd(steps_to_conversion),
            .groups = "drop") %>%
  arrange(mean_steps)

# customer count by session count
ggplot(users, aes(x = sessions, fill = conversion_group)) +
  geom_bar() +
  labs(x = "Number of Sessions",
       y = "Number of Customers",
       fill = "Conversions",
       title = "Customer Count by Number of Sessions and Conversion") +
  theme_minimal()

# customer conversion rate
customer_conversion <- users %>%
  summarise(customers = n(),
            converting_customers = sum(conversions > 0),
            conversion_rate = mean(conversions > 0))
# 98.55%

# session conversion rate
session_conversions <- sessions %>%
  summarise(sessions = n(),
            converting_sessions = sum(conversion),
            conversion_rate = mean(conversion))
# 58.76%

# conversion proportion by session touchpoint count
position_conversion <- journeys %>%
  group_by(order) %>%
  summarise(visits = n(),
            converting = sum(conversion),
            conversion_rate = mean(conversion),
            .groups = "drop")

ggplot(position_conversion,
       aes(x = order, y = conversion_rate)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent) +
  labs(x = "Session Touchpoint Count",
       y = "Conversion Proportion",
       title = "Conversion proportion by Session Touchpoint Count") +
  theme_minimal()

# channel visits by converting and non-converting session
channel_visits <- journeys %>%
  group_by(channel, conversion) %>%
  summarise(visits = n(),
            .groups = "drop")

# graph of channel visit by session count and outcome
ggplot(channel_visits,
       aes(x = reorder(channel, visits),
           y = visits,
           fill = factor(conversion))) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"),
                    labels = c("0" = "Non-converting journey",
                               "1" = "Converting journey"),
                    name = "Journey outcome") +
  labs(x = "Channel",
       y = "Number of Visits",
       title = "Channel Visit Count by Session Outcome") +
  theme_minimal()
