# TESTTESTTESTTEST
library(ChannelAttribution)
library(tidyverse)
# More Specifically
library(dplyr)
library(lubridate)
library(stringr)
library(markovchain)
library(data.table)
library(reshape2)
library(igraph)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)


# calculating the models (Markov and heuristics)
mod2 <- markov_model(attribution_v2,
                     var_path = 'path',
                     var_conv = 'conversion',
                     var_null = 'null_conversion',
                     out_more = TRUE)

# heuristic_models() function doesn't work for me, therefore I used the manual calculations
# instead of:
# h_mod2 <- heuristic_models(attribution_v2, var_path = 'path', var_conv = 'conversion')

df_hm <- attribution_v2 %>%
  mutate(channel_name_ft = sub('>.*', '', path),
         channel_name_ft = sub(' ', '', channel_name_ft),
         channel_name_lt = sub('.*>', '', path),
         channel_name_lt = sub(' ', '', channel_name_lt))
# first-touch conversions
df_ft <- df_hm %>%
  group_by(channel_name_ft) %>%
  summarise(first_touch_conversions = sum(conversion)) %>%
  ungroup()
# last-touch conversions
df_lt <- df_hm %>%
  group_by(channel_name_lt) %>%
  summarise(last_touch_conversions = sum(conversion)) %>%
  ungroup()
h_mod2 <- merge(df_ft, df_lt, by.x = 'channel_name_ft', by.y = 'channel_name_lt')

# merging all models
all_models <- merge(h_mod2, mod2$result, by.x = 'channel_name_ft', by.y = 'channel_name')
colnames(all_models)[c(1, 4)] <- c('channel_name', 'attrib_model_conversions')

############## visualizations ##############
# transition matrix heatmap for "real" data
df_plot_trans <- mod2$transition_matrix

df_plot_trans <- df_plot_trans %>%
  mutate(channel_from = fct_recode(channel_from,
                                   "GD" = "1",
                                   "GS" = "2",
                                   "FO" = "3",
                                   "FD" = "4",
                                   "GY" = "5",
                                   "MI" = "6",
                                   "MF" = "7",
                                   "FE" = "8"),
         channel_to = fct_recode(channel_to,
                                 "GD" = "1",
                                 "GS" = "2",
                                 "FO" = "3",
                                 "FD" = "4",
                                 "GY" = "5",
                                 "MI" = "6",
                                 "MF" = "7",
                                 "FE" = "8"))

cols <- c("#e7f0fa", "#c9e2f6", "#95cbee", "#0099dc", "#4ab04a", "#ffd73e", "#eec73a",
          "#e29421", "#e29421", "#f05336", "#ce472e")
t <- max(df_plot_trans$transition_probability)

ggplot(df_plot_trans, aes(y = channel_from, x = channel_to, fill = transition_probability)) +
  theme_minimal() +
  geom_tile(colour = "white", width = .9, height = .9) +
  scale_fill_gradientn(colours = cols, limits = c(0, t),
                       breaks = seq(0, t, by = t/4),
                       labels = c("0", round(t/4*1, 2), round(t/4*2, 2), round(t/4*3, 2), round(t/4*4, 2)),
                       guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
  geom_text(aes(label = round(transition_probability, 2)), fontface = "bold", size = 4) +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 24, face = "bold"),
        axis.text.y = element_text(size = 8, face = "bold", color = 'black'),
        axis.text.x = element_text(size = 8, angle = 90, hjust = 0.5, vjust = 0.5, face = "plain")) +
  ggtitle("Transition matrix heatmap")

# models comparison
all_mod_plot <- melt(all_models, id.vars = 'channel_name', variable.name = 'conv_type')
all_mod_plot$value <- round(all_mod_plot$value)
# slope chart
pal <- colorRampPalette(brewer.pal(10, "Set1"))
ggplot(all_mod_plot, aes(x = conv_type, y = value, group = channel_name)) +
  theme_solarized(base_size = 18, base_family = "", light = TRUE) +
  scale_color_manual(values = pal(10)) +
  scale_fill_manual(values = pal(10)) +
  geom_line(aes(color = channel_name), size = 2.5, alpha = 0.8) +
  geom_point(aes(color = channel_name), size = 5) +
  geom_label_repel(aes(label = paste0(channel_name, ': ', value), fill = factor(channel_name)),
                   alpha = 0.7,
                   fontface = 'bold', color = 'white', size = 5,
                   box.padding = unit(0.25, 'lines'), point.padding = unit(0.5, 'lines'),
                   max.iter = 100) +
  theme(legend.position = 'none',
        legend.title = element_text(size = 16, color = 'black'),
        legend.text = element_text(size = 16, vjust = 2, color = 'black'),
        plot.title = element_text(size = 20, face = "bold", vjust = 2, color = 'black', lineheight = 0.8),
        axis.title.x = element_text(size = 24, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(size = 16, face = "bold", color = 'black'),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16, hjust = 0.5, vjust = 0.5, face = "bold", color = 'black'),
        strip.background = element_rect(fill = "#f0b35f")) +
  labs(x = 'Model', y = 'Conversions') +
  ggtitle('Models comparison') +
  guides(colour = guide_legend(override.aes = list(size = 4)))
