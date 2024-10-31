library(tidyverse)
library(ggplot2)
library(lcars)

violin_data <- clarity_data |>
  group_by(date) |>
  arrange(desc(date)) |>
  mutate(value = as.numeric(value), date = as.factor(date))
  
violin_data <- box_data[(1:2000),]

glycemic_var <- ggplot(violin_data, aes(x = date, y = value, fill = date)) +
  geom_violin() +
  labs(
    title = "Daily Glycemic Variation",
    y = "Glucose Value (mg/dL)",
    x = "Date"
  ) +
  scale_y_continuous(limits = c(55, 375), breaks = seq(40, 400, 25)) +
  theme_lcars_dark() +
  theme(
    plot.title = element_text(size = 17),
    axis.title = element_text(size = 12),
    plot.margin = margin(15, 30, 15, 15),
    legend.position = "none"
  ) 
