library(tidyverse)
library(ggplot2)


bar_time <- averages()

bar_time <- bar_time |>
  mutate(class = "time", range = value) |>
  mutate(range = replace(range, range > 250, 3)) |>
  mutate(range = replace(range, range > 180 & range != 3, 2 )) |>
  mutate(range = replace(range, range <= 180 & range >= 80, 1 )) |>
  mutate(range = replace(range, range < 80 & range != 2 & range != 1 & range != 3, 0)) |>
  mutate(range = as.character(range))
 

range_time <- ggplot(bar_time, aes(x = class, y = value, fill = range)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  scale_fill_manual(
    values = c("#BC4411","#ffcc66", "#99CCFF", "#CD99CE" ), 
    breaks = c("3", "2", "1", "0"),
    labels = c(
      "Very High (> 250 mg/dL)",
      "High (> 180 mg/dL)", 
      "In Range",
      "Low (< 80 mg/dL)")
  ) +
  labs(
    caption = "Overall Time in Range ",
    x = NULL,
    y = NULL
  ) +
  theme_lcars_dark() +
  theme(
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 15)),
    legend.text = element_text(size = 12),
    axis.text.x = element_blank(),
    plot.margin = margin(28, 28, 28, 32),
  )

range_time
