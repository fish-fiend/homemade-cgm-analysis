library(stats)
library(tidyverse)


reshape <- function(x) {
  x[-(1:19),] |>
    mutate(date = as.Date(...2, '%Y-%m-%d'), value = as.numeric(...8)) |>
    select(date, value) |>
    filter(!is.na(value))
}

highs <- function(x) {
  mutate(x, ...8 = replace(...8, ...8 == "High", "400" ))
}

lows <- function(x) {
  mutate(x, ...8 = replace(...8, ...8 == "Low", "40" ))
}

cgm_data <- clarity_0420_0717

cgm_averages <- cgm_data |>
  highs() |>
  lows() |>
  reshape() |>
  group_by(date) |>
  summarize(daily_avg = mean(value))


# cubic spline? is that something??

m1 <- lm(daily_avg ~ bs(date, df = 12), data = daily_averages)
daily_averages_m1 <- mutate(daily_averages, cubic = fitted(m1))

spline_plot <- ggplot(daily_averages_m1, aes(date, daily_avg)) +
  geom_line(aes(y = daily_avg), color = "#cc99cc", linewidth = 0.9) +
  geom_line(aes(date, cubic), color = "#cc6699", linewidth = 0.9) +
  theme_bw()

spline_plot





