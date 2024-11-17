library(tidyverse)
library(ggplot2)
library(splines)
library(hpfilter)

# comparing different methods for smoothing the daily averages
# hodrick-prescott wins (as expected)


reshape <- function(cgm_data) {
  cgm_data[-(1:19),] |>
    mutate(date = as.Date(...2, '%Y-%m-%d'), value = as.numeric(...8)) |>
    select(date, value)
}


clarity_01_21_04_19$...8[clarity_01_21_04_19$...8 == 'High'] <- '400'
clarity_01_21_04_19$...8[clarity_01_21_04_19$...8 == 'Low'] <- '40'

clarity_04_20_07_17$...8[clarity_04_20_07_17$...8 == 'High'] <- '400'
clarity_04_20_07_17$...8[clarity_04_20_07_17$...8 == 'Low'] <- '40'

clarity_10_23_01_20$...8[clarity_10_23_01_20$...8 == 'High'] <- '400'
clarity_10_23_01_20$...8[clarity_10_23_01_20$...8 == 'Low'] <- '40'

oct23_jan20 <- reshape(clarity_10_23_01_20)
jan21_apr19 <- reshape(clarity_01_21_04_19)
apr20_jul17 <- reshape(clarity_04_20_07_17)

clarity_data <- apr20_jul17 |>
  full_join(jan21_apr19, join_by(date, value)) |>
  full_join(oct23_jan20, join_by(date, value))

clarity_avgs <- clarity_data |>
  group_by(date) |>
  summarize(daily_avg = mean(value))


# basic cubic spline creation
m1 <- lm(daily_avg ~ bs(date, df = ), data = clarity_avgs)
daily_averages_m1 <- mutate(clarity_avgs, cubic = fitted(m1))

spline_plot <- ggplot(daily_averages_m1, aes(date, daily_avg)) +
  geom_line(aes(y = daily_avg), color = "#cc99cc") +
  geom_line(aes(date, cubic), color = "#cc6699", linewidth = 0.9) +
  theme_bw()

spline_plot


# hp time
hped <- hp2(clarity_avgs, lambda = 1000)
daily_averages_hp <- mutate(clarity_avgs, hp = hped$daily_avg)

hp_plot <- ggplot(daily_averages_hp, aes(x = date)) +
  geom_line(aes(y = daily_avg), color = "#cc99cc") +
  geom_line(aes(y = hp), color = "#cc6699", linewidth = 0.9) +
  theme_bw()

hp_plot


# compare and contrast
combo <- mutate(daily_averages_hp, cubic = daily_averages_m1$cubic)

combo_plot <- ggplot(combo, aes(x = date)) +
  geom_line(aes(y = daily_avg), color = "#cc99cc") +
  geom_line(aes(y = hp), color = "#cc6699", linewidth = 0.9) +
  geom_line(aes(y = cubic), color = "#99e", linewidth = 0.9) +
  theme_bw()

combo_plot


