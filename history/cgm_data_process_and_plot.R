library(tidyverse)
library(ggplot2)
library(zoo)

# earlier version for the code of the plot that the app is based around
# outdated but interesting

# conjunction junction whats ur function
  reshape <- function(cgm_data) {
    cgm_data[-(1:19),] |>
      mutate(date = as.Date(...2, '%Y-%m-%d'), value = as.numeric(...8)) |>
      select(date, value)
  }


# converting 'High' and 'Low' to '400' and '40'
# i feel like i should be able to make this into a function but idk how yet
  clarity_export_90$...8[clarity_export_90$...8 == 'High'] <- '400'
  clarity_export_90$...8[clarity_export_90$...8 == 'Low'] <- '40'

  clarity_0420_0717$...8[clarity_0420_0717$...8 == 'High'] <- '400'
  clarity_0420_0717$...8[clarity_0420_0717$...8 == 'Low'] <- '40'

  clarity_0121_04_19$...8[clarity_0121_04_19$...8 == 'High'] <- '400'
  clarity_0121_04_19$...8[clarity_0121_04_19$...8 == 'Low'] <- '40'

  clarity_1023_0120$...8[clarity_1023_0120$...8 == 'High'] <- '400'
  clarity_1023_0120$...8[clarity_1023_0120$...8 == 'Low'] <- '40'


# making data graphable, removing irrelevant data
  jul18_oct15 <- reshape(clarity_export_90)

  apr20_jul17 <- reshape(clarity_0420_0717)

  jan21_apr19 <- reshape(clarity_0121_04_19)

  oct23_jan20 <- reshape(clarity_1023_0120)


# joininggg
# probably will become irrelevant if i actually turn this into a usable app
# users could only upload one dataset at a time
  clarity_data <- jul18_oct15 |>
    full_join(apr20_jul17, join_by(date, value)) |>
    full_join(jan21_apr19, join_by(date, value)) |>
    full_join(oct23_jan20, join_by(date, value))


# calculate and record averages
  clarity_avgs <- clarity_data |>
    group_by(date) |>
    summarize(daily_avg = mean(value))

  total_avg <- round(mean(as.vector(clarity_avgs$daily_avg)))


# moving averages
  daily_averages <- clarity_avgs |>
    mutate(ma_12 = rollmean(daily_avg, k = 12, fill = NA, align = "right")) |>
    filter(!is.na(ma_12))

# cubic spline?

  m1 <- lm(daily_avg ~ bs(date, df = 12), data = daily_averages)
  daily_averages_m1 <- mutate(daily_averages, cubic = fitted(m1))

  spline_plot <- ggplot(daily_averages_m1, aes(date, daily_avg)) +
    geom_line(aes(y = daily_avg), color = "#cc99cc", linewidth = 0.9) +
    geom_line(aes(date, cubic), color = "#cc6699", linewidth = 0.9) +
    theme_bw()

  spline_plot



# create variable date range for the graph
  first_date <- daily_averages$date[1] - 4

  daily_averages_inverse <- arrange(daily_averages, desc(date))
  last_date <- daily_averages_inverse$date[1] + 4



# graph time
  averages_plot <- ggplot(daily_averages_m1, aes(x = date)) +
    annotate(
      "rect",
      xmin = as.Date(first_date), xmax = as.Date(last_date),
      ymin = 100, ymax = 170,
      fill = "#99CCFF",
      alpha = 0.25
    ) +
    annotate(
      "rect",
      xmin = as.Date(first_date), xmax = as.Date(last_date),
      ymin = 170, ymax = 320,
      fill = "#ffcc66",
      alpha = 0.25
    ) +
    geom_line(aes(y = daily_avg), color = "#cc99cc") +
    geom_line(aes(y = ma_12), color = "#cc6699") +
    geom_line(aes(y = cubic), color = "#99e", linewidth = 0.92) +
    labs(
      title = "Average Daily Blood Glucose",
      subtitle = "11/4/23 to 10/15/24",
      x = "Date",
      y = "Value (mg/dL)"
    ) +
    scale_x_date(date_breaks = "months", date_labels = "%b") +
    scale_y_continuous(limits = c(100, 320), breaks = seq(100,320,20)) +
    scale_color_manual(
      "Method",
      values = c("#cc6699", "#cc99cc", "#99e"),
      labels = c("Moving Average", "Plain Daily Average", "Spline")
      )


  averages_plot + theme_bw()
