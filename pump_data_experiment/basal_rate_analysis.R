
library(tidyverse)
library(ggplot2)
library(stats)


# prepping the data
tidepool_basal <- tidepool_new |>
  rename(time = 'Local Time', duration_min = 'Duration (mins)',
         actual_rate = Rate, scheduled_rate = `Suppressed Rate`, type = 'Delivery Type') |>
  select(time, duration_min, actual_rate, scheduled_rate, type)

tidepool_basal$scheduled_rate[is.na(tidepool_basal$scheduled_rate)] <-
  tidepool_basal$actual_rate[is.na(tidepool_basal$scheduled_rate)]

tidepool_basal <- tidepool_basal |>
  select(time, duration_min, actual_rate, scheduled_rate) |>
  mutate(match = (scheduled_rate == actual_rate))


################################################################################

daily_basal <- tidepool_basal |>
  mutate(date = substring()) |>
  group_by(date)

ggplot(tidepool_basal, aes(x = time)) +
  geom_line(aes(y = scheduled_rate)) +
  geom_line(aes(y = actual_rate)) +
  theme_bw()


################################################################################

# comparing the ratio of time in which the pre-programmed basal is unchanged,
# overridden, or suspended (manually or as a result of pump error)

coordination <- tidepool_basal |>
  group_by(match) |>
  summarize(total_mins = sum(as.integer(duration_min))) |>
  mutate(total_hrs = total_mins/60, total_days = total_mins/1440) |>
  mutate(percentage = total_mins/sum(total_mins), class = "time") |>
  mutate(match = replace(match, match == TRUE, "Scheduled" )) |>
  mutate(match = replace(match, match == FALSE, "Overidden" )) |>
  mutate(match = replace(match, is.na(match), "Suspended" ))


basal_state_total <- ggplot(coordination, aes(x = match, y = percentage, fill = match)) +
  geom_col(color = "black", linewidth = .25) +
  scale_fill_manual(
    "Type of Delivery",
    values = c("#BC4411", "#ffDD77", "#BBCC55"),
    breaks = c("Suspended", "Overidden", "Scheduled"),
    labels = c("Basal Suspended", "Control IQ Overide", "Scheduled Rate")
  ) +
  labs(
    title = "Ratio of Time Spent in Alternate Basal States",
    y = "Percentage"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 12, margin = margin(0, 10, 0, 0), face = "bold"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.spacing.y = unit(1, "mm"),
    plot.margin = margin(12, 12, 12, 12)
  )

basal_state_total

# conclusion:
# seems like a promising way to track one's dependency on the pump's automation
# and/or to determine whether their pre-programmed basal regimen needs adjusting
# it would be helpful to know what the "normal" amount of control iq activity is


###############################################################################


# comparing the frequency of automatic basal decreases vs increases

more_or_less <- tidepool_basal |>
  filter(match == FALSE) |>
  mutate(difference = replace(match, actual_rate > scheduled_rate, "higher")) |>
  mutate(difference = replace(difference, actual_rate < scheduled_rate, "lower")) |>
  group_by(difference) |>
  summarize(total_mins = sum(as.integer(duration_min))) |>
  mutate(total_hrs = total_mins/60, total_days = total_mins/1440) |>
  mutate(percentage = total_mins/sum(total_mins), class = "time")

more_or_less_plot <- ggplot(more_or_less, aes(x = class, y = percentage, fill = difference)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  theme_bw()

more_or_less_plot

# conclusion:
# this data is complicated by the fact that the algorithm can only correct for
# lows by decreasing the basal rate but for highs it can use automatic boluses
# as well and therefore (i guess) is less likely to fiddle with the basal to
# correct highs. this makes it difficult to come to any real conclusion about
# whether or not one's scheduled basal rates are (generally speaking) too high
# or too low. bummer.


###############################################################################



# comparing mean glucose value during times when the pump isn't connected to
# the cgm vs when it is
bg_data <- tidepool_cgm |>
  rename(time = 'Local Time', bg_value = Value, payload = Payload) |>
  select(time, bg_value, payload) |>
  mutate(backfill = grepl("Backfill", payload)) |>
  group_by(backfill) |>
  summarize(avg = mean(bg_value))


avg_point <- ggplot(bg_data, aes(x = backfill, y = avg, shape = backfill)) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(140, 164), breaks = seq(0, 168, 2)) +
  labs(
    y = "Average CGM Reading (mg/dL)",
    x = "Backfill"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0), face = "bold"),
    axis.title.x = element_text(size = 14, margin = margin(10, 0, 0, 0), face = "bold"),
    plot.margin = margin(12, 12, 12, 12),
    legend.position = "none"
  )

avg_point


full_data <- full_join(tidepool_3, tidepool_cgm)


bg_data <- full_data |>
  rename(time = 'Local Time', bg_value = Value, payload = Payload) |>
  select(time, bg_value, payload) |>
  mutate(backfill = grepl("Backfill", payload)) |>
  group_by(backfill) |>
  summarize(avg = mean(bg_value))


backfill_col <- tibble(backfill = full_data$Value[grepl("Backfill", full_data$Payload) == TRUE])
backfill_col$ID <- seq.int(nrow(backfill_col))

no_backfill_col <- tibble(instant = full_data$Value[grepl("Backfill", full_data$Payload) == FALSE])
no_backfill_col$ID <- seq.int(nrow(no_backfill_col))

all_values <- tibble(all = full_data$Value)
all_values$ID <- seq.int(nrow(all_values))

data_density <- full_join(x = backfill_col, y = no_backfill_col, by = join_by(ID))

data_density <- data_density |>
  full_join(all_values, by = join_by(ID)) |>
  relocate(ID, .before = backfill)



# i don't know if this works
backfill_vec <- full_data$Value[grepl("Backfill", full_data$Payload) == TRUE]
no_backfill_vec <- full_data$Value[grepl("Backfill", full_data$Payload) == FALSE]
wilcox.test(x = backfill_vec, y = no_backfill_vec, conf.level = 0.99,
            alternative = "greater", paired = FALSE)
t.test(x = backfill_vec, y = no_backfill_vec, alternative = "greater", conf.level = 0.99)


# but this definitely does
avg_distribution <- ggplot(data_density) +
  geom_density(aes(x = instant, fill = "blue"), color = "blue", alpha = 0.2) +
  geom_density(aes(x = backfill, fill = "red"), color = "red", alpha = 0.2) +
  geom_vline(color = "blue", linetype = "dashed", xintercept = as.numeric(bg_data$avg[1])) +
  geom_vline(color = "red", linetype = "dashed", xintercept = as.numeric(bg_data$avg[2])) +
  labs(
    x = "Blood Glucose Value (mg/dL)",
    y = "Density",
    title = "Overall Distribution of CGM Readings (9/22/24 to 10/03/24)",
    subtitle = "(differentiated by whether the insulin pump was actively recieving CGM data at the time)"
  ) +
  scale_fill_manual(
    "Timing",
    values = c("blue", "red"),
    labels = c("Instantaneous", "Backfilled")
  ) +
  scale_x_continuous(
    breaks = seq(50, 400, 50),
    minor_breaks = c(as.numeric(bg_data$avg[1]), as.numeric(bg_data$avg[2]))
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 12, margin = margin(10, 0, 0, 0), face = "bold"),
    plot.margin = margin(12, 12, 12, 12),
    legend.key.spacing.y = unit(1, "mm")
  )

avg_distribution


