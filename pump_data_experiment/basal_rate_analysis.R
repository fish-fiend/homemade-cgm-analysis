
# some pump data analysis for a change

tidepool_basal <- tidepool_new |>
  rename(time = 'Local Time', duration_sec = 'Duration (mins)', rate = Rate, supp_rate = `Suppressed Rate`, payload = Payload) |>
  select(time, duration_sec, rate, supp_rate, payload) |>
  mutate(bool = grepl('"algorithm_rate":null', payload), nah = is.na(supp_rate),
         duration_sec = duration_sec*60)

tidepool_basal$supp_rate[tidepool_basal$bool == TRUE] <-
  tidepool_basal$rate[tidepool_basal$bool]

tidepool_basal$supp_rate[tidepool_basal$nah == TRUE] <-
  tidepool_basal$rate[tidepool_basal$nah]


coordination <- tidepool_basal |>
  select(time, duration_sec, rate, supp_rate) |>
  mutate(match = (supp_rate == rate)) |>
  group_by(match) |>
  summarize(total_secs = sum(as.integer(duration_sec))) |>
  mutate(total_hrs = total_secs/3600, total_days = total_secs/86400) |>
  mutate(percentage = total_secs/sum(total_secs), class = "time")

coordination$match[coordination$match == TRUE] <- "aye"
coordination$match[coordination$match == FALSE] <- "nay"
coordination$match[is.na(coordination$match)] <- "zzz"

coordination <- coordination |>
  arrange(match) |>
  mutate(state = c("Pre-Programmed Basal", "Control IQ Basal Overide", "Pump Error (Delivery Paused)"))


basal_state_total <- ggplot(coordination, aes(x = match, y = total_hrs, fill = match)) +
  geom_col() +
  scale_fill_manual(
    "States",
    values = c("#BC4411", "#ffDD77", "#BBCC55"),
    breaks = c("zzz", "nay", "aye"),
    labels = c("Pump Error (Delivery Paused)", "Control IQ Basal Overide", "Pre-Programmed Basal")
  ) +
  scale_y_continuous(limits = c(0, 160), breaks = seq(0, 180, 20)) +
  labs(
    title = "Time Spent in Alternate Basal States",
    y = "Total Hours"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0), face = "bold"),
    axis.text.x = element_blank(),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(12, 12, 12, 12)
  )

basal_state_total



basal_state_percent <- ggplot(coordination, aes(x = match, y = percentage, fill = match)) +
  geom_col() +
  scale_fill_manual(
    "States",
    values = c("#BC4411", "#ffDD77", "#BBCC55"),
    breaks = c("zzz", "nay", "aye"),
    labels = c("Pump Error (Delivery Paused)", "Control IQ Basal Overide", "Pre-Programmed Basal")
  ) +
  scale_y_continuous(limits = c(0, .65), breaks = seq(0, .7, .10)) +
  labs(
    title = "Time Spent in Alternate Basal States",
    y = "Time (percentage)"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, margin = margin(0, 10, 0, 0), face = "bold"),
    axis.text.x = element_blank(),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(12, 12, 12, 12)
  )

basal_state_percent






# less good methinks
control_basal_bar <- ggplot(coordination, aes(x = class, y = total_hrs, fill = match)) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  scale_fill_manual(
    "Basal Setting",
    values = c("#BC4411", "#ffDD77", "#BBCC55"),
    breaks = c("zzz", "nay", "aye"),
    labels = c("Pump Error (Delivery Paused)", "Control IQ Basal Overide", "Pre-Programmed Basal")
  ) +
  labs(
    y = "Percentage of Time"
  ) + theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_text(size = 16, margin = margin(0, 10, 0, 0)),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 12),

  )

control_basal_bar


