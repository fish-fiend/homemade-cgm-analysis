
library(tidyverse)
library(ggplot2)
library(lubridate)
library(stringr)


theme_news <- function() {
  theme(
    panel.grid.major = element_line(linewidth = 0.1, color = "#1a1c1a"),
    panel.grid.minor = element_line(linewidth = 0.05, color = "#444444"),
    panel.border = element_rect(linewidth = 0.75),
    panel.background = element_rect(fill = "#f0ecdf"),
    plot.margin = margin(14, 14, 14, 14),
    plot.background = element_rect(fill = "#f0ecdf", color = "#1a1c1a", linewidth = 1.2),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0), hjust = 0.5),
    text = element_text(family = "serif"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0))
  )
}

clarity_10_23_01_20 <- clarity_10_23_01_20[-(1:19),]



# removing the random and very annoying T from all the date/time values
clarity_10_23_01_20$...2 <- str_replace_all(clarity_10_23_01_20$...2, "T", " ")

# in the app, time1 will be given by timeInput
# for now, using randomly selected row
time1 <- clarity_10_23_01_20$...2[666]
time2 <- as.character(as.POSIXct(time1) + hours(24))


clarity <- clarity_10_23_01_20 |>
  filter(...2 >= time1 & ...2 <= time2) |>
  rename(time = ...2, value = ...8) |>
  mutate(time = as.POSIXct(time), value = as.numeric(value))

extrema <- c(max(clarity$value), min(clarity$value))

avg <- mean(clarity$value)

capt <- paste0("Highest Value = ", extrema[1], "      Lowest Value = ", extrema[2])


daily_overview <- ggplot() +
  geom_hline(aes(yintercept = 180), linetype = "dotted", color = "#ED1D24", linewidth = 0.6) +
  geom_hline(aes(yintercept = 70), linetype = "dotted", color = "#ED1D24", linewidth = 0.6) +
  geom_line(clarity, mapping = aes(x = time, y = value), linewidth = 0.5, color = "#1a1c1a") +
  scale_y_continuous(breaks = seq(40, 400, 60), limits = c(40,400), expand = c(0,0)) +
  scale_x_datetime(
    date_labels = "%d %b, %H:%M",
    date_minor_breaks = "hour",
    expand = c(0, 0)
  ) +
  labs(
    title = "Daily Report",
    subtitle = "Oct. 24th to 25th",
    x = "Date/Time",
    y = "Blood Glucose (mg/dL)",
    caption = capt
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(linewidth = 0.1, color = "#1a1c1a"),
    panel.grid.minor = element_line(linewidth = 0.05, color = "#444444"),
    panel.border = element_rect(linewidth = 0.75),
    panel.background = element_rect(fill = "#f0ecdf"),
    plot.margin = margin(14, 14, 14, 14),
    plot.background = element_rect(fill = "#f0ecdf", color = "#1a1c1a", linewidth = 1.2),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0), hjust = 0.5),
    text = element_text(family = "sans"),
    axis.title.x = element_text(size = 12, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0))
  )

daily_overview




################################################################################


percent_it <- function(x, y) {

  x <- x |>
    select(...8) |>
    mutate(...8 = replace(...8, ...8 == "High", "400")) |>
    mutate(...8 = replace(...8, ...8 == "Low", "40")) |>
    mutate(...8 = as.numeric(...8), type = ...8)

  x$type[x$...8 >= 180] <- "High"
  x$type[x$...8 <= 70] <- "Low"
  x$type[x$...8 >= 70 & x$...8 <= 180] <- "In Range"

  x <- x |>
    group_by(type) |>
    summarize(count = n()) |>
    mutate(prop = round((count/nrow(x)*100))) |>
    mutate(factor = c(1, 2, 3)) |>
    mutate(type = fct_reorder(type, factor)) |>
    mutate(period = c(y, y, y))
}


sample1 <- clarity_10_23_01_20
sample2 <- clarity_07_18_10_15[-(1:19),]


onepercent <- percent_it(sample1, "one")
twopercent <- percent_it(sample2, "two")


all_percent <- onepercent |>
  full_join(twopercent) |>
  mutate(label = paste0(prop, "%"))


length1 <- paste(as.Date(sample1$...2[1]), "to", as.Date(sample1$...2[nrow(sample1)]))
length2 <- paste(as.Date(sample2$...2[1]), "to", as.Date(sample2$...2[nrow(sample2)]))

comp_bars <- ggplot(all_percent, aes(x = type, y = prop, fill = period)) +
  geom_bar(position = position_dodge2(width = 1), stat = "identity", color = "black", linewidth = 0.25) +
  geom_text(aes(label = label), position = position_dodge2(width = 0.9), vjust = -1, color = "white", family = "Antonio") +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_manual(
    name = "Time Period",
    values = c("#cc99cc", "#CC6699"),
    labels = c(length1, length2)) +
  theme(
    text = element_text(family = "Antonio"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    plot.margin = margin(15, 15, 15, 15),
    axis.text.x = element_text(size = 12, face = "bold", color = "white", margin = margin(10,0,0,0)),
    legend.text = element_text(size = 12, color = "white"),
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.key.spacing.y = unit(2, "mm"),
    legend.background = element_rect(fill = "black", color = "white", linewidth = 0.25, linetype = "solid"),
    legend.title = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "white"),
    axis.ticks = element_blank(),
    axis.text.y = element_blank()
  )

comp_bars







percent_bars <- ggplot(percentages) +
  geom_col(aes(x = prop, y = name, fill = name), width = 0.70) +
  scale_x_continuous(
    limits = c(0,100),
    expand = c(0,0),
    position = "top"
  ) +
  scale_y_discrete(expand = expansion(add = c(0, 0))) +
  scale_fill_manual(values = c("#CD99CE", "#99CCFF", "#ffcc66")) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_line(color = "grey", linewidth = 0.2),
    axis.line = element_blank(),
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

percent_bars <- percent_bars +
  geom_shadowtext(
    aes(prop, y = name, label = name),
    hjust = 0,
    nudge_x = 2,
    bg.colour = "white",
    color = c("#ffcc66", "#99CCFF", "#CD99CE"),
    bg.r = 0.25,
    size = 7
  )

percent_bars




