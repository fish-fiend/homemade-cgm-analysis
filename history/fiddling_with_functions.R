library(tidyverse)


# THIS WORKS.
highs <- function(x) {
  mutate(x, ...8 = replace(...8, ...8 == "High", "400" ))
}

lows <- function(x) {
  mutate(x, ...8 = replace(...8, ...8 == "Low", "40" ))
}

victim_02 <- clarity_export_90

victim_02 <- victim_02 |>
  highs() |>
  lows()

View(victim_02)


