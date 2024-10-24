

#approximately reverse engineering a plot of the plain daily averages using 
#moving averages?

#since there's 1,440 minutes in a day and the CGM produces a measurement 
#every five minutes, it should produce 288 observations per day.
#if the interval is set at k = 288, one average value is produced per 288 
#observations, which is equivalent to averaging all the values in a given day.
#however, the data is skewed by the fact that the cgm is occasionally 
#nonfunctional (during a warmup period, for example) so some days will have 
#fewer than 288 observations. 


ma_idek <- clarity_data |>
  mutate(ma_288 = rollmean(value, k = 288, fill = NA, align = "right")) |>
  filter(!is.na(ma_288))

ggplot(ma_idek, aes(x = date, y = ma_288)) +
  geom_line()