library(ggplot2)
load("data-rda/minute_2022_8.tb.rda")
#load("data-rda/minute_2022_9.tb.rda")

ggplot(minute_2022_9.tb[-1, ], aes(AirTemp_Max - AirTemp_Min)) +
  stat_density() +
  expand_limits(x = c(-0.1, 0.5))
