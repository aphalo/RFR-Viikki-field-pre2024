library(tibble)
library(lubridate)
library(dplyr)
library(ggplot2)
library(photobiology)

load(file = "minute_2015_2020.tb.rda")

colnames(minute_2015_2020.tb)

minute_2015_2020.tb %>%
  filter(time > ymd_hms("2020-06-25 06:40:00") & time < ymd_hms("2020-06-25 16:55:00")) %>%
  select(-series_start) -> sensor_values.tb

sensor_values.tb %>%
  summarise(PAR_umol = mean(PAR_umol),
            PAR_umol_BF = mean(PAR_umol_BF))
