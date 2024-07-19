# Juha Mikola
library(lubridate)
library(dplyr)
library(readr)

load(file = "minute_2015_2019.tb.rda")

minute_2015_2019.tb %>%
  filter(time %within% interval(ymd_hm("2019-06-10 00:00"), ymd_hm("2019-06-14 23:59"))) %>%
  select(-series_start) %>%
  rename(time_utc = time) %>%
  mutate(time_eet = with_tz(time_utc, tzone = "EET")) %>%
  write_excel_csv(path = "Mikola-June-2019-minute-data.csv")

load(file = "day_2015_2019.tb.rda")

day_2015_2019.tb %>%
  rename(day_utc = time) %>%
  filter(year(day_utc) >= 2016) %>%
  write_excel_csv(path = "Mikola-Viikki-weather-2016-2019-day-data.csv")

day_2015_2019.tb %>%
  rename(day_utc = time) %>%
  filter(year(day_utc) >= 2016) %>%
  ggplot(aes(day_utc, PAR_molday)) +
  geom_point()
