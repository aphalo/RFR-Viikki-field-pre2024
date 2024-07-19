library(tibble)
library(lubridate)
library(dplyr)
library(ggplot2)
library(photobiology)

load(file = "minute_2015_2020.tb.rda")

colnames(minute_2015_2020.tb)

# find gaps and identify which ones correspond to switch to/from DST

gap_selector <- which(diff(minute_2015_2020.tb$time) != minutes(1))
series_change <- which(diff(minute_2015_2020.tb$series_start) > days(7))
series_change %in% gap_selector

gaps_in_data <-
  tibble(time_before = minute_2015_2020.tb[["time"]][gap_selector],
         time_after = minute_2015_2020.tb[["time"]][gap_selector + 1L],
         gap_mins = diff(minute_2015_2020.tb$time)[gap_selector],
         dts_change = abs(gap_mins) > 50 & abs(gap_mins) < 70,
         long_gap = gap_mins > hours(24),
         series_start_time = minute_2015_2020.tb[["series_start"]][gap_selector],
         is_series_start = series_start_time > lag(series_start_time, 1L))

gaps_in_data[["is_series_start"]][1] <- FALSE
gaps_in_data

filter(gaps_in_data, dts_change)

# Are DST shifts created when reading the data in, or when setting the
# logger clock?

minute_2015_2020.tb %>%
  mutate(year = year(time), month = month(time)) %>%
  group_by(year, month, day_of_year) %>%
  summarise(PAR_max = max(PAR_umol),
            noon_time = noon_time(time[which.max(PAR_umol)],
                                  geocode = data.frame(lon = 25.01673, lat = 60.2253, address = "BIO3, Viikki"),
                                  unit.out = "hours"),
            max_time = time[which.max(PAR_umol)],
            max_tod = time_of_day[which.max(PAR_umol)],
            max_solar_time = solar_time[which.max(PAR_umol)]) %>%
  filter(PAR_max > 100) -> max_PAR.tb
max_PAR.tb

filter(max_PAR.tb, max_tod > 16 | max_tod < 8)

unique(minute_2015_2020.tb$series_start)

ggplot(max_PAR.tb, aes(x = factor(month), y = max_tod, colour = factor(year))) +
#  geom_point() +
  geom_boxplot() +
  stat_summary(aes(y = noon_time), fun = mean, geom = "point", colour = "black") +
  scale_y_continuous(breaks=6:18) +
  facet_grid(~year)

ggplot(max_PAR.tb, aes(x = factor(month), y = max_solar_time, colour = factor(year))) +
#  geom_point() +
  geom_boxplot() +
  geom_hline(yintercept = 12, linetype = "dotted") +
  facet_grid(~year)

