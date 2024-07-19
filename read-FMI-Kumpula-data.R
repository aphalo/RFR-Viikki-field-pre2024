# This script reads basic weather data for Kumpula FMI station
# It stores the read data in a file, and when run again reads only newer data
# To reset saved data, simply delete file "fmi-data-wide.Rda"

library(fmi2)
library(dplyr)
library(sf)
library(lubridate)

if (!file.exists("fmi-data-wide.Rda")) {
  # Used only once or when replacing all data
  starttime.char <- "2021-01-01 00:00"
  starttime <- ymd_hm(starttime.char)
  wide_data <- data.frame()
} else {
  load("fmi-data-wide.Rda")
  # we start 1 h after end of previously downloaded data
  starttime <- max(wide_data$time) + hours(1) + hours(2) # convert to UTC + 2h
}

endtime <- trunc(now(), units = "hours") + hours(2) # UTC -> UTC + 2h

# we read the new data to a new dataframe
# (to avoid appending repeatedly to a long one)
new_wide_data <- data.frame()
while (starttime < endtime) {
  sliceendtime <- starttime + days(28) # keep query size at max 4 weeks
  if (sliceendtime > endtime) {
    sliceendtime <- endtime
  }
  kumpula_data <- obs_weather_hourly(starttime = as.character(starttime),
                                     endtime = as.character(sliceendtime),
                                     fmisid = 101004)

  slice_data <- kumpula_data %>%
    tidyr::spread(variable, value) %>%
    # convert the sf object into a regular tibble
    sf::st_set_geometry(NULL)

  new_wide_data <- rbind(new_wide_data, slice_data)
  starttime <- sliceendtime + hours(1)
  cat(".")
}

new_wide_data$time <- new_wide_data$time - hours(2)
new_wide_data$time <- force_tz(new_wide_data$time, tzone = "UTC")

range(new_wide_data$time)

wide_data <- rbind(wide_data, new_wide_data)
range(wide_data$time)

save(wide_data, file = "fmi-data-wide.Rda")

ggplot(wide_data, aes(time, PRA_PT1H_ACC)) +
  geom_line()
