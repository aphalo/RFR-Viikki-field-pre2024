# This script reads sun weather data for Kumpula FMI station (1 min frequency)
# It stores the read data in a file, and when run again reads only newer data
# To reset saved data, simply delete file "fmi-data-wide.Rda"

library(fmi2)
library(dplyr)
library(sf)
library(lubridate)

# # modified from fmi2
# obs_sun_minute <- function(starttime, endtime, fmisid = NULL) {
#   fmi_obj <- fmi_api(request = "getFeature",
#                      storedquery_id = "fmi::observations::radiation::simple",
#                      starttime = starttime, endtime = endtime, fmisid = fmisid)
#   sf_obj <- fmi2:::to_sf(fmi_obj)
#   sf_obj <- sf_obj %>%
#     dplyr::select(time = .data$Time, variable = .data$ParameterName,
#                   value = .data$ParameterValue) %>%
#     dplyr::mutate(time = lubridate::parse_date_time(.data$time, "Ymd HMS"),
#                   variable = as.character(.data$variable),
#                   # Factor needs to be coerced into character first
#                   value = as.numeric(as.character(.data$value))) %>%
#     dplyr::mutate(value = ifelse(is.nan(.data$value), NA, .data$value))
#   return(sf_obj)
# }

if (!file.exists("fmi-sun-data-wide.Rda")) {
  # Used only once or when replacing all data
  starttime.char <- "2022-01-01 00:00"
  starttime <- ymd_hm(starttime.char)
  wide_sun_data <- data.frame()
} else {
  load("fmi-sun-data-wide.Rda")
  # we start 1 h after end of previously downloaded data
  starttime <- max(wide_sun_data$time) + minutes(1) + hours(2) # convert to UTC + 2h
}

endtime <- trunc(now(), units = "mins") + hours(2) # UTC -> UTC + 2h

# we read the new data to a new dataframe
# (to avoid appending repeatedly to a long one)
new_wide_data <- data.frame()
while (starttime < endtime) {
  sliceendtime <- starttime + days(7) # keep query size at max of 1 week
  if (sliceendtime > endtime) {
    sliceendtime <- endtime
  }
  kumpula_data <- obs_radiation_minute(starttime = as.character(starttime),
                                       endtime = as.character(sliceendtime),
                                       fmisid = 101004)

  slice_data <- kumpula_data %>%
    tidyr::spread(variable, value) %>%
    # convert the sf object into a regular tibble
    sf::st_set_geometry(NULL)

  new_wide_data <- rbind(new_wide_data, slice_data)
  starttime <- sliceendtime + minutes(1)
  cat(".")
}

new_wide_data$time <- new_wide_data$time - hours(2)
new_wide_data$time <- force_tz(new_wide_data$time, tzone = "UTC")

range(new_wide_data$time)

wide_sun_data <- rbind(wide_sun_data, new_wide_data)
range(wide_sun_data$time)
colnames(wide_sun_data)

save(wide_sun_data, file = "fmi-sun-data-wide.Rda")
