library(ggplot2)
library(lubridate)
library(r4photobiology)
library(photobiologyInOut)
library(lubridate)
library(dplyr)

viikki_bio3.geo <- data.frame(lon = 25.01673, lat = 60.2253, address = "BIO3, Viikki")

day.tb <- read_csi_dat(file = "CR6Series_TableDay.dat")

names(day.tb)

minute_raw.tb <- read_csi_dat(file = "CR6Series_TableMinute.dat")

minute_raw.tb[["TIMESTAMP"]][1]
tz(minute_raw.tb[["TIMESTAMP"]][1])
minute_raw.tb[["TIMESTAMP"]] <- force_tz(minute_raw.tb[["TIMESTAMP"]], tzone = "Europe/Helsinki")
minute_raw.tb[["TIMESTAMP"]][1]
tz(minute_raw.tb[["TIMESTAMP"]][1])

# what we do bellow
# *) Apply calibration to R:FR sensor data.
# 1) Select two periods of 31 days, should be better, at least
# for visual comparisons.
# 2) Calculate R:FR ratio and diffuse:total PAR, but being
# careful to keep values for times with relaible data (high enough
# irradiance)
# 3) Add solar angles to each time point.
# 4) Create a factor indexing the two periods of 31 days.

minute_raw.tb %>%
  # there is a gap in the data record
  filter(TIMESTAMP %within% interval(ymd_hm("2017-06-17 00:00"),
                                     ymd_hm("2017-07-18 00:00")) |
           TIMESTAMP %within% interval(ymd_hm("2017-08-05 00:00"),
                                       ymd_hm("2017-09-28, 00:00")) ) %>%
  mutate(red_umol = ifelse(Red_Den_Avg > 0, Red_Den_Avg * 2.972, 0),
         far_red_umol = ifelse(Far_red_Den_Avg > 0, Far_red_Den_Avg * 2.483, 0),
         red_far_red_raw = red_umol/far_red_umol,
         # Ratio is discarded if it is "too dark"
         # A reasonable cut off seems to be at 0.05 umol m-2 s-1.
         red_far_red = ifelse(red_umol < 0.05 |
                              far_red_umol < 0.05,
                              NA_real_,
                              red_far_red_raw),
         PAR_diff_fr_raw = PAR_BF_diff_Avg/PAR_BF_tot_Avg,
         # PAR readings from BF5 start being unreliable at
         # a relatively high irradiance of 5 umol m-2 s-1.
         PAR_diff_fr = ifelse(PAR_BF_diff_Avg < 5,
                              NA_real_,
                              PAR_diff_fr_raw),
         time_of_day = as_tod(TIMESTAMP),
         sun_elevation = sun_elevation(TIMESTAMP, geocode = viikki_bio3.geo, use.refraction = TRUE),
         sun_azimuth = sun_azimuth(TIMESTAMP, geocode = viikki_bio3.geo, use.refraction = TRUE),
         period = factor(ifelse(TIMESTAMP %within% interval(ymd_hm("2017-08-05 00:00"),
                                                            ymd_hm("2017-09-28, 00:00")),
                                "AugSep", "JunJuly"),
                         levels = c("JunJuly", "AugSep"))) -> minute.tb


# "Time series" plots
ggplot(minute.tb, aes(TIMESTAMP, PAR_Den_Avg)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, red_far_red)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, red_umol)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, far_red_umol)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")

# Two dimensional density plots against sun elevation
# Higher density = locally higher concnetration of observations
ggplot(minute.tb, aes(sun_elevation, PAR_Den_Avg)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
ggplot(minute.tb, aes(sun_elevation, red_far_red)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
ggplot(minute.tb, aes(sun_elevation, red_umol)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period, scales = "free_x") +
  scale_fill_viridis_c(option = "C")
ggplot(minute.tb, aes(sun_elevation, far_red_umol)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period, scales = "free_x") +
  scale_fill_viridis_c(option = "C")

# "Time series" plots for BF5
ggplot(minute.tb, aes(TIMESTAMP, PAR_BF_tot_Avg)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, PAR_BF_diff_Avg)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, PAR_diff_fr)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")

# Two dimensional density plots against sun elevation for BF5
# Very few sunny days -> mostly diffuse radiation
ggplot(minute.tb, aes(sun_elevation, PAR_BF_tot_Avg)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
ggplot(minute.tb, aes(sun_elevation, PAR_BF_diff_Avg)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
ggplot(minute.tb, aes(sun_elevation, PAR_diff_fr)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")


# "Time series" plots for Vaisala
ggplot(minute.tb, aes(TIMESTAMP, AirTemp_Avg)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, AirDewPoint)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, RelHumidity)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, AirPressure)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")
ggplot(minute.tb, aes(TIMESTAMP, WindSpd_S_WVT)) +
  geom_line() +
  facet_grid(~period, scales = "free_x")

# Two dimensional density plots against time of day for BF5
# Very few sunny days -> mostly diffuse radiation
ggplot(minute.tb, aes(time_of_day, WindSpd_S_WVT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-1, 25)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
ggplot(minute.tb, aes(time_of_day, WindDir_D1_WVT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-1, 25)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
ggplot(minute.tb, aes(sun_elevation, WindSpd_S_WVT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
ggplot(minute.tb, aes(sun_elevation, WindDir_D1_WVT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = c(-30, 60)) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
ggplot(subset(minute.tb, WindSpd_S_WVT > 1000/3600), aes(WindDir_D1_WVT, WindSpd_S_WVT)) +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  expand_limits(x = -1) +
  facet_grid(~period) +
  scale_fill_viridis_c(option = "C")
