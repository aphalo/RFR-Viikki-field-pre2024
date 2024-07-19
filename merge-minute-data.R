library(dplyr)
library(lubridate)
library(photobiology)
library(photobiologyWavebands)

## 2022-11-23
#
# There still seems to be a problem with the time shift computations that needs
# to be fixed. Solar time and angles are correct and agree with radiation data.
# The `time` variable seems to be shifted by one hour, in the computed hourly
# means, most likely the bug is in the data merge script.

## 2022-11-24
#
# Fixed, now timing agrees with FMI data

## 2024-04-20
#
# Added code to recompute sun angles and solar time using more accurate
# coordinates for the location of the station
#
# Recompute was_sunny using a better algorithm based on the solar elevation
# and expected diffuse fraction function from TUV simulations.
#
# Instead of reading again the raw data, we overwrite these values in the
# data already imported. See line 815

# clean up
rm(list = ls(pattern = "*"))
gc()

load("data-rda/minute_2015_8.tb.rda")
nrow(minute_2015_8.tb)
minute_2015_8.tb %>%
  mutate(series_start = TIMESTAMP[1],
         PAR_umol = NA_real_,
         PAR_umol_LI = PAR_Den_Avg) %>%
  select(series_start,
         time = TIMESTAMP,
         PAR_umol_LI,
         PAR_umol_BF = PAR_BF_tot_Avg,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         was_sunny = sunny) -> minute_2015_8x.tb

load("data-rda/minute_2016_7.tb.rda")
nrow(minute_2016_7.tb)
minute_2016_7.tb %>%
  mutate(series_start = TIMESTAMP[1],
         PAR_umol = NA_real_,
         PAR_umol_LI = PAR_Den_Avg) %>%
  select(series_start,
         time = TIMESTAMP,
         PAR_umol_LI,
         PAR_umol_BF = PAR_BF_tot_Avg,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         was_sunny = sunny) -> minute_2016_7x.tb

load("data-rda/minute_2016_8.tb.rda")
nrow(minute_2016_8.tb)
minute_2016_8.tb %>%
  mutate(series_start = TIMESTAMP[1],
         PAR_umol = NA_real_,
         air_vp = water_RH2vp(RelHumidity, AirTemp_Avg)) %>%
  select(series_start,
         time = TIMESTAMP,
         PAR_umol_LI = PAR_Den_Avg,
         PAR_umol_BF = PAR_BF_tot_Avg,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_vp,
         air_RH = RelHumidity,
         air_DP = AirDewPoint,
         air_pressure = AirPressure,
         rain_mm_min = Ramount_Tot,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         was_sunny = sunny) -> minute_2016_8x.tb

load("data-rda/minute_2017_6.tb.rda")
nrow(minute_2017_6.tb)
minute_2017_6.tb %>%
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = PAR_Den_Avg * 1.17,
         PAR_umol_BF = PAR_BF_tot_Avg * 1,
         PAR_umol = PAR_umol_LI,
         red_umol = red_umol * 4.35,
         far_red_umol = far_red_umol * 4.35) %>%
  select(series_start,
         time = TIMESTAMP,
         PAR_umol_LI,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_vp,
         air_RH = RelHumidity,
         air_DP = AirDewPoint,
         air_pressure = AirPressure,
         rain_mm_min = Ramount_Tot,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         was_sunny = sunny) -> minute_2017_6x.tb

load("data-rda/minute_2019_4.tb.rda")
nrow(minute_2019_4.tb)
minute_2019_4.tb %>%
  # calibrations adjusted based on spectra from Maxime for summer 2020
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = PAR_Den_Avg * 1.26,
         PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.36,
         PAR_umol = PAR_umol_LI,
         red_umol = red_umol * 5.18 / 1.055,
         far_red_umol = far_red_umol * 5.12 / 1.048) %>%
  select(series_start,
         time = TIMESTAMP,
         PAR_umol_LI,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_vp,
         air_RH = RelHumidity,
         air_DP = AirDewPoint,
         air_pressure = AirPressure,
         rain_mm_min = Ramount_Tot,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         was_sunny = sunny) -> minute_2019_4x.tb

load("data-rda/minute_2020_5.tb.rda")
nrow(minute_2020_5.tb)
minute_2020_5.tb %>%
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = PAR_Den_Avg * 1.26,
         PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.27,
         PAR_umol = PAR_umol_LI,
         blue_umol = Blue_Den_Avg * 0.881 / 1.03 - 0.916,
         blue_sellaro_umol = Blue_Den_Avg * 0.637 / 1.03 - 0.545,
         UVA_umol = UVA_Den_Avg * 0.2356 - 0.251,
         UVB_umol = UVB_Den_Avg * 0.0046644 / 0.992 - 0.00433,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.055,
         far_red_umol = far_red_umol * 5.12 / 1.048,
         SurfTemp_veg_Avg = ifelse(SurfTemp_veg_Avg < -27 | SurfTemp_veg_Avg > 100,
                                   NA_real_,
                                   SurfTemp_veg_Avg),
         SurfTemp_grnd_Avg = ifelse(SurfTemp_grnd_Avg < -27 | SurfTemp_grnd_Avg > 100,
                                    NA_real_,
                                    SurfTemp_grnd_Avg),
         surf_temp_sensor_delta_C = SurfTemp_veg_Avg - SurfTemp_grnd_Avg) %>%
  select(series_start,
         time = TIMESTAMP,
         PAR_umol_LI,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_vp,
         air_RH = RelHumidity,
         air_DP = AirDewPoint,
         air_pressure = AirPressure,
         rain_mm_min = Ramount_Tot,
         surf_temp_C = SurfTemp_veg_Avg, # differences in vegetation??
         surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         was_sunny = sunny) -> minute_2020_5x.tb

# same data are in the file loaded above
#
# load("data-rda/minute_2020_11.tb.rda")
# nrow(minute_2020_11.tb)
# minute_2020_11.tb %>%
#   mutate(series_start = TIMESTAMP[1],
#          air_vp = water_RH2vp(RelHumidity, AirTemp_Avg),
#          PAR_umol = NA_real_,
#          PAR_umol_LI = PAR_Den_Avg * 1.26,
#          PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.27,
#          blue_umol = Blue_Den_Avg * 0.881 - 0.944,
#          blue_sellaro_umol = Blue_Den_Avg * 0.637 - 0.561,
#          UVA_umol = UVA_Den_Avg * 0.2356 - 0.251,
#          UVB_umol = UVB_Den_Avg * 0.0046644 / 0.982 - 0.00437,
#          UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
#          UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
#          red_umol = red_umol * 5.18 / 1.055,
#          far_red_umol = far_red_umol * 5.12 / 1.048,
#          SurfTemp_veg_Avg = ifelse(SurfTemp_veg_Avg < -27 | SurfTemp_veg_Avg > 100,
#                                    NA_real_,
#                                    SurfTemp_veg_Avg),
#          SurfTemp_grnd_Avg = ifelse(SurfTemp_grnd_Avg < -27 | SurfTemp_grnd_Avg > 100,
#                                     NA_real_,
#                                     SurfTemp_grnd_Avg),
#          surf_temp_sensor_delta_C = SurfTemp_veg_Avg - SurfTemp_grnd_Avg,
#          surf_temp_C = (SurfTemp_veg_Avg + SurfTemp_grnd_Avg) / 2,
#          surf_temp_C = ifelse(!is.na(surf_temp_C), surf_temp_C,
#                               ifelse(is.na(SurfTemp_veg_Avg), SurfTemp_grnd_Avg, SurfTemp_veg_Avg))) %>%
#   select(series_start,
#          time = TIMESTAMP,
#          PAR_umol_LI,
#          PAR_umol_BF,
#          PAR_umol,
#          PAR_diff_fr,
#          global_watt = Solar_irrad_Avg,
#          red_umol,
#          far_red_umol,
#          blue_umol,
#          blue_sellaro_umol,
#          UVA_umol,
#          UVA1_umol,
#          UVA2_umol,
#          UVB_umol,
#          wind_speed = WindSpd_S_WVT,
#          wind_direction = WindDir_D1_WVT,
#          air_temp_C = AirTemp_Avg,
#          air_vp,
#          air_RH = RelHumidity,
#          air_DP = AirDewPoint,
#          air_pressure = AirPressure,
#          rain_mm_min = Ramount_Tot,
#          surf_temp_C = surf_temp_C,
#          surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
#          day_of_year,
#          month_of_year,
#          month_name,
#          calendar_year = year,
#          time_of_day = time_of_day_utc,
#          solar_time = solar_time_h,
#          sun_elevation,
#          sun_azimuth,
#          was_sunny = sunny) -> minute_2020_11x.tb

load("data-rda/minute_2021_6.tb.rda")
nrow(minute_2021_6.tb)
minute_2021_6.tb %>%
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = NA_real_, # PAR_Den_Avg * 1.26 / 0.948,
         PAR_umol_CS = PAR_Den_CS_Avg / 0.965,
         PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.27,
         blue_umol = Blue_Den_Avg * 0.881 / 0.911 - 0.944,
         blue_sellaro_umol = Blue_Den_Avg * 0.637 / 0.911 - 0.561,
         UVA_umol = UVA_Den_Avg * 0.2356 / 0.891 - 0.251,
         UVB_umol = UVB_Den_Avg * 0.0046644 / 0.749 - 0.00437,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.097,
         far_red_umol = far_red_umol * 5.12 / 1.090,
         SurfTemp_veg_Avg = ifelse(SurfTemp_veg_Avg < -27 | SurfTemp_veg_Avg > 100,
                                   NA_real_,
                                   SurfTemp_veg_Avg),
         SurfTemp_grnd_Avg = ifelse(SurfTemp_grnd_Avg < -27 | SurfTemp_grnd_Avg > 100,
                                    NA_real_,
                                    SurfTemp_grnd_Avg),
         surf_temp_sensor_delta_C = SurfTemp_veg_Avg - SurfTemp_grnd_Avg,
         surf_temp_C = (SurfTemp_veg_Avg + SurfTemp_grnd_Avg) / 2,
         surf_temp_C = ifelse(!is.na(surf_temp_C), surf_temp_C,
                              ifelse(is.na(SurfTemp_veg_Avg), SurfTemp_grnd_Avg, SurfTemp_veg_Avg))) %>%
  select(series_start,
         time = TIMESTAMP,
         PAR_umol_LI,
         PAR_umol_CS,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_vp,
         air_RH = RelHumidity,
         air_DP = AirDewPoint,
         air_pressure = AirPressure,
         rain_mm_min = Ramount_Tot,
         surf_temp_C = surf_temp_C,
         surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         was_sunny = sunny) -> minute_2021_6x.tb

load("data-rda/minute_2021_8.tb.rda")
nrow(minute_2021_8.tb)
minute_2021_8.tb %>%
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = NA_real_, # PAR_Den_Avg * 1.26 / 1.06, #  / 0.948
         PAR_umol_CS = PAR_Den_CS_Avg / 0.946, # -0 - 50
         PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.27,
         blue_umol = Blue_Den_Avg * 0.881 / 0.902 - 0.944,
         blue_sellaro_umol = Blue_Den_Avg * 0.637 / 0.902 - 0.561,
         UVA_umol = UVA_Den_Avg * 0.2356 / 0.936 - 0.251,
         UVB_umol = UVB_Den_Avg * 0.0046644 / 0.854 - 0.00437,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.050,
         far_red_umol = far_red_umol * 5.12 / 1.02, # / 1.048
         SurfTemp_grnd_Avg = ifelse(TIMESTAMP > ymd_hms("2021-12-09 18:00:00") &
                                TIMESTAMP < ymd_hms("2022-06-21 11:24:00"),
                                SurfTemp_grnd_Avg - 5.5,
                                SurfTemp_grnd_Avg),
         SurfTemp_veg_Avg = ifelse(SurfTemp_veg_Avg < -27 | SurfTemp_veg_Avg > 100,
                                   NA_real_,
                                   SurfTemp_veg_Avg),
         SurfTemp_grnd_Avg = ifelse(SurfTemp_grnd_Avg < -27 | SurfTemp_grnd_Avg > 100,
                                    NA_real_,
                                    SurfTemp_grnd_Avg),
         surf_temp_sensor_delta_C = SurfTemp_veg_Avg - SurfTemp_grnd_Avg,
         surf_temp_C = (SurfTemp_veg_Avg + SurfTemp_grnd_Avg) / 2,
         surf_temp_C = ifelse(!is.na(surf_temp_C), surf_temp_C,
                              ifelse(is.na(SurfTemp_veg_Avg), SurfTemp_grnd_Avg, SurfTemp_veg_Avg))) %>%
  select(series_start,
         time = TIMESTAMP,
         PAR_umol_LI,
         PAR_umol_CS,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_vp,
         air_RH = RelHumidity,
         air_DP = AirDewPoint,
         air_pressure = AirPressure,
         rain_mm_min = Ramount_Tot,
         surf_temp_C = surf_temp_C,
         surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         was_sunny = sunny) -> minute_2021_8x.tb

load("data-rda/minute_2022_8.tb.rda")
nrow(minute_2022_8.tb)
minute_2022_8.tb %>%
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = NA_real_, # PAR_Den_Avg * 1.26 / 1.06, #  / 0.948
         PAR_umol_CS = PAR_Den_CS_Avg / 0.946, # -0 - 50
         PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.27,
         blue_umol = Blue_Den_Avg * 0.881 / 0.902 - 0.944,
         blue_sellaro_umol = Blue_Den_Avg * 0.637 / 0.902 - 0.561,
         UVA_umol = UVA_Den_Avg * 0.2356 / 0.936 - 0.251,
         UVB_umol = UVB_Den_Avg * 0.0046644 / 0.854 - 0.00437,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.050,
         far_red_umol = far_red_umol * 5.12 / 1.02, # / 1.048
         SurfTemp_veg_Avg = ifelse(SurfTemp_veg_Avg < -27 | SurfTemp_veg_Avg > 100,
                                   NA_real_,
                                   SurfTemp_veg_Avg),
         SurfTemp_grnd_Avg = ifelse(SurfTemp_grnd_Avg < -27 | SurfTemp_grnd_Avg > 100,
                                    NA_real_,
                                    SurfTemp_grnd_Avg),
         surf_temp_sensor_delta_C = SurfTemp_veg_Avg - SurfTemp_grnd_Avg,
         surf_temp_C = (SurfTemp_veg_Avg + SurfTemp_grnd_Avg) / 2,
         surf_temp_C = ifelse(!is.na(surf_temp_C), surf_temp_C,
                              ifelse(is.na(SurfTemp_veg_Avg), SurfTemp_grnd_Avg, SurfTemp_veg_Avg))) %>%
  select(series_start,
         time = TIMESTAMP,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_LI,
         PAR_umol_CS,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_temp_min_C = AirTemp_Min,
         air_temp_max_C = AirTemp_Max,
         air_vp,
         air_RH = RelHumidity,
         air_DP = AirDewPoint,
         air_pressure = AirPressure,
         rain_mm_min = Ramount_Tot,
         surf_temp_C = surf_temp_C,
         surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
         was_sunny = sunny,
         SupplyVoltage_Max,
         ReferenceVoltage_Min,
         ReferenceVoltage_Max,
         BattV_Min,
         BattV_Max) -> minute_2022_8x.tb

load("data-rda/minute_2022_9.tb.rda")
nrow(minute_2022_9.tb)
minute_2022_9.tb %>%
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity_Avg, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = NA_real_, # PAR_Den_Avg * 1.26 / 1.06, #  / 0.948
         PAR_umol_CS = PAR_Den_CS_Avg / 0.946, # -0 - 50
         PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.27,
         blue_umol = Blue_Den_Avg * 0.881 / 0.902 - 0.944,
         blue_sellaro_umol = Blue_Den_Avg * 0.637 / 0.902 - 0.561,
         UVA_umol = UVA_Den_Avg * 0.2356 / 0.936 - 0.251,
         UVB_umol = UVB_Den_Avg * 0.0046644 / 0.854 - 0.00437,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.050,
         far_red_umol = far_red_umol * 5.12 / 1.02, # / 1.048
         SurfTemp_veg_Avg = ifelse(SurfTemp_veg_Avg < -27 | SurfTemp_veg_Avg > 100,
                                   NA_real_,
                                   SurfTemp_veg_Avg),
         SurfTemp_grnd_Avg = ifelse(SurfTemp_grnd_Avg < -27 | SurfTemp_grnd_Avg > 100,
                                    NA_real_,
                                    SurfTemp_grnd_Avg),
         surf_temp_sensor_delta_C = SurfTemp_veg_Avg - SurfTemp_grnd_Avg,
         surf_temp_C = (SurfTemp_veg_Avg + SurfTemp_grnd_Avg) / 2,
         surf_temp_C = ifelse(!is.na(surf_temp_C), surf_temp_C,
                              ifelse(is.na(SurfTemp_veg_Avg), SurfTemp_grnd_Avg, SurfTemp_veg_Avg))) %>%
  select(series_start,
         time = TIMESTAMP,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_LI,
         PAR_umol_CS,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_temp_min_C = AirTemp_Min,
         air_temp_max_C = AirTemp_Max,
         air_vp,
         air_RH = RelHumidity_Avg,
         air_DP = AirDewPoint_Avg,
         air_pressure = AirPressure_Avg,
         rain_mm_min = Ramount_Tot,
         surf_temp_C = surf_temp_C,
         surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
         was_sunny = sunny,
         SupplyVoltage_Max,
         ReferenceVoltage_Min,
         ReferenceVoltage_Max,
         BattV_Min,
         BattV_Max) -> minute_2022_9x.tb

load("data-rda/minute_2023_10.tb.rda")
nrow(minute_2023_10.tb)
minute_2023_10.tb %>%
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity_Avg, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = NA_real_, # PAR_Den_Avg * 1.26 / 1.06, #  / 0.948
         PAR_umol_CS = PAR_Den_CS_Avg / 0.946, # -0 - 50
         PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.27,
         blue_umol = Blue_Den_Avg * 0.881 / 0.902 - 0.944,
         blue_sellaro_umol = Blue_Den_Avg * 0.637 / 0.902 - 0.561,
         UVA_umol = UVA_Den_Avg * 0.2356 / 0.936 - 0.251,
         UVB_umol = UVB_Den_Avg * 0.0046644 / 0.854 - 0.00437,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.050,
         far_red_umol = far_red_umol * 5.12 / 1.02, # / 1.048
         SurfTemp_veg_Avg = ifelse(SurfTemp_veg_Avg < -27 | SurfTemp_veg_Avg > 100,
                                   NA_real_,
                                   SurfTemp_veg_Avg),
         SurfTemp_grnd_Avg = ifelse(SurfTemp_grnd_Avg < -27 | SurfTemp_grnd_Avg > 100,
                                    NA_real_,
                                    SurfTemp_grnd_Avg),
         surf_temp_sensor_delta_C = SurfTemp_veg_Avg - SurfTemp_grnd_Avg,
         surf_temp_C = (SurfTemp_veg_Avg + SurfTemp_grnd_Avg) / 2,
         surf_temp_C = ifelse(!is.na(surf_temp_C), surf_temp_C,
                              ifelse(is.na(SurfTemp_veg_Avg), SurfTemp_grnd_Avg, SurfTemp_veg_Avg))) %>%
  select(series_start,
         time = TIMESTAMP,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_LI,
         PAR_umol_CS,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_temp_min_C = AirTemp_Min,
         air_temp_max_C = AirTemp_Max,
         air_vp,
         air_RH = RelHumidity_Avg,
         air_DP = AirDewPoint_Avg,
         air_pressure = AirPressure_Avg,
         rain_mm_min = Ramount_Tot,
         surf_temp_C = surf_temp_C,
         surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
         was_sunny = sunny,
         SupplyVoltage_Max,
         ReferenceVoltage_Min,
         ReferenceVoltage_Max,
         BattV_Min,
         BattV_Max) -> minute_2023_10x.tb

load("data-rda/minute_2024_4.tb.rda")
nrow(minute_2024_4.tb)
minute_2024_4.tb %>%
  mutate(series_start = TIMESTAMP[1],
         air_vp = water_RH2vp(RelHumidity_Avg, AirTemp_Avg),
         PAR_umol = NA_real_,
         PAR_umol_LI = NA_real_, # PAR_Den_Avg * 1.26 / 1.06, #  / 0.948
         PAR_umol_CS = PAR_Den_CS_Avg / 0.946, # -0 - 50
         PAR_umol_BF = PAR_BF_tot_Avg * 1.09 - 0.27,
         blue_umol = Blue_Den_Avg * 0.881 / 0.902 - 0.944,
         blue_sellaro_umol = Blue_Den_Avg * 0.637 / 0.902 - 0.561,
         UVA_umol = UVA_Den_Avg * 0.2356 / 0.936 - 0.251,
         UVB_umol = UVB_Den_Avg * 0.0046644 / 0.854 - 0.00437,
         UVA1_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
         UVA2_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
         red_umol = red_umol * 5.18 / 1.050,
         far_red_umol = far_red_umol * 5.12 / 1.02, # / 1.048
         SurfTemp_veg_Avg = ifelse(SurfTemp_veg_Avg < -27 | SurfTemp_veg_Avg > 100,
                                   NA_real_,
                                   SurfTemp_veg_Avg),
         SurfTemp_grnd_Avg = ifelse(SurfTemp_grnd_Avg < -27 | SurfTemp_grnd_Avg > 100,
                                    NA_real_,
                                    SurfTemp_grnd_Avg),
         surf_temp_sensor_delta_C = SurfTemp_veg_Avg - SurfTemp_grnd_Avg,
         surf_temp_C = (SurfTemp_veg_Avg + SurfTemp_grnd_Avg) / 2,
         surf_temp_C = ifelse(!is.na(surf_temp_C), surf_temp_C,
                              ifelse(is.na(SurfTemp_veg_Avg), SurfTemp_grnd_Avg, SurfTemp_veg_Avg))) %>%
  select(series_start,
         time = TIMESTAMP,
         day_of_year,
         month_of_year,
         month_name,
         calendar_year = year,
         time_of_day = time_of_day_utc,
         solar_time = solar_time_h,
         sun_elevation,
         sun_azimuth,
         PAR_umol_LI,
         PAR_umol_CS,
         PAR_umol_BF,
         PAR_umol,
         PAR_diff_fr,
         global_watt = Solar_irrad_Avg,
         red_umol,
         far_red_umol,
         blue_umol,
         blue_sellaro_umol,
         UVA_umol,
         UVA1_umol,
         UVA2_umol,
         UVB_umol,
         wind_speed = WindSpd_S_WVT,
         wind_direction = WindDir_D1_WVT,
         air_temp_C = AirTemp_Avg,
         air_temp_min_C = AirTemp_Min,
         air_temp_max_C = AirTemp_Max,
         air_vp,
         air_RH = RelHumidity_Avg,
         air_DP = AirDewPoint_Avg,
         air_pressure = AirPressure_Avg,
         rain_mm_min = Ramount_Tot,
         surf_temp_C = surf_temp_C,
         surf_temp_sensor_delta_C = surf_temp_sensor_delta_C,
         was_sunny = sunny,
         SupplyVoltage_Max,
         ReferenceVoltage_Min,
         ReferenceVoltage_Max,
         BattV_Min,
         BattV_Max) -> minute_2024_4x.tb

# bind_rows(minute_2015_8x.tb, minute_2016_8x.tb, minute_2017_6x.tb,
#           minute_2019_4x.tb, minute_2020_5x.tb, minute_2020_11x.tb,
#           minute_2021_6x.tb) -> minute_2015_latest.tb

bind_rows(minute_2022_9x.tb[-1, ], minute_2017_6x.tb[-1, ],
          minute_2019_4x.tb[-1, ], minute_2020_5x.tb[-1, ], # minute_2020_11x.tb,
          minute_2021_6x.tb[-1, ], minute_2021_8x.tb[-1, ],
          minute_2022_8x.tb[-1, ], minute_2023_10x.tb,
          minute_2024_4x.tb) %>%
          arrange(time) -> minute_2015_latest.tb
minute_2015_latest.tb[nrow(minute_2015_latest.tb), "time"]

rm(minute_2017_6x.tb,
   minute_2019_4x.tb, minute_2020_5x.tb, # minute_2020_11x.tb,
   minute_2021_6x.tb, minute_2021_8x.tb,
   minute_2022_8x.tb, minute_2022_9x.tb,
   minute_2023_10x.tb, minute_2024_4x.tb)
gc()

# We check for duplicate rows
# Sometimes after changes in the logger program data already downloaded has been
# appended to files, leading to duplicate rows.
# We now check this when reading the file from the logger but older data
# may have duplicate records.
rle.check <- rle(sort(as.numeric(minute_2015_latest.tb$time)))
duplicates <- sum(rle.check$lengths > 1)
if (duplicates > 0L) {
  message("Found ", duplicates, " duplicated rows in data. Deleting them!!")
  minute_2015_latest.tb <- distinct(minute_2015_latest.tb, .keep_all = TRUE)
  nrow(minute_2015_latest.tb)
}

# clean_spikes <- function(x,
#                          z.threshold = 50,
#                          d.threshold = NULL,
#                          na.rm = FALSE) {
#   max.spike.width <- 3
#   n <- length(x)
#   if (na.rm) {
#     na.idx <- which(is.na(x))
#     x <- na.omit(x)
#   }
#   d.var <- diff(x)
#   if (is.null(d.threshold)) {
#     d.threshold <- mean(abs(d.var))
#   }
#   z <- (d.var - median(d.var)) / mad(d.var) * 0.6745
#   bad.obs.idx <- ifelse(abs(d.var) < d.threshold,
#                         FALSE,
#                         abs(z) > z.threshold)
#   # ensure same length as input
#   bad.obs.idx <- c(FALSE, bad.obs.idx)
#   if (!is.null(max.spike.width) && max.spike.width > 0) {
#     # ignore broad peaks using run length encoding
#     runs <- rle(bad.obs.idx)
#     runs[["values"]] <- ifelse(runs[["lengths"]] > max.spike.width, FALSE, runs[["values"]])
#     bad.obs.idx <- inverse.rle(runs)
#   }
#   if (na.rm) {
#     # restore length of logical vector
#     for (i in na.idx) {
#       bad.obs.idx <- append(bad.obs.idx, FALSE, after = i - 1L)
#     }
#   }
#   # check assertion
#   stopifnot(length(bad.obs.idx) == length(x))
#   bad.obs.idx <- which(bad.obs.idx)
#   x[bad.obs.idx] <- NA_integer_
#   if (1L %in% bad.obs.idx) {
#     x[1L] <- x[2L]
#     bad.obs.idx <- setdiff(bad.obs.idx, 1L)
#   }
#   if (n %in% bad.obs.idx) {
#     x[n] <- x[n - 1L]
#     bad.obs.idx <- setdiff(bad.obs.idx, n)
#   }
#
#   x[bad.obs.idx] <- (x[bad.obs.idx - 1] + x[bad.obs.idx + 1]) / 2
#   message("Replaced ", length(bad.obs.idx), " observations.")
#   x
# }
#
# #  Clean spikes in data
# minute_2015_latest.tb %>%
#   mutate(air_temp_C = clean_spikes(air_temp_C),
#          surf_temp_C = clean_spikes(surf_temp_C),
#          global_watt = clean_spikes(global_watt),
#          air_vp = clean_spikes(air_vp)
#   ) -> minute_2015_latest.tb

# Add computed values for radiation and PET
# value updated 2024-04-20 (difference about 200 m)
viikki_bio3.geo <- data.frame(lon = 25.019212,
                              lat = 60.226805,
                              address = "BIO3, Viikki")

clear_sky_diff_fr <- readRDS("TUV-diffuse-direct-SZA/spline-fun.RDS")

albedo <- 0.23

minute_2015_latest.tb %>%
  mutate(PAR_umol = ifelse(time < ymd_hms("2021-01-01 00:00:01", tz = "EET"),
                           PAR_umol_LI, PAR_umol_CS)) %>%
  mutate(
    # corrected values overwrite original ones
    solar_time_h = solar_time(time,
                              geocode = viikki_bio3.geo),
    sun_elevation = sun_elevation(time,
                                  geocode = viikki_bio3.geo,
                                  use.refraction = TRUE,
                                  tz = "UTC"),
    sun_azimuth = sun_azimuth(time,
                              geocode = viikki_bio3.geo,
                              use.refraction = TRUE,
                              tz = "UTC"),
    was_day = sun_elevation > 0,
    PAR_diff_fr_rel = ifelse(was_day,
                             1 - ((1 - PAR_diff_fr) / (1 - clear_sky_diff_fr(sun_elevation))),
                             NA_real_),
    was_sunny = PAR_diff_fr_rel < 0.6,
    ## end corrections
    ## temperature and energy balance
    logged_air_temp_C = air_temp_C,
    # filter bad air temperature data
    # out of range
    air_temp_C = ifelse(logged_air_temp_C < -40 | logged_air_temp_C > 40,
                        NA_real_,
                        logged_air_temp_C),
    # use running median for 11 min as reference
    air_temp_run_median = runmed(air_temp_C, k = 21),
    # if minute average is much less than 21 min median, replace with median
    air_temp_C = ifelse((air_temp_C - air_temp_run_median) < 1 | is.na(air_temp_C),
                        air_temp_run_median,
                        logged_air_temp_C),

    temp_surf2air_C = surf_temp_C - air_temp_C,
    R_0 = irrad_extraterrestrial(time = time,
                                 geocode = viikki_bio3.geo),
    R_rel = ifelse(R_0 > 2,
                   global_watt / (R_0 * 0.75),
                   0.25), # for now we assume cloudy sky at night
    Rn_sw_ref = global_watt,
    Rn_ref = net_irradiance(temperature = air_temp_C,
                            sw.down.irradiance = Rn_sw_ref,
                            sw.albedo = albedo,
                            water.vp = air_vp,
                            R_rel = R_rel),
    ET_ref_FAO56 = ET_ref(temperature = air_temp_C,
                          water.vp = air_vp,
                          wind.speed = wind_speed,
                          net.irradiance = Rn_ref,
                          nighttime = sun_elevation < 0,
                          atmospheric.pressure = air_pressure,
                          method = "FAO.PM",
                          check.range = FALSE),
    ET_ref_short = ET_ref(temperature = air_temp_C,
                          water.vp = air_vp,
                          wind.speed = wind_speed,
                          net.irradiance = Rn_ref,
                          nighttime = sun_elevation < 0,
                          atmospheric.pressure = air_pressure,
                          method = "ASCE.PM.short",
                          check.range = FALSE),
    ET_ref_tall = ET_ref(temperature = air_temp_C,
                         water.vp = air_vp,
                         wind.speed = wind_speed,
                         net.irradiance = Rn_ref,
                         nighttime = sun_elevation < 0,
                         atmospheric.pressure = air_pressure,
                         method = "ASCE.PM.tall",
                         check.range = FALSE),
    # ET_ref_surf = ET_ref(temperature = surf_temp_C,
    #                 water.vp = air_vp,
    #                 wind.speed = wind_speed,
    #                 net.irradiance = Rn_ref,
    #                 nighttime = sun_elevation < 0,
    #                 atmospheric.pressure = air_pressure,
    #                 method = "ASCE.PM.short",
    #                 check.range = FALSE),
    air_vpd = ifelse(is.na(air_temp_C),
                     NA_real_,
                     water_vp_sat(air_temp_C) - air_vp),
    rain_mm_min = ifelse(time < ymd_hms("2021-06-09 14:00:00"),
                         NA_real_,
                         rain_mm_min),
    .after = "was_sunny") %>%
  mutate(# correct for temperature coefficient assuming sensors are at air temperature
    UVA_umol = UVA_umol * (1 + 0.0033 * (air_temp_C - 20)),
    UVB_umol = UVB_umol * (1 + 0.0015 * (air_temp_C - 20)),
    blue_red = blue_umol / red_umol,
    blue_red_sq = blue_red * wl_expanse(Red("Smith10")) / wl_expanse(Blue("Sellaro")),
    UVA_PAR = UVA_umol / PAR_umol,
    UVA_PAR_sq = UVA_PAR * wl_expanse(PAR()) / wl_expanse(UVA()),
    UVA1_PAR = UVA1_umol / PAR_umol,
    UVA1_PAR_sq = UVA1_PAR * wl_expanse(PAR()) / wl_expanse(UVA1()),
    UVA2_PAR = UVA2_umol / PAR_umol,
    UVA2_PAR_sq = UVA2_PAR * wl_expanse(PAR()) / wl_expanse(UVA2()),
    UVB_PAR = UVB_umol / PAR_umol,
    UVB_PAR_sq = UVB_PAR * wl_expanse(PAR()) / wl_expanse(UVB()),
    red_far_red = red_umol / far_red_umol,
    .after = "UVB_umol") -> minute_2015_latest.tb
minute_2015_latest.tb[nrow(minute_2015_latest.tb), "time"]

gc()

range(minute_2015_latest.tb$time)
nrow(minute_2015_latest.tb)
ncol(minute_2015_latest.tb)
colnames(minute_2015_latest.tb)

# checks
sum(is.na(minute_2015_latest.tb$air_temp_C))
sum(is.na(minute_2015_latest.tb$air_RH))

minute_2015_latest.tb %>%
  group_by(calendar_year, month_of_year, day_of_year) %>%
  filter(PAR_umol == max(PAR_umol)) %>%
  select(time, calendar_year, month_of_year, day_of_year,
         PAR_umol, solar_time, time_of_day, sun_elevation) -> solar.time.max.PAR

solar.time.max.PAR %>%
  group_by(calendar_year, month_of_year) %>%
  summarise(solar.time.max.PAR = mean(solar_time) ,
            local.time.max.PAR = mean(time_of_day),
            elevation.max.PAR = mean(sun_elevation)) %>%
  ungroup() %>%
  arrange(month_of_year, calendar_year) -> zz

# View(zz)

range(minute_2015_latest.tb$time)
tail(minute_2015_latest.tb)

save(minute_2015_latest.tb, file = "data-rda/minute_2015_latest.tb.rda")

# compute hourly summaries

mean_min_max <- list(
  mean = ~mean(.x, na.rm = FALSE),
#  mean.trimmed = ~mean(.x, trim = 1/3, na.rm = FALSE),
#  median = ~median(.x, na.rm = FALSE),
  min = ~min(.x, na.rm = FALSE),
  max = ~max(.x, na.rm = FALSE)
)

colnames(minute_2015_latest.tb)

# time is the hour at the
minute_2015_latest.tb %>%
  mutate(time_hours = trunc(time, units = "hours") + hours(1)) %>% # match FMI timing
  select(-series_start) %>%
#  relocate(contains(c("time", "year", "month", "sun"))) %>%
  group_by(time_hours) %>%
    summarize(across(time:calendar_year, first, .names = "{.col}_first"),
              across(time:calendar_year, last, .names = "{.col}_last"),
              across(time_of_day:sun_azimuth, median, .names = "{.col}_median"),
              across(PAR_umol_LI:air_vpd, mean_min_max),
              across(BattV_Min:BattV_Max, mean_min_max),
              rain_mm_h = sum(rain_mm_min),
              n = n()) %>%
  ungroup() %>%
  filter(n > 31) %>% # delete summaries for hours with less than 31 min of data
#  select(-time) %>%
  rename(time = time_hours) -> hour_calc_2015_latest.tb

colnames(hour_calc_2015_latest.tb)

save(hour_calc_2015_latest.tb, file = "data-rda/hour_calc_2015_latest.tb.rda")

