library(dplyr)
library(lubridate)
library(photobiology)

# hour.tb %>%
#   select(TIMESTAMP, time_of_day, contains("_Hst")) ->
#   hour_histograms.tb
# colnames(hour_histograms.tb)
#

# hour.tb %>%
#   select(-contains("_Hst")) ->
#   hour_summaries.tb
# colnames(hour_summaries.tb)

load("data-rda/hour_2015_8.tb.rda")
hour_2015_8.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2015_8x.tb

load("data-rda/hour_2016_7.tb.rda")
hour_2016_7.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount_Tot,
                          hail = Hamount_Tot,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2016_7x.tb

load("data-rda/hour_2019_4.tb.rda")
hour_2019_4.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount_Tot,
                          hail = Hamount_Tot,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2019_4x.tb

load("data-rda/hour_2020_5.tb.rda")
hour_2020_5.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount_Tot,
                          hail = Hamount_Tot,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2020_5x.tb

load("data-rda/hour_2020_8.tb.rda")
hour_2020_8.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount,
                          hail = Hamount,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day
                          ) -> hour_2020_8x.tb

load("data-rda/hour_2020_11.tb.rda")
hour_2020_11.tb %>% select(time = TIMESTAMP,
                           PAR_umol_LI = PAR_Den_Avg,
                           PAR_umol_LI_sd = PAR_Den_Std,
                           PAR_umol_BF = PAR_BF_tot_Avg,
                           PAR_umol_BF_sd = PAR_BF_tot_Std,
                           global_watt = Solar_irrad_Avg,
                           global_watt_sd = Solar_irrad_Std,
                           wind_speed = WindSpd_S_WVT,
                           wind_direction = WindDir_D1_WVT,
                           air_temp_C = AirTemp_Avg,
                           air_RH = RelHumidity_Avg,
                           air_DP = AirDewPoint_Avg,
                           air_pressure = AirPressure,
                           rain = Ramount,
                           hail = Hamount,
                           day_of_year,
                           month_of_year,
                           month_name,
                           calendar_year = year,
                           solar_time = time_of_day) -> hour_2020_11x.tb

load("data-rda/hour_2021_6.tb.rda")
hour_2021_6.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount,
                          hail = Hamount,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2021_6x.tb

load("data-rda/hour_2021_8.tb.rda")
hour_2021_8.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount,
                          hail = Hamount,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2021_8x.tb

load("data-rda/hour_2022_8.tb.rda")
hour_2022_8.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure_Avg,
                          rain = Ramount,
                          hail = Hamount,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2022_8x.tb

load("data-rda/hour_2022_9.tb.rda")
hour_2022_9.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure_Avg,
                          rain = Ramount_Tot,
                          hail = Hamount_Tot,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2022_9x.tb

load("data-rda/hour_2023_10.tb.rda")
hour_2023_10.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure_Avg,
                          rain = Ramount_Tot,
                          hail = Hamount_Tot,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day) -> hour_2023_10x.tb

load("data-rda/hour_2024_4.tb.rda")
hour_2024_4.tb %>% select(time = TIMESTAMP,
                           PAR_umol_LI = PAR_Den_Avg,
                           PAR_umol_LI_sd = PAR_Den_Std,
                           PAR_umol_CS = PAR_Den_CS_Avg,
                           PAR_umol_CS_sd = PAR_Den_CS_Std,
                           PAR_umol_BF = PAR_BF_tot_Avg,
                           PAR_umol_BF_sd = PAR_BF_tot_Std,
                           global_watt = Solar_irrad_Avg,
                           global_watt_sd = Solar_irrad_Std,
                           wind_speed = WindSpd_S_WVT,
                           wind_direction = WindDir_D1_WVT,
                           air_temp_C = AirTemp_Avg,
                           air_RH = RelHumidity_Avg,
                           air_DP = AirDewPoint_Avg,
                           air_pressure = AirPressure_Avg,
                           rain = Ramount_Tot,
                           hail = Hamount_Tot,
                           day_of_year,
                           month_of_year,
                           month_name,
                           calendar_year = year,
                           solar_time = time_of_day) -> hour_2024_4x.tb

bind_rows(hour_2022_9x.tb, hour_2015_8x.tb, hour_2016_7x.tb, hour_2019_4x.tb,
          hour_2020_5x.tb, hour_2020_8x.tb, hour_2020_11x.tb,
          hour_2021_6x.tb, hour_2021_8x.tb, hour_2022_8x.tb,
          hour_2023_10x.tb, hour_2024_4x.tb)  %>%
  arrange(time) %>%
  mutate(PAR_umol = ifelse(time < ymd_hms("2021-01-01 00:00:01", tz = "EET"),
                           PAR_umol_LI, PAR_umol_CS),
         PAR_umol_sd = ifelse(time < ymd_hms("2021-01-01 00:00:01", tz = "EET"),
                              PAR_umol_LI_sd, PAR_umol_CS_sd),
         .after = "PAR_umol_BF_sd") -> hour_2015_latest.tb

rm(hour_2015_8x.tb, hour_2016_7x.tb, hour_2019_4x.tb,
   hour_2020_5x.tb, hour_2020_8x.tb, hour_2020_11x.tb,
   hour_2021_6x.tb, hour_2021_8x.tb, hour_2022_8x.tb,
   hour_2022_9x.tb, hour_2023_10x.tb, hour_2024_4x.tb)

gc()

range(hour_2015_latest.tb$time, na.rm = TRUE)
sum(is.na(hour_2015_latest.tb$time))

# sometimes PC400 appends rows already in file
# we delete duplicates
hour_2015_latest.tb <-
  distinct(hour_2015_latest.tb, time, .keep_all = TRUE) %>%
  filter(!is.na(time))

anyNA(hour_2015_latest.tb$time)
range(hour_2015_latest.tb$time, na.rm = TRUE)

save(hour_2015_latest.tb, file = "data-rda/hour_2015_latest.tb.rda")

## Including soil sensors data

# load("data-rda/hour_2020_5.tb.rda")
hour_2020_5.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount_Tot,
                          hail = Hamount_Tot,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day,
                          contains(c("5cm", "0cm"))) -> hour_2020_5z.tb

# load("data-rda/hour_2020_8.tb.rda")
hour_2020_8.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount,
                          hail = Hamount,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day,
                          contains(c("5cm", "0cm"))) -> hour_2020_8z.tb

# load("data-rda/hour_2020_11.tb.rda")
hour_2020_11.tb %>% select(time = TIMESTAMP,
                           PAR_umol_LI = PAR_Den_Avg,
                           PAR_umol_LI_sd = PAR_Den_Std,
                           PAR_umol_BF = PAR_BF_tot_Avg,
                           PAR_umol_BF_sd = PAR_BF_tot_Std,
                           global_watt = Solar_irrad_Avg,
                           global_watt_sd = Solar_irrad_Std,
                           wind_speed = WindSpd_S_WVT,
                           wind_direction = WindDir_D1_WVT,
                           air_temp_C = AirTemp_Avg,
                           air_RH = RelHumidity_Avg,
                           air_DP = AirDewPoint_Avg,
                           air_pressure = AirPressure,
                           rain = Ramount,
                           hail = Hamount,
                           day_of_year,
                           month_of_year,
                           month_name,
                           calendar_year = year,
                           solar_time = time_of_day,
                           contains(c("5cm", "0cm"))) -> hour_2020_11z.tb

#load("data-rda/hour_2021_6.tb.rda")
hour_2021_6.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount,
                          hail = Hamount,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day,
                          contains(c("5cm", "0cm"))) -> hour_2021_6z.tb

#load("data-rda/hour_2021_8.tb.rda")
hour_2021_8.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure,
                          rain = Ramount,
                          hail = Hamount,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day,
                          ends_with(c("_1", "_2", "_3"))
                          ) -> hour_2021_8z.tb

#load("data-rda/hour_2022_8.tb.rda")
hour_2022_8.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure_Avg,
                          rain = Ramount,
                          hail = Hamount,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day,
                          ends_with(c("_1_Avg", "_2_Avg", "_3_Avg"))
) -> hour_2022_8z.tb
colnames(hour_2022_8z.tb) <- gsub("_Avg", "", colnames(hour_2022_8z.tb))

#load("data-rda/hour_2022_9.tb.rda")
hour_2022_9.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure_Avg,
                          rain = Ramount_Tot,
                          hail = Hamount_Tot,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day,
                          ends_with(c("_1_Avg", "_2_Avg", "_3_Avg"))
) -> hour_2022_9z.tb
colnames(hour_2022_9z.tb) <- gsub("_Avg", "", colnames(hour_2022_9z.tb))

#load("data-rda/hour_2023_10.tb.rda")
hour_2023_10.tb %>% select(time = TIMESTAMP,
                          PAR_umol_LI = PAR_Den_Avg,
                          PAR_umol_LI_sd = PAR_Den_Std,
                          PAR_umol_CS = PAR_Den_CS_Avg,
                          PAR_umol_CS_sd = PAR_Den_CS_Std,
                          PAR_umol_BF = PAR_BF_tot_Avg,
                          PAR_umol_BF_sd = PAR_BF_tot_Std,
                          global_watt = Solar_irrad_Avg,
                          global_watt_sd = Solar_irrad_Std,
                          wind_speed = WindSpd_S_WVT,
                          wind_direction = WindDir_D1_WVT,
                          air_temp_C = AirTemp_Avg,
                          air_RH = RelHumidity_Avg,
                          air_DP = AirDewPoint_Avg,
                          air_pressure = AirPressure_Avg,
                          rain = Ramount_Tot,
                          hail = Hamount_Tot,
                          day_of_year,
                          month_of_year,
                          month_name,
                          calendar_year = year,
                          solar_time = time_of_day,
                          ends_with(c("_1_Avg", "_2_Avg", "_3_Avg"))
) -> hour_2023_10z.tb
colnames(hour_2023_10z.tb) <- gsub("_Avg", "", colnames(hour_2023_10z.tb))

#load("data-rda/hour_2024_4.tb.rda")
hour_2024_4.tb %>% select(time = TIMESTAMP,
                           PAR_umol_LI = PAR_Den_Avg,
                           PAR_umol_LI_sd = PAR_Den_Std,
                           PAR_umol_CS = PAR_Den_CS_Avg,
                           PAR_umol_CS_sd = PAR_Den_CS_Std,
                           PAR_umol_BF = PAR_BF_tot_Avg,
                           PAR_umol_BF_sd = PAR_BF_tot_Std,
                           global_watt = Solar_irrad_Avg,
                           global_watt_sd = Solar_irrad_Std,
                           wind_speed = WindSpd_S_WVT,
                           wind_direction = WindDir_D1_WVT,
                           air_temp_C = AirTemp_Avg,
                           air_RH = RelHumidity_Avg,
                           air_DP = AirDewPoint_Avg,
                           air_pressure = AirPressure_Avg,
                           rain = Ramount_Tot,
                           hail = Hamount_Tot,
                           day_of_year,
                           month_of_year,
                           month_name,
                           calendar_year = year,
                           solar_time = time_of_day,
                           ends_with(c("_1_Avg", "_2_Avg", "_3_Avg"))
) -> hour_2024_4z.tb
colnames(hour_2024_4z.tb) <- gsub("_Avg", "", colnames(hour_2024_4z.tb))

bind_rows(hour_2020_5z.tb, hour_2020_8z.tb,
          hour_2020_11z.tb, hour_2021_6z.tb,
          hour_2021_8z.tb, hour_2022_8z.tb,
          hour_2022_9z.tb, hour_2023_10z.tb,
          hour_2024_4z.tb) %>%
  relocate(PAR_umol_CS:PAR_umol_CS_sd, .before = "PAR_umol_BF")%>%
  mutate(PAR_umol = ifelse(time < ymd_hms("2021-01-01 00:00:01", tz = "EET"),
                           PAR_umol_LI, PAR_umol_CS),
         PAR_umol_sd = ifelse(time < ymd_hms("2021-01-01 00:00:01", tz = "EET"),
                              PAR_umol_LI_sd, PAR_umol_CS_sd),
         .after = "PAR_umol_BF_sd") ->
  hour_soil_2020_latest.tb

soil_profile_cols <-
  which(grepl("cm_", colnames(hour_soil_2020_latest.tb)))

bad_profile_rows <-
  hour_soil_2020_latest.tb$time > ymd_hms("2023-06-01 12:00:00", tz = "EET") &
          hour_soil_2020_latest.tb$time < ymd_hms("2023-10-01 00:00:00", tz = "EET")

zzz <- hour_soil_2020_latest.tb

hour_soil_2020_latest.tb$soil_watering_disturbance <- bad_profile_rows

# hour_soil_2020_latest.tb[bad_profile_rows, soil_profile_cols] <- NA_real_

hour_soil_2020_latest.tb$time <- with_tz(hour_soil_2020_latest.tb$time, tzone = "UTC")


rm(hour_2020_5z.tb, hour_2020_8z.tb,
   hour_2020_11z.tb, hour_2021_6z.tb,
   hour_2021_8z.tb, hour_2022_8z.tb,
   hour_2022_9z.tb, hour_2023_10z.tb,
   hour_2024_4z.tb)

range(hour_soil_2020_latest.tb$time, na.rm = TRUE)
sum(is.na(hour_soil_2020_latest.tb$time))

# sometimes PC400 appends rows already in file
# we delete duplicates
hour_soil_2020_latest.tb <-
  distinct(hour_soil_2020_latest.tb, time, .keep_all = TRUE) %>%
  filter(!is.na(time))

# Before 2021-06-09 14:00:00 rain and hail were accumulated rather than instantaneous

anyNA(hour_soil_2020_latest.tb$time)
range(hour_soil_2020_latest.tb$time, na.rm = TRUE)

save(hour_soil_2020_latest.tb, file = "data-rda/hour_soil_2020_latest.tb.rda")

load("data-rda/hour_calc_2015_latest.tb.rda")

intersect(names(hour_calc_2015_latest.tb), names(hour_soil_2020_latest.tb))
range(hour_calc_2015_latest.tb$time)
range(hour_soil_2020_latest.tb$time)

class(hour_calc_2015_latest.tb$time)
class(hour_soil_2020_latest.tb$time)

right_join(hour_calc_2015_latest.tb, hour_soil_2020_latest.tb[ , c(1,21:103)], by = "time") -> hour_soil_calc_2015_latest.tb

names(hour_soil_calc_2015_latest.tb)

save(hour_soil_calc_2015_latest.tb, file = "data-rda/hour_soil_calc_2015_latest.tb.rda")

full_join(hour_calc_2015_latest.tb, hour_soil_2020_latest.tb[ , c(1,21:103)], by = "time") -> hour_soil_calc_2015_latest.tb

names(hour_soil_calc_2015_latest.tb)

save(hour_soil_calc_2015_latest.tb, file = "data-rda/hour_soil_calc_2015_latest.tb.rda")

