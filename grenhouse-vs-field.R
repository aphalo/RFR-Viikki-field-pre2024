library(dplyr)
library(lubridate)
library(ggpmisc)

read.csv2("greenhouse-stn/greenhouse weather station 5-9_2023.csv", skip = 3,
          col.names = c("time", "air_temp_grnh"), header = FALSE) |>
  mutate(time = dmy_hm(time, tz = "EET")) -> temperature_grnh_stn.df

ggplot(temperature_grnh_stn.df, aes(time, air_temp_grnh)) +
  geom_line()

load("data-osf/Viikki-1h-all-2017-latest-PRELIMINARY.rda")
Viikki_1h_all_2017_2023_PRELIMINARY.tb |>
  filter(time >= min(temperature_grnh_stn.df$time) &
           time <= max(temperature_grnh_stn.df$time)) |>
  select(time, air_temp_C_mean) -> temperature_field_stn.df

left_join(temperature_field_stn.df, temperature_grnh_stn.df) -> temperature.df


ggplot(temperature.df, aes(air_temp_C_mean, air_temp_grnh)) +
  geom_point(alpha = 0.1) +
  stat_poly_line(formula = y ~ x + 1) +
  stat_poly_eq(use_label(c("eq", "n")), formula = y ~ x + 1) +
  geom_abline(slope = 1, intercept = 0, colour = "red")
