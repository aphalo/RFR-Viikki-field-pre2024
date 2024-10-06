library(photobiology)
library(lubridate)
library(dplyr)

viikki_bio3.geo <- data.frame(lon = 25.019212, lat = 60.226805, address = "Viikki, Finland")

load("data-rda/minute_2015_latest.tb.rda")

sun_four_days_1min.tb <-
  subset(minute_2015_latest.tb,
         time >= ymd_hm("2023-07-09 00:00") &
           time < ymd_hm("2023-07-13 00:00")) |>
  select(time, solar_time, sun_elevation, PAR_umol:UVB_umol, was_sunny, was_day)

dim(sun_four_days_1min.tb) # 5760 x 16
class(sun_four_days_1min.tb)
comment(sun_four_days_1min.tb) <- "Small subset of a data set published at https://osf.io/e4vau/ doi:10.17605/OSF.IO/E4VAU."
where_measured(sun_four_days_1min.tb) <- viikki_bio3.geo
what_measured(sun_four_days_1min.tb) <- "Photon irradiance for different wavebands measured with broadband sensors. One-minute means."

save(sun_four_days_1min.tb, file = "~/R-pkgs-owned/photobiologySun/data-raw/broad-band/sun-four-days-1min-tb.rda")
