library(dplyr)
library(photobiology)
library(lubridate)
library(ggplot2)


load("data-rda/minute_2015_2021.tb.rda")

sun_times_viikki <- function(date_string) {
  viikki_bio3.geo <- data.frame(lon = 25.01673, lat = 60.2253, address = "BIO3, Viikki")
  z <- day_night(ymd(date_string),
                    geocode = viikki_bio3.geo,
                    unit.out = "datetime")
  z[["sunrise"]]
  c(z[["sunrise"]][1], z[["noon"]][1], z[["sunset"]][1])
}
# sun_times_viikki(today())
# class(sun_times_viikki(today()))

minute_2015_2021.tb$series_start <- NULL
minute_2015_2021.tb$was_sunny <- NULL

# attributes(minute_2015_2021.tb)
attr(minute_2015_2021.tb, "file.header") <- NULL
comment(minute_2015_2021.tb) <- NULL

names(minute_2015_2021.tb)

minute_2015_2021.tb$time_EET <- with_tz(minute_2015_2021.tb$time, tzone = "EET")

broken_clouds_day.tb <-
  subset(minute_2015_2021.tb,
         time_EET > ymd_hms("2019-06-15 00:00:00", tz = "EET") &
           time_EET < ymd_hms("2019-06-16 00:00:00", tz = "EET"))

ggplot(broken_clouds_day.tb, aes(time_EET, PAR_umol)) +
  geom_area(colour = "tomato", alpha = 0.4, fill = "orange") +
  geom_area(aes(y = PAR_umol * PAR_diff_fr), colour = "blue",
            alpha = 0.2, fill = "blue") +
  geom_vline(xintercept = sun_times_viikki("2019-06-15"),
             linetype = "dashed") +
  expand_limits(y = 2500) +
  labs(x = "Local time at Viikki, EEST",
       y = expression(Photon~~irradiance~~(mu*mol~s^{-1}~m^{-2}))) +
  theme_bw()

sunny_day.tb <-
subset(minute_2015_2021.tb,
       time_EET > ymd_hms("2019-06-18 00:00:00", tz = "EET") &
         time_EET < ymd_hms("2019-06-19 00:00:00", tz = "EET"))

ggplot(sunny_day.tb, aes(time_EET, PAR_umol)) +
  geom_area(colour = "tomato", alpha = 0.4, fill = "orange") +
  geom_area(aes(y = PAR_umol * PAR_diff_fr), colour = "blue",
            alpha = 0.2, fill = "blue") +
  geom_vline(xintercept = sun_times_viikki("2019-06-18"),
             linetype = "dashed") +
  expand_limits(y = 2500) +
  labs(x = "Local time at Viikki, EEST",
       y = expression(Photon~~irradiance~~(mu*mol~s^{-1}~m^{-2}))) +
  theme_bw()

overcast_day.tb <-
  subset(minute_2015_2021.tb,
         time_EET > ymd_hms("2019-06-14 00:00:00", tz = "EET") &
           time_EET < ymd_hms("2019-06-15 00:00:00", tz = "EET"))

ggplot(overcast_day.tb, aes(time_EET, PAR_umol)) +
  geom_area(colour = "tomato", alpha = 0.4, fill = "orange") +
  geom_area(aes(y = PAR_umol * PAR_diff_fr), colour = "blue",
            alpha = 0.2, fill = "blue") +
  geom_vline(xintercept = sun_times_viikki("2019-06-14"),
             linetype = "dashed") +
  expand_limits(y = 2500) +
  labs(x = "Local time at Viikki, EEST",
       y = expression(Photon~~irradiance~~(mu*mol~s^{-1}~m^{-2}))) +
  theme_bw()

save(sunny_day.tb, broken_clouds_day.tb, overcast_day.tb,
     file = "IPS-141-sun-data.rda")

library(ggspectra)
library(photobiologyWavebands)
ggplot(sun.spct) +
  stat_wb_box(w.band = VIS_bands(), ypos.fixed = 0.5, box.height = 0.3, color = "white") +
  stat_wb_label(w.band = VIS_bands(), ypos.fixed = 0.5, angle = 90) +
  stat_wb_box(w.band = Plant_bands("CIE"), ypos.fixed = -0.5, box.height = 0.3, color = "white") +
  stat_wb_label(w.band = Plant_bands("CIE"), ypos.fixed = -0.5, angle = 90) +
  stat_wb_box(w.band = Plant_bands("sensory20"), ypos.fixed = 0, box.height = 0.3, color = "white") +
  stat_wb_label(w.band = Plant_bands("sensory20"), ypos.fixed = 0, angle = 90) +
  stat_wl_strip(ymax = -0.7, ymin = -0.75, length.out = 400) +
  expand_limits(y = -0.75) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(x = "Wavelength (nm)") +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

library(photobiologySun)
autoplot(gap.mspct)
