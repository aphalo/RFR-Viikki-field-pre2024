rm(list = ls(pattern = "*"))

library(dplyr)
library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(lubridate)
library(ggpp)
library(ggpmisc)
library(dplyr)
library(patchwork)
library(svglite)

photon_as_default()
options(ggspectra.add.symbols = FALSE)

# data from weather station's broadband sensors and computed sun position
load("data-rda/minute_2015_latest.tb.rda")

colnames(minute_2015_latest.tb)

whole_day.tb <-
  subset(minute_2015_latest.tb,
         time >= ymd_hm("2023-07-09 00:00") & time < ymd_hm("2023-07-13 00:00")) # 4 days
# time >= ymd_hm("2023-06-27 00:00") & time < ymd_hm("2023-07-02 00:00")) # 4 days
# time >= ymd_hm("2023-07-09 00:00") & time < ymd_hm("2023-07-10 00:00")) # overcast
# time >= ymd_hm("2023-07-10 00:00") & time < ymd_hm("2023-07-11 00:00")) # broken clouds
# time >= ymd_hm("2023-07-12 00:00") & time < ymd_hm("2023-07-13 00:00")) # no clouds
# time >= ymd_hm("2023-07-01 00:00") & time < ymd_hm("2023-07-02 00:00")) # dark clouds and rain
# time >= ymd_hm("2023-06-29 00:00") & time < ymd_hm("2023-06-30 00:00")) # very dark clouds
# time >= ymd_hm("2023-06-23 00:00") & time < ymd_hm("2023-06-24 00:00")) # afternoon clouds
# time >= ymd_hm("2023-06-22 00:00") & time < ymd_hm("2023-06-23 00:00")) # cloudy
# time >= ymd_hm("2023-06-21 00:00") & time < ymd_hm("2023-06-22 00:00")) # few morning clouds
# time >= ymd_hm("2023-07-05 00:00") & time < ymd_hm("2023-07-06 00:00")) # many small clouds
# time >= ymd_hm("2023-04-04 00:00") & time < ymd_hm("2023-04-05 00:00")) # sensor cleaning
# time >= ymd_hm("2023-04-26 00:00") & time < ymd_hm("2023-04-27 00:00")) # broken clouds

whole_day.tb |>
  select(day_of_year, sun_elevation, was_sunny) |>
  filter(sun_elevation > 10) |>
  group_by(day_of_year) |>
  summarise(occluded_fr = sum(!was_sunny, na.rm = TRUE) / sum(!is.na(was_sunny)))

whole_day.tb |>
  select(day_of_year, sun_elevation, was_sunny) |>
  filter(sun_elevation > 19) |>
  group_by(day_of_year) |>
  summarise(occluded_fr = sum(!was_sunny, na.rm = TRUE) / sum(!is.na(was_sunny)),
            occluded_h = occluded_fr * 12,
            occluded_min = occluded_fr * 12 * 60)

subset_noon <- function(x) {x[(x$solar_time %% 12 < 1/60) &
                                (as.numeric(x$solar_time) > 5), ]}
viikki_bio3.geo <- data.frame(lon = 25.019212, lat = 60.226805, address = "BIO3, Viikki")

noons.tb <- subset_noon(whole_day.tb)
round(day_length(noons.tb$time, geocode = viikki_bio3.geo, twilight = -1), 2)

round(day_length(noons.tb$time, geocode = viikki_bio3.geo, twilight = 19), 2)

(round(day_length(noons.tb$time, geocode = viikki_bio3.geo, twilight = -1), 2) -
  round(day_length(noons.tb$time, geocode = viikki_bio3.geo, twilight = 20), 2)) / 2

ggplot(whole_day.tb, aes(time, UVA_umol)) +
  geom_line()

ggplot(whole_day.tb, aes(time, UVB_umol)) +
  geom_line()

ggplot(whole_day.tb, aes(time, blue_umol)) +
  geom_line()

ggplot(whole_day.tb, aes(time, red_umol)) +
  geom_line()

ggplot(whole_day.tb, aes(time, far_red_umol)) +
  geom_line()

par_july_4days_diff.fig <-
  ggplot(whole_day.tb, aes(x = time)) +
  geom_spct(aes(y = PAR_umol), linewidth = 0.25, fill = "wheat",
            colour = "black") +
  geom_spct(aes(y = PAR_umol * PAR_diff_fr), linewidth = 0.25,
            fill = "skyblue", colour = "black") +
  geom_x_margin_point(mapping = aes(xintercept = time,
                                    colour = sun_elevation > 1),
                      shape = "|") +
  geom_x_margin_point(data = subset_noon,
                      mapping = aes(xintercept = time),
                      shape = "triangle down filled", size = 2,
                      fill = "yellow") +
  scale_y_continuous(name = expression("PAR photon irradiance, "*Q~~(mu*mol~m^{-2}~s^{-1}))) +
  scale_x_datetime(name = "Time, (hh:mm UTC)",
                   date_labels = "%H:%M",
                   date_breaks = "12 hours",
                   date_minor_breaks = "6 hours",
                   expand = expansion()) +
  scale_colour_manual(values = c("grey50", "yellow")) +
  theme_classic()
  # theme(axis.text.x.bottom = element_text(hjust = 0, vjust = 0.5, angle = 90))
par_july_4days_diff.fig

par_july_4days.fig <-
  ggplot(whole_day.tb, aes(x = time)) +
  geom_spct(aes(y = PAR_umol), linewidth = 0.25, fill = "wheat", colour = "black", alpha = 1/3) +
  geom_x_margin_point(mapping = aes(xintercept = time,
                                    colour = sun_elevation > 1),
                      shape = "|") +
  geom_x_margin_point(data = subset_noon,
                      mapping = aes(xintercept = time),
                      shape = "triangle down filled",
                      size = 2, fill = "wheat") +
  geom_text(data = subset_noon,
            mapping = aes(x = time,
                          label = format(time, format = "%e %B"),
            y = 2400)) +
  scale_y_continuous(name = expression("PAR photon irradiance"~~(mu*mol~m^{-2}~s^{-1}))) +
  scale_x_datetime(name = "Time of day, (hh:mm UTC)",
                   date_labels = "%H:%M",
                   date_breaks = "12 hours",
                   date_minor_breaks = "6 hours",
                   expand = expansion()) +
  labs(x = "", tag = "A") +
  scale_colour_manual(values = c("grey50", "wheat")) +
  theme_classic() + theme(plot.tag.position = c(0.11, 0.95),
                          plot.tag = element_text(hjust = 0, size = 10))
par_july_4days.fig

uvb_july_4days.fig <-
  ggplot(whole_day.tb, aes(x = time)) +
  geom_spct(aes(y = UVB_umol), linewidth = 0.25,
            fill = "purple", colour = "black", alpha = 1/3) +
  geom_x_margin_point(mapping = aes(xintercept = time,
                                    colour = sun_elevation > 1),
                      shape = "|") +
  geom_x_margin_point(data = subset_noon,
                      mapping = aes(xintercept = time),
                      shape = "triangle down filled", size = 2,
                      fill = "wheat") +
  scale_y_continuous(name = expression("UV-B photon irradiance"~~(mu*mol~m^{-2}~s^{-1}))) +
  expand_limits(y = 4) +
  scale_x_datetime(name = "Time of day, (hh:mm UTC)",
                   date_labels = "%H:%M",
                   date_breaks = "12 hours",
                   date_minor_breaks = "6 hours",
                   expand = expansion()) +
  labs(x = "", tag = "B") +
  scale_colour_manual(values = c("grey50", "wheat")) +
  theme_classic() + theme(plot.tag.position = c(0.11, 0.95),
                          plot.tag = element_text(hjust = 0, size = 10))
uvb_july_4days.fig

ggplot(whole_day.tb, aes(time, PAR_umol_BF)) +
  geom_line()

ggplot(whole_day.tb, aes(time, PAR_umol_CS)) +
  geom_line()


pdf("sunfleck-review-figs/par-july-4days-fig.pdf", width = 8, height = 4)
print(par_july_4days.fig)
dev.off()

pdf("sunfleck-review-figs/par-uvb-july-4days-fig.pdf", width = 7, height = 8)
print(par_july_4days.fig / uvb_july_4days.fig +
        plot_layout(guides = "collect", axes = "collect_x"))
dev.off()

svg("sunfleck-review-figs/par-july-4days-fig.svg", width = 8, height = 4)
print(par_july_4days.fig)
dev.off()

svg("sunfleck-review-figs/par-uvb-july-4days-fig.svg", width = 7, height = 8)
print(par_july_4days.fig / uvb_july_4days.fig +
        plot_layout(guides = "collect", axes = "collect_x"))
dev.off()


