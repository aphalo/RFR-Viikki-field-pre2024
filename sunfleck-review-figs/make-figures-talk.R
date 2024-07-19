library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)
library(lubridate)
library(ggpmisc)
library(dplyr)
library(patchwork)

rm(list = ls(pattern = "*"))

photon_as_default()

# data from weather station's broadband sensors and computed sun position
load("data-rda/minute_2015_latest.tb.rda")

colnames(minute_2015_latest.tb)

whole_day.tb <-
  filter(minute_2015_latest.tb,
         #         format(minute_2015_latest.tb$time) %in% format(round.POSIXt(when.measured, "mins")))
         #          time >= ymd_hm("2023-06-27 00:00") & time < ymd_hm("2023-07-02 00:00")) # 4 days
# time >= ymd_hm("2023-07-09 00:00") & time < ymd_hm("2023-07-10 00:00")) # overcast
# time >= ymd_hm("2023-07-10 00:00") & time < ymd_hm("2023-07-11 00:00")) # broken clouds
time >= ymd_hm("2023-07-09 00:00") & time < ymd_hm("2023-07-13 00:00")) # 4 days
# time >= ymd_hm("2023-07-01 00:00") & time < ymd_hm("2023-07-02 00:00")) # dark clouds and rain
# time >= ymd_hm("2023-06-29 00:00") & time < ymd_hm("2023-06-30 00:00")) # very dark clouds
# time >= ymd_hm("2023-06-23 00:00") & time < ymd_hm("2023-06-24 00:00")) # afternoon clouds
# time >= ymd_hm("2023-06-22 00:00") & time < ymd_hm("2023-06-23 00:00")) # cloudy
# time >= ymd_hm("2023-06-21 00:00") & time < ymd_hm("2023-06-22 00:00")) # few morning clouds
# time >= ymd_hm("2023-07-05 00:00") & time < ymd_hm("2023-07-06 00:00")) # many small clouds
# time >= ymd_hm("2023-04-04 00:00") & time < ymd_hm("2023-04-05 00:00")) # sensor cleaning
# time >= ymd_hm("2023-04-26 00:00") & time < ymd_hm("2023-04-27 00:00")) # broken clouds

par_july_4days_diff.fig <-
  ggplot(whole_day.tb, aes(x = time)) +
  geom_spct(aes(y = PAR_umol), linewidth = 0.25, fill = "wheat", colour = "black") +
  geom_spct(aes(y = PAR_umol * PAR_diff_fr), linewidth = 0.25, fill = "skyblue", colour = "black") +
  scale_y_continuous(name = expression("PAR photon irradiance, "*Q~~(mu*mol~m^{-2}~s^{-1}))) +
  scale_x_datetime(date_labels = "%A %e %B", expand = expansion()) +
  labs(x = "") +
  theme_classic() +
  theme(axis.text = element_text(hjust = 0),
        legend.position = "none")
par_july_4days_diff.fig

par_july_4days.fig <-
  ggplot(whole_day.tb, aes(x = time)) +
  geom_spct(aes(y = PAR_umol), linewidth = 0.25, fill = "wheat", colour = "black", alpha = 1/3) +
  scale_y_continuous(name = expression(Q[PAR]~~(mu*mol~m^{-2}~s^{-1}))) +
  scale_x_datetime(date_labels = "%A %e %B", expand = expansion()) +
  labs(x = "") +
  theme_classic() + theme(axis.text = element_text(hjust = 0))
par_july_4days.fig

uvb_july_4days.fig <-
  ggplot(whole_day.tb, aes(x = time)) +
  geom_spct(aes(y = UVB_umol), linewidth = 0.25, fill = "violet", colour = "black", alpha = 1/3) +
  scale_y_continuous(name = expression(Q[UV-B]~~(mu*mol~m^{-2}~s^{-1}))) +
  scale_x_datetime(date_labels = "%A %e %B", expand = expansion()) +
  labs(x = "") +
  theme_classic() +
  theme(axis.text = element_text(hjust = 0),
        legend.position = "none")
uvb_july_4days.fig

ggplot(whole_day.tb, aes(time, PAR_umol_BF)) +
  geom_line()

ggplot(whole_day.tb, aes(time, PAR_umol_CS)) +
  geom_line()


pdf("sunfleck-review-figs/par-july-4days-fig.pdf", width = 8, height = 4)
print(par_july_4days.fig)
dev.off()

pdf("sunfleck-review-figs/par-uvb-july-4days-fig.pdf", width = 6, height = 4)
print(par_july_4days.fig / uvb_july_4days.fig + plot_layout(guides = "collect"))
dev.off()

### spectra

z <- list()

load("sunfleck-review-figs/cosine.hour.1.spct.Rda")
autoplot(cosine.hour.1.spct)

z[["noon"]] <-
  s_irrad_corrected(cosine.hour.1.raw_mspct,
                    spct.names = c(light = "light.1", filter = "filter", dark = "dark"),
                    correction.method = MAYP112785_sun.mthd) |>
  despike()

autoplot(z[["noon"]])

load("sunfleck-review-figs/cosine.hour.2.spct.Rda")
autoplot(cosine.hour.2.spct)


z[["afternoon"]] <-
  s_irrad_corrected(cosine.hour.2.raw_mspct[1:3],
                    spct.names = c(light = "light.01", filter = "filter", dark = "dark"),
                    correction.method = MAYP112785_sun.mthd) |>
  despike()

autoplot(z[["afternoon"]])

load("sunfleck-review-figs/cosine.hour.6.spct.Rda")
autoplot(cosine.hour.6.spct)

z[["evening"]] <-
  s_irrad_corrected(cosine.hour.6.raw_mspct[c(1:2, 3)],
                    spct.names = c(light = "light.1", filter = "filter", dark = "dark"),
                    correction.method = MAYP112785_sun.mthd) |>
  despike()

autoplot(z[["evening"]])


load("sunfleck-review-figs/cosine.hour.7.spct.Rda")
autoplot(cosine.hour.7.spct)

z[["evening2"]] <-
  s_irrad_corrected(cosine.hour.7.raw_mspct[c(1:2, 3)],
                    spct.names = c(light = "light.1", filter = "filter", dark = "dark"),
                    correction.method = MAYP112785_sun.mthd) |>
  despike()

autoplot(z[["evening2"]])

spectra.mspct <- source_mspct(z[-2])

irrad.tb <- q_irrad(spectra.mspct,
                    w.band = Plant_bands("CIE"),
                    scale.factor = 1e6)
irrad.tb[ , -1] <- signif(irrad.tb[ , -1], 2)
colnames(irrad.tb) <- gsub("\\.CIE|\\.idx", "", colnames(irrad.tb))

sun_spct_uvpar.fig <-
  autoplot(spectra.mspct,
           range = c(280, 700),
           w.band = Plant_bands("CIE"),
           annotations = c("-", "peaks"),
           ylim = c(NA, 7),
           text.size = 4) +
  geom_table(data = data.frame(x = I(0.01), y = I(0.82), label = I(list(irrad.tb))),
             mapping = aes(x = x, y = y, label = label),
             size = 3) +
  labs(linetype = "") +
  theme_classic() + theme(legend.position = "none")
sun_spct_uvpar.fig

pdf("sunfleck-review-figs/sun_spct_uvpar.fig.pdf", width = 8, height = 4)
print(sun_spct_uvpar.fig)
dev.off()

sun_spct_uv.fig <-
  autoplot(spectra.mspct,
           range = c(280, 400),
           w.band = Plant_bands("CIE"),
           annotations = c("-", "peaks"),
           ylim = c(NA, 3.25),
           text.size = 4) +
  geom_vline(xintercept = c(280, 293, 315, 340, 400), linetype = "dashed",
             colour = c("black", "red", "black", "black", "black")) +
  geom_table(data = data.frame(x = I(0.085), y = I(0.7), label = I(list(irrad.tb))),
             mapping = aes(x = x, y = y, label = label),
             size = 3) +
  labs(linetype = "") +
  theme_classic() + theme(legend.position = "none")
sun_spct_uv.fig

pdf("sunfleck-review-figs/sun_spct_uv.fig.pdf", width = 8, height = 4)
print(sun_spct_uv.fig)
dev.off()

