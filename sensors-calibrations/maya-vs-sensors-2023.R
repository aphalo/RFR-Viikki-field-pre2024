library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(ooacquire)
library(lubridate)
library(ggpmisc)
library(dplyr)

rm(list = ls(pattern = "*"))

photon_as_default()

path <- "./sensors-calibrations/spectra-2023-04-04"
files <- list.files(path = path,
                    pattern = "\\.spct\\.Rda",
                    full.names = TRUE)
for (f in files) {
  load(f)
}
rm(list = ls(pattern = "*\\.spct"))

sun_noon.spct <-
  s_irrad_corrected(x = viikki.sun.noon.raw_mspct,
                    spct.names = list(light = setdiff(names(viikki.sun.noon.raw_mspct), c("filter.1", "dark.1")),
                                      filter = "filter.1",
                                      dark = "dark.1"),
                    correction.method = MAYP11278_ylianttila.mthd,
                    descriptor = MAYP11278_descriptors$cal_2023a)

autoplot(sun_noon.spct)

noon_irrads.tb <-
  q_irrad(subset2mspct(sun_noon.spct),
          w.band = c(Plant_bands(), list(PAR(), UVA(), UVC(),
                                         Red("Smith10"), Far_red("Smith10"),
                                         waveband(c(400, 500)))),
          scale.factor = 1e6, attr2tb = c("when.measured" = "time", "instr.desc"))

sun_afternoon.spct <-
  s_irrad_corrected(x = viikki.sun.afternoon.raw_mspct,
                    spct.names = list(light = setdiff(names(viikki.sun.afternoon.raw_mspct), c("filter.1", "dark.1")),
                                      filter = "filter.1",
                                      dark = "dark.1"),
                    correction.method = MAYP11278_ylianttila.mthd,
                    descriptor = MAYP11278_descriptors$cal_2023a)

autoplot(sun_afternoon.spct)

afternoon_irrads.tb <-
  q_irrad(subset2mspct(sun_afternoon.spct),
          w.band = c(Plant_bands(), list(PAR(), UVA(), UVC(),
                                         Red("Smith10"), Far_red("Smith10"),
                                         waveband(c(400, 500)))),
          scale.factor = 1e6, attr2tb = c("when.measured" = "time", "instr.desc"))

sun_late_afternoon.spct <-
  s_irrad_corrected(x = Viikki.sun.late.raw_mspct,
                    spct.names = list(light = setdiff(names(Viikki.sun.late.raw_mspct), c("filter.1", "dark.1")),
                                      filter = "filter.1",
                                      dark = "dark.1"),
                    correction.method = MAYP11278_ylianttila.mthd,
                    descriptor = MAYP11278_descriptors$cal_2023a)

autoplot(sun_late_afternoon.spct)

late_afternoon_irrads.tb <-
  q_irrad(subset2mspct(sun_late_afternoon.spct),
          w.band = c(Plant_bands(), list(PAR(), UVA(), UVC(),
                                         Red("Smith10"), Far_red("Smith10"),
                                         waveband(c(400, 500)))),
          scale.factor = 1e6, attr2tb = c("when.measured" = "time", "instr.desc"))

sun_vlate_afternoon.spct <-
  s_irrad_corrected(x = Viikki.sun.last.raw_mspct,
                    spct.names = list(light = setdiff(names(Viikki.sun.last.raw_mspct), c("filter.1", "dark.1")),
                                      filter = "filter.1",
                                      dark = "dark.1"),
                    correction.method = MAYP11278_ylianttila.mthd,
                    descriptor = MAYP11278_descriptors$cal_2023a)

autoplot(sun_vlate_afternoon.spct)

vlate_afternoon_irrads.tb <-
  q_irrad(subset2mspct(sun_vlate_afternoon.spct),
          w.band = c(Plant_bands(), list(PAR(), UVA(), UVC(),
                                         Red("Smith10"), Far_red("Smith10"),
                                         waveband(c(400, 500)))),
          scale.factor = 1e6, attr2tb = c("when.measured" = "time", "instr.desc"))

sun_vvlate_afternoon.spct <-
  s_irrad_corrected(x = viikki.sun.very.last.raw_mspct,
                    spct.names = list(light = setdiff(names(viikki.sun.very.last.raw_mspct), c("filter.1", "dark.1")),
                                      filter = "filter.1",
                                      dark = "dark.1"),
                    correction.method = MAYP11278_ylianttila.mthd,
                    descriptor = MAYP11278_descriptors$cal_2023a)

autoplot(sun_vvlate_afternoon.spct)

vvlate_afternoon_irrads.tb <-
  q_irrad(subset2mspct(sun_vvlate_afternoon.spct),
          w.band = c(Plant_bands(), list(PAR(), UVA(), UVC(),
                                         Red("Smith10"), Far_red("Smith10"),
                                         waveband(c(400, 500)))),
          scale.factor = 1e6, attr2tb = c("when.measured" = "time", "instr.desc"))

maya_irrads.tb <-
  bind_rows(noon_irrads.tb, afternoon_irrads.tb, late_afternoon_irrads.tb,
            vlate_afternoon_irrads.tb, vvlate_afternoon_irrads.tb)


maya_irrads.tb <- maya_irrads.tb[order(maya_irrads.tb$time), ]

ggplot(maya_irrads.tb, aes(time, Q_PAR)) +
  geom_point()

ggplot(maya_irrads.tb, aes(time, Q_UVA.ISO)) +
  geom_point()

ggplot(maya_irrads.tb, aes(time, Q_UVB.ISO)) +
  geom_point()

ggplot(maya_irrads.tb, aes(time, `Q_]UVC.ISO`)) +
  geom_point()

# not looking good. Is this a temperature effect?
# there could be two effects going on, the temperature change in the
# spectrometer with no new dark measurement and the "failure" of
# the filter measurement when direct sunlight is impinging the polycarbonate
# at a very shallow angle

# the estimate of UVC seems to be usable to correct the stray light/dark current
# effect on UVB irradiance measured values.

ggplot(maya_irrads.tb, aes(time, Q_UVB.ISO - `Q_]UVC.ISO` / 1.55)) +
  geom_point()

# diffuser pointing directly to the sun
late_side.spct <-
  s_irrad_corrected(x = Viikki.sun.side.raw_mspct,
                    spct.names = list(light = setdiff(names(Viikki.sun.side.raw_mspct), c("filter.1", "dark.1")),
                                      filter = "filter.1",
                                      dark = "dark.1"),
                    correction.method = MAYP11278_ylianttila.mthd,
                    descriptor = MAYP11278_descriptors$cal_2023a)

autoplot(late_side.spct)

late_sideways_irrads.tb <-
  q_irrad(subset2mspct(late_side.spct),
          w.band = c(Plant_bands(), list(PAR(), UVA(), UVC(),
                                         Red("Smith10"), Far_red("Smith10"),
                                         waveband(c(400, 500)))),
          scale.factor = 1e6, attr2tb = c("when.measured" = "time", "instr.desc"))

save(maya_irrads.tb, late_sideways_irrads.tb, file = paste(path, "irradiances_maya.Rda", sep = "/"))

# data from weather station's broadband sensors and computed sun position
load("data-rda/minute_2015_latest.tb.rda")

# merge with MAYA's irradiances
# in the logger the recorded time is that when the previous minute's values are summarized
# in the data from the Maya, times are when the "light" measurements were started

times <- maya_irrads.tb$time + minutes(1)
times <- round_date(times, unit = "minute")
maya_irrads.tb$time.orig <- maya_irrads.tb$time
maya_irrads.tb$time <- times
times

colnames(minute_2015_latest.tb)
station <- filter(minute_2015_latest.tb, time %in% times)
class(station$time)

all_irrads.tb <- left_join(station, maya_irrads.tb)

# merge with MAYA's irradiances taken "sideways"
# in the logger the recorded time is that when the previous minute's values are summarized
# in the data from the Maya, times are when the "light" measurements were started

side_times <- late_sideways_irrads.tb$time + minutes(1)
side_times <- round_date(side_times, unit = "minute")
late_sideways_irrads.tb$time.orig <- late_sideways_irrads.tb$time
late_sideways_irrads.tb$time <- side_times
side_times

side_station <- filter(minute_2015_2023.tb, time %in% side_times)
class(side_station$time)

side_irrads.tb <- left_join(side_station, late_sideways_irrads.tb)

### plots of sideways measurement

side_photon.fig <-
autoplot(subset2mspct(late_side.spct), plot.data = "mean",
annotations = c("-", "peaks"), geom = "spct") + theme_bw(10)
side_energy.fig <-
autoplot(subset2mspct(late_side.spct), plot.data = "mean", unit.out = "energy",
annotations = c("-", "peaks"), geom = "spct") + theme_bw(10)
side_energy_detail.fig <-
autoplot(subset2mspct(late_side.spct), plot.data = "mean", unit.out = "energy",
annotations = c("-", "peaks"), geom = "spct", range = c(280, 400)) +
theme_bw(10)
pdf(file = paste(path, "spectra-sideways-sun-elevation-14-degrees.pdf", sep = "/"),
onefile = TRUE,
width = 7, height = 5)
print(side_photon.fig)
print(side_energy.fig)
print(side_energy_detail.fig)
dev.off()

### Plots comparing broadband sensors' readings with last year's calibration
### against 250 measurements with the Maya taken during a whole afternoon

ggplot(all_irrads.tb, aes(month_name, air_temp_C)) +
  geom_point(position = "jitter")

ggplot(all_irrads.tb,
       aes(y = PAR_umol, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
              formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR old LI-190") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol_CS, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR new CS") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol_BF, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR total Delta-T BF5") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol_BF, x = PAR_umol_CS)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR BF5 vs. CS") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVA_umol, x = Q_UVA.ISO)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVB_umol, x = Q_UVB.ISO)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVB") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVB_umol, x = Q_UVB.ISO - `Q_]UVC.ISO` / 1.55)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVB") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = blue_umol, x = Q_range.400.500)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Violet+Blue 400-500 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = blue_sellaro_umol, x = Q_Blue.Sellaro)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Blue \"Sellaro\" 420-490 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = red_umol, x = Q_Red.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Red 655-665 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = far_red_umol, x = Q_FarRed.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Far-red 730-740 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = red_far_red, x = Q_Red.Smith10/Q_FarRed.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x) +
  stat_poly_eq(use_label(c("eq", "n", "R2")),
               rr.digits = 3,
               label.y = 0.75,
               formula = y ~ x) +
  expand_limits(x = 1, y = 1) +
  labs(title = "R:FR photon ratio \"Smith10\"") +
  theme_bw()

# test effect of temperature

uva.lm <- lm(Q_UVA.ISO ~ UVA_umol * air_temp_C + 0, data = all_irrads.tb)
summary(uva.lm)

uvb.lm <- lm(Q_UVB.ISO ~ UVB_umol * air_temp_C + 0, data = all_irrads.tb)
summary(uvb.lm)

blue.lm <- lm(Q_range.400.500 ~ blue_umol * air_temp_C + 0, data = all_irrads.tb)
summary(blue.lm)

# test UVA1 and UVA2
# range of solar elevations is too small???

# uva1.lm <- lm(Q_UVA1.CIE ~ UVA_umol + UVB_umol + 0, data = all_irrads.tb)
# summary(uva1.lm)
#
# uva2.lm <- lm(Q_UVA2.CIE ~ UVA_umol + blue_sellaro_umol + 0, data = all_irrads.tb)
# summary(uva2.lm)
#
# uvb.lm <- lm(Q_UVB.ISO ~ UVA_umol + UVB_umol + 0, data = all_irrads.tb)
# summary(uvb.lm)
#
# all_irrads.tb %>%
#   mutate(UVA2_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
#          UVA1_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
#          UVB_corr_umol = 0.0018777 * UVA_umol + 0.9331867 * UVB_umol) -> all_irrads.tb

ggplot(all_irrads.tb, aes(y = UVA1_umol, x = Q_UVA1.CIE)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x + 0) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
                formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA1 computed from UV-A and Blue") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVA2_umol, x = Q_UVA2.CIE)) +
  geom_abline(linetype = "dashed") +
  stat_quant_band(formula = y ~ x + 0) +
  geom_point() +
  stat_quant_eq(use_label(c("eq", "n")),
             formula = y ~ x + 0) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA2 computed from UV-A and UV-B") +
  theme_bw()



