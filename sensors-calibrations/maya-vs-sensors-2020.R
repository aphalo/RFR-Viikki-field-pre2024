library(photobiology)
library(photobiologyWavebands)
library(lubridate)
library(ggpmisc)
library(dplyr)

rm(list = ls(pattern = "*"))

load("./sensors-calibrations/maxime-2020-mspct.rda")
load("./sensors-calibrations/sun-2020-06-26.mspct.rda")

all_spectra.mspct <- c(maxime_2020.mspct, sun_2020_06_26.mspct)
# if a spectrum shows UVC in sunlight it means stray-light correction has failed
discard <- which(abs(q_irrad(all_spectra.mspct, UVC(), scale.factor = 1e6)[["Q_]UVC.ISO"]]) > 0.1)
all_spectra.mspct <- all_spectra.mspct[-discard]

length(all_spectra.mspct)

maya_irrads.tb <-
  q_irrad(all_spectra.mspct,
          w.band = c(Plant_bands(), list(PAR(), UVA(),
                                         Red("Smith10"), Far_red("Smith10"),
                                         waveband(c(400, 500)))),
          scale.factor = 1e6, attr2tb = c("when.measured" = "time", "instr.desc"))

instr_desc.ls <- maya_irrads.tb$instr.desc
sn <- sapply(instr_desc.ls, `[[`, i = "spectrometer.sn")
maya_irrads.tb$serial.no <- sn
maya_irrads.tb$instr.desc <- NULL

# logger uses time at end of 1 min logging period
# while spectrometer uses time at start of measurement
times <- maya_irrads.tb$time + minutes(1) + seconds(5)
second(times) <- 0
maya_irrads.tb$time.orig <- maya_irrads.tb$time
maya_irrads.tb$time <- times
times

load("data-rda/minute_2015_2022.tb.rda")
colnames(minute_2015_2022.tb)
station <- filter(minute_2015_2022.tb, time %in% times)
class(station$time)

all_irrads.tb <- full_join(station, maya_irrads.tb) %>%
  mutate(PAR_umol_mean = (PAR_umol + PAR_umol_BF) / 2)

ggplot(all_irrads.tb, aes(month_name, air_temp_C)) +
  geom_point()

ggplot(all_irrads.tb, aes(y = PAR_umol_mean, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol_BF, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol_BF, x = PAR_umol)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVA_umol, x = Q_UVA.ISO)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVB_umol, x = Q_UVB.ISO)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVB") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = blue_umol, x = Q_range.400.500)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Violet+Blue 400-500 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = blue_sellaro_umol, x = Q_Blue.Sellaro)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Blue \"Sellaro\" 420-490 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = red_umol, x = Q_Red.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Red 655-665 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = far_red_umol, x = Q_FarRed.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Far-red 730-740 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = red_far_red, x = Q_Red.Smith10/Q_FarRed.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               rr.digits = 4,
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 1, y = 1) +
  labs(title = "R:FR photon ratio \"Smith10\"") +
  theme_bw()

# test UVA1 and UVA2
# range of solar elevations is too small

# uva1.lm <- lm(Q_UVA1.CIE ~ UVA_umol * UVB_umol, data = all_irrads.tb)
# summary(uva1.lm)
#
# uva2.lm <- lm(Q_UVA2.CIE ~ UVA_umol * blue_sellaro_umol, data = all_irrads.tb)
# summary(uva2.lm)
#
# using equations from 2021
#
# all_irrads.tb %>%
#   mutate(UVA2_umol = 0.73747 * UVA_umol + 0.03433 * blue_sellaro_umol,
#          UVA1_umol = 0.157966 * UVA_umol + 1.364312 * UVB_umol,
#          UVB_corr_umol = 0.005626 * UVA_umol + 0.547995 * UVB_umol) -> all_irrads.tb

ggplot(all_irrads.tb, aes(y = UVA1_umol, x = Q_UVA1.CIE)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA1") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVA2_umol, x = Q_UVA2.CIE)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = serial.no, colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA2") +
  theme_bw()

