library(photobiology)
library(photobiologyWavebands)
library(lubridate)
library(ggpmisc)
library(dplyr)

rm(list = ls(pattern = "*"))

# load("sensors-calibrations/spectra-2021-summer/collection.sun.july.4.Rda")

load("./sensors-calibrations/maxime-2020-mspct.rda")
load("./sensors-calibrations/sun-2020-06-26.mspct.rda")

all_spectra <- c(maxime_2020.mspct, sun_2020_06_26.mspct)

load("sensors-calibrations/maxime-2021-mspct.rda")
load("sensors-calibrations/sun-2021-06-17.mspct.rda")
load("sensors-calibrations/sun-2021-07-04.mspct.rda")
load("sensors-calibrations/sun-2021-10-19.mspct.rda")
load("sensors-calibrations/sun-2021-10-19b.mspct.rda")

all_spectra <- c(maxime_2020.mspct,
                 sun_2020_06_26.mspct,
                 maxime_2021.mspct,
                 collection.sun.july.4.irrad.mspct,
                 collection.sun.noon.irrad.mspct,
                 collection.long.integ.irrad.mspct,
                 collection.noon.irrad.mspct)

paths <- c("sensors-calibrations/spectra-2022-04-22",
           "sensors-calibrations/spectra-2022-05-11")

for (path in paths) {
  cat(path, "\n")
  files <- list.files(path = path,
                      pattern = "\\.spct\\.Rda",
                      full.names = TRUE)

  for (f in files) {
    load(f)
    spct.name <- gsub("\\.Rda$", "", basename(f))
    temp.spct <- get(spct.name)
    when <- when_measured(temp.spct)
    member.name <- paste(sub("\\.spct$", "", spct.name),  day(when), month.abb[month(when)], sep = ".")
    uvc.irrad <- q_irrad(temp.spct, UVC(), scale.factor = 1e6)
    if (uvc.irrad < 0.1) {
      cat("adding: ")
      all_spectra[[member.name]] <- temp.spct
    } else {
      cat("skipping: ")
    }
    cat(spct.name, "UVC = ", uvc.irrad, "umol m-2 s-1\n")
  }
}
rm(list = ls(pattern = "spct$"))
names(all_spectra)

discard <- which(abs(q_irrad(all_spectra, UVC(), scale.factor = 1e6)[["Q_]UVC.ISO"]]) > 0.1)
all_spectra <- all_spectra[-discard]
length(all_spectra)

maya_irrads.tb <-
  q_irrad(all_spectra,
          w.band = c(Plant_bands(), list(PAR(), UVA(),
                                         Red("Smith10"), Far_red("Smith10"),
                                         waveband(c(400, 500)))),
          scale.factor = 1e6, attr2tb = c("when.measured" = "time", "instr.desc"))

instr_desc.ls <- maya_irrads.tb$instr.desc
sn <- sapply(instr_desc.ls, `[[`, i = "spectrometer.sn")
maya_irrads.tb$serial.no <- unname(sn)
maya_irrads.tb$instr.desc <- NULL

times <- maya_irrads.tb$time + minutes(1) + seconds(5)
second(times) <- 0
maya_irrads.tb$time.orig <- maya_irrads.tb$time
maya_irrads.tb$time <- times
times

load("data-rda/minute_2015_2022.tb.rda")

colnames(minute_2015_2022.tb)
station <- filter(minute_2015_2022.tb, time %in% times)
class(station$time)

all_irrads.tb <- left_join(station, maya_irrads.tb)

ggplot(all_irrads.tb, aes(month_name, air_temp_C)) +
  geom_point(position = "jitter")

ggplot(all_irrads.tb,
       aes(y = PAR_umol, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR old LI-190") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol_CS, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR new CS") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol_BF, x = Q_PAR)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR total Delta-T BF5") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = PAR_umol_BF, x = PAR_umol_CS)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAR BF5 vs. CS") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVA_umol, x = Q_UVA.ISO)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVB_umol, x = Q_UVB.ISO)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVB") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = blue_umol, x = Q_range.400.500)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Violet+Blue 400-500 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = blue_sellaro_umol, x = Q_Blue.Sellaro)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Blue \"Sellaro\" 420-490 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = red_umol, x = Q_Red.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Red 655-665 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = far_red_umol, x = Q_FarRed.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "Far-red 730-740 nm") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = red_far_red, x = Q_Red.Smith10/Q_FarRed.Smith10)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
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
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA1 computed from UV-A and Blue") +
  theme_bw()

ggplot(all_irrads.tb, aes(y = UVA2_umol, x = Q_UVA2.CIE)) +
  geom_abline(linetype = "dashed") +
  stat_smooth(method = lm, formula = y ~ x - 1) +
  geom_point(aes(shape = factor(calendar_year), colour = month_name)) +
  stat_poly_eq(aes(label =  paste(stat(eq.label),
                                  stat(adj.rr.label), sep = "*\", \"*")),
               formula = y ~ x - 1, parse = TRUE) +
  expand_limits(x = 0, y = 0) +
  labs(title = "UVA2 computed from UV-A and UV-B") +
  theme_bw()

