library(photobiology)
library(photobiologyWavebands)
library(dplyr)

rm(list = ls(pattern = "*"))

viikki_bio3.geo <- data.frame(lon = 25.01673, lat = 60.2253, address = "BIO3, Viikki")
locale_UTC <- locale(tz = "UTC")

folder <- "acq-irrad-2022-04-22"
files <- list.files(folder, pattern = "sun0[0-9][0-9]\\.spct\\.Rda", full.names = TRUE)

for (f in files) load(f, verbose = TRUE)
rm(list = ls(pattern = "raw_mspct$"))
sun_clear_sky_cos.mspct <- collect2mspct()
rm(list = ls(pattern = "\\.spct$"))

wavebands <- c(Plant_bands("CIE"),
               list(Blue("Sellaro"), Green("Sellaro"), Red("Smith10"), Far_red("Smith10")))

summaries <-
  full_join(
    q_irrad(sun_clear_sky_cos.mspct, wavebands, scale.factor = 1e6, attr2tb = c("when.measured" = "time")),
    q_ratio(sun_clear_sky_cos.mspct, wavebands, PAR(), attr2tb = c("when.measured" = "time"))
  ) %>%
  mutate(sun_elevation = sun_elevation(time = time, geocode = viikki_bio3.geo),
         solar_time = solar_time(time = time, geocode = viikki_bio3.geo, unit.out = "datetime"))


