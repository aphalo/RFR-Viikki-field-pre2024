# read TUV spectra for different SZA

library(photobiologyInOut)
library(ggspectra)
library(photobiologyWavebands)

files <- list.files("TUV-diffuse-direct-SZA/", pattern = "4str", full.names = TRUE)

SZA.mspct <- source_mspct()

for (f in files) {
  name <- gsub("tuv-default-|-4str.htm", "", basename(f))
  print(name)
  SZA.mspct[[name]] <- read_qtuv_txt(f, ozone.du = 300)
}

SZA_tot.mspct <- source_mspct()
SZA_diff.mspct <- source_mspct()
SZA_dir.mspct <- source_mspct()
for (n in names(SZA.mspct)) {
  SZA_tot.mspct[[n]] <-
    SZA.mspct[[n]][ , c("w.length", "s.e.irrad")]
  SZA_diff.mspct[[n]] <-
    with(SZA.mspct[[n]], source_spct(w.length = w.length, s.e.irrad = s.e.irrad.diff.down))
  SZA_dir.mspct[[n]] <-
    with(SZA.mspct[[n]], source_spct(w.length = w.length, s.e.irrad = s.e.irrad.dir))
}

q_irrad(SZA.mspct, PAR(), scale.factor = 1e6)
PAR_tot.tb <- q_irrad(SZA_tot.mspct, PAR(), scale.factor = 1e6)
PAR_dir.tb <- q_irrad(SZA_dir.mspct, PAR(), scale.factor = 1e6)
PAR_diff.tb <- q_irrad(SZA_diff.mspct, PAR(), scale.factor = 1e6)

PAR.tb <-
  data.frame(
    SZA = as.numeric(gsub("sza", "", PAR_tot.tb$spct.idx)),
    SEA = 90 - as.numeric(gsub("sza", "", PAR_tot.tb$spct.idx)),
    total = PAR_tot.tb$`Q_PAR[`,
    direct = PAR_dir.tb$`Q_PAR[`,
    diffuse = PAR_diff.tb$`Q_PAR[`,
    diff_fraction = PAR_diff.tb$`Q_PAR[` / PAR_tot.tb$`Q_PAR[`
  )

ggplot(PAR.tb, aes(SEA, diff_fraction)) +
  geom_line() +
  expand_limits(y = 0)
