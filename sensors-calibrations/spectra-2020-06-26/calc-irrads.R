library(photobiology)
library(photobiologyWavebands)
library(photobiologyPlants)
oldwd <- setwd("sensors-calibrations/spectra-2020-06-26")
on.exit(setwd(oldwd))

load("sun001.spct.Rda")
load("sun002.spct.Rda")
load("sun003.spct.Rda")
sun_2020_06_26.mspct <- collect2mspct()

save(sun_2020_06_26.mspct, file = "../sun-2020-06-26.mspct.rda")

my_plant_bands <- list(UVB(), UVA1(), UVA2(),
                       waveband(c(400, 500), wb.name = "blue"),
                       Red("Smith10"), Far_red("Smith10"),
                       PAR())
q_irrad(sun_2020_06_26.mspct, my_plant_bands, scale.factor = 1e6,
        attr2tb = c("when.measured")) -> sun_q_irrad.tb

sun_q_irrad.tb
R_FR(sun_2020_06_26.mspct)

