# example with spectra

library(r4photobiology)
library(photobiologyPlants)
library(lubridate)

viikki_bio3.geo <- data.frame(lon = 25.01673, lat = 60.2253, address = "BIO3, Viikki")

# no need to save to a variable, we load ready made R objects with their
# original names.
# this command loads the R objects earlier saved during data acquisition
# Two objects have been saved: a source_spct and a raw_mspct.
#
load("spectra/field6.spct.Rda")
getInstrDesc(field6.spct)
getInstrSettings(field6.spct)

getWhatMeasured(field6.spct)
with_tz(getWhenMeasured(field6.spct), tzone = "EET")
setWhereMeasured(field6.spct, viikki_bio3.geo)
getWhereMeasured(field6.spct)

photon_as_default() # requires latest CRAN version of 'photobiology'

plot(field6.spct, annotations = c("+", "title:when:where"))

R_FR(field6.spct)

q_irrad(field6.spct, list(Red("Smith10"), Far_red("Smith10")))


