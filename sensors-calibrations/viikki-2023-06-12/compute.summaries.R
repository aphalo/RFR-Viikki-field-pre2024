library(photobiology)
library(photobiologyWavebands)
library(ggpp)

files <- list.files(pattern = "^cosine.*Rda")
for (f in files) load(f)
cosine.mspct <- collect2mspct(list = ls(pattern = "\\.spct$"))
rm(list = ls(pattern = "\\.spct$|raw_mspct"))
summary(cosine.mspct)

files <- list.files(pattern = "^hemis.*Rda")
for (f in files) load(f)
hemis.mspct <- collect2mspct(list = ls(pattern = "\\.spct$"))
rm(list = ls(pattern = "\\.spct$|raw_mspct"))

cosine.mspct <- subset2mspct(cosine.mspct)
cosine_PAR.tb <- q_irrad(cosine.mspct, PAR(), attr2tb = "when.measured", scale.factor = 1e6)
cosine_PAR.tb[["diffuser"]] <- "flat"
cosine_plant.tb <- q_irrad(cosine.mspct, c(list(UVC(), NIR()), Plant_bands()),
                           attr2tb = "when.measured", scale.factor = 1e6)
cosine_plant.tb[["Q_UVB.corrected"]] <-
  cosine_plant.tb[["Q_UVB.ISO"]] - cosine_plant.tb[["Q_]UVC.ISO"]] * 0.67 # 0.75 for Luke's Maya, 0.67 for new Maya
cosine_plant.tb[["diffuser"]] <- "flat"

hemis.mspct <- subset2mspct(hemis.mspct)
hemis_PAR.tb <- q_irrad(hemis.mspct, PAR(), attr2tb = "when.measured", scale.factor = 1e6)
hemis_PAR.tb[["diffuser"]] <- "dome"
hemis_plant.tb <- q_irrad(hemis.mspct, c(list(UVC(), NIR()), Plant_bands()),
                          attr2tb = "when.measured", scale.factor = 1e6)
hemis_plant.tb[["Q_UVB.corrected"]] <-
  hemis_plant.tb[["Q_UVB.ISO"]] - hemis_plant.tb[["Q_]UVC.ISO"]] * 0.57
hemis_plant.tb[["diffuser"]] <- "dome"

all_PAR.tb <- rbind(cosine_PAR.tb, hemis_PAR.tb)
all_PAR.tb[["diffuser"]] <- factor(all_PAR.tb[["diffuser"]])
all_plant.tb <- rbind(cosine_plant.tb, hemis_plant.tb)
all_plant.tb[["diffuser"]] <- factor(all_plant.tb[["diffuser"]])

ggplot(all_PAR.tb, aes(x = when.measured, y = Q_PAR, colour = diffuser)) +
  geom_line()

ggplot(all_plant.tb, aes(x = when.measured, y = `Q_]UVC.ISO`, colour = diffuser)) +
  geom_line()

ggplot(all_plant.tb, aes(x = when.measured, y = Q_UVB.ISO, colour = diffuser)) +
  geom_line()

ggplot(all_plant.tb, aes(x = when.measured, y = Q_UVB.corrected, colour = diffuser)) +
  geom_line() +
  geom_point() +
  stat_group_counts()

ggplot(all_plant.tb, aes(x = when.measured, y = Q_UVA2.CIE, colour = diffuser)) +
  geom_line()

ggplot(all_plant.tb, aes(x = when.measured, y = Q_UVA1.CIE, colour = diffuser)) +
  geom_line()
ggplot(all_plant.tb, aes(x = when.measured, y = Q_FarRed.Smith20, colour = diffuser)) +
  geom_line()
ggplot(all_plant.tb, aes(x = when.measured, y = Q_Red.Smith20, colour = diffuser)) +
  geom_line()
ggplot(all_plant.tb, aes(x = when.measured, y = Q_Red.Smith20/Q_FarRed.Smith20, colour = diffuser)) +
  geom_line()
ggplot(all_plant.tb, aes(x = when.measured, y = Q_Blue.Sellaro, colour = diffuser)) +
  geom_line()
ggplot(all_plant.tb, aes(x = when.measured, y = Q_Green.Sellaro, colour = diffuser)) +
  geom_line()
ggplot(all_plant.tb, aes(x = when.measured, y = Q_Blue.Sellaro/Q_Green.Sellaro, colour = diffuser)) +
  geom_line()
ggplot(all_plant.tb, aes(x = when.measured, y = Q_UVA2.CIE/Q_Blue.Sellaro, colour = diffuser)) +
  geom_line()
ggplot(all_plant.tb, aes(x = when.measured, y = Q_UVB.corrected/Q_Blue.Sellaro, colour = diffuser)) +
  geom_line()
