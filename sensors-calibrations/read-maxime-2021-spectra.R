library(ooacquire)
library(ggspectra)
library(lubridate)

file_paths <- function(path, event) {
  list(light = paste(path, "/open", event, ".txt", sep = ""),
       filter = paste(path, "/open", event, "PC.txt", sep = ""),
       dark = paste(path, "/open", event, "dark.txt", sep = ""))
}

# dirs <- list.dirs("./sensors-calibrations/spectra-from-Maxime-2020/forPedro")[-1]
dirs <- list.dirs("./sensors-calibrations/spectra-from-Maxime-2021")[-1]

for (dir in dirs) {
  date <- ymd(gsub("^.*/.*/", "", dir))
#  date <- dmy(gsub("^.*/.*/", "", dir))
  date.char <- gsub("-", "", as.character(date))

  for (event in c("start", "mid", "end")) {
    if (file.exists(file_paths(dir, event)[[1]])) {
      serial.no <- scan(file_paths(dir, event)[[1]], nmax = 2, skip = 7, what = "character")[2]
      spct_name <- paste(event, date.char, ".spct", sep = "")
      temp_spct <- s_irrad_corrected(file_paths(dir, event),
                                     descriptor = which_descriptor(date,
                                                                   get(paste(serial.no, "descriptors", sep = "_"))),
                                     correction.method = get(paste(serial.no, "ylianttila.mthd", sep = "_")))
      assign(spct_name, temp_spct)
    }
  }
}

rm(temp_spct)

maxime_2021.mspct <- collect2mspct()
autoplot(maxime_2021.mspct, facets = FALSE)
autoplot(smooth_spct(maxime_2021.mspct), facets = FALSE)

save(maxime_2021.mspct, file = "./sensors-calibrations/maxime-2021-mspct.rda")
