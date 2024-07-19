# read and merge collections and make new summaries
library(ggspectra)
library(ooacquire)

photon_as_default()

collection.hemispherical <- c(collection.first.two.hours.irrad.mspct,
                              collection.third.to.fifth.hours.irrad.mspct)

summaries.hemispherical <- irrad_summary_table(collection.hemispherical,
                                               type = "PAR",
                                               digits = 5)
colnames(summaries.hemispherical)

ggplot(summaries.hemispherical, aes(when.measured, Q_PAR)) +
  geom_line()

ggplot(summaries.hemispherical, aes(when.measured, `Q_]UVC.CIE`)) +
  geom_line()

ggplot(summaries.hemispherical, aes(when.measured, Q_UVB.CIE - `Q_]UVC.CIE` * 0.55)) +
  geom_line()

ggplot(summaries.hemispherical, aes(when.measured, Q_UVA1.CIE)) +
  geom_line()

ggplot(summaries.hemispherical, aes(when.measured, Q_UVA2.CIE)) +
  geom_line()

ggplot(summaries.hemispherical, aes(when.measured + lubridate::hours(3),
                                    (Q_UVB.CIE - `Q_]UVC.CIE` * 0.55) / Q_PAR * 1e3)) +
  geom_line() +
  labs(y = "UV-B : PAR photon ratio (x 1000)",
       x = "Local summer time (hh:mm)")

