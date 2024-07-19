library(dplyr)
library(photobiology)

# hour.tb %>%
#   select(TIMESTAMP, time_of_day, contains("_Hst")) ->
#   hour_histograms.tb
# colnames(hour_histograms.tb)
#

# hour.tb %>%
#   select(-contains("_Hst")) ->
#   hour_summaries.tb
# colnames(hour_summaries.tb)

load("data-rda/day_2016_2.tb.rda")

load("data-rda/day_2019_march.tb.rda")

load("data-rda/day_2019_april_02.tb.rda")

load("data-rda/day_2019_november_15.tb.rda")

load("data-rda/day_2020_01_14.tb.rda")

load("data-rda/day_2020.tb.rda")

bind_rows(day_2016_2.tb, day_2019_march.tb, day_2019_april_02.tb,
          day_2019_november_15.tb, day_2020_01_14.tb, day_2020.tb) %>%
  mutate(.,
         PAR_Day_Tot = PAR_Den_Avg * 24 * 3600 * 1e-6) -> day_2015_2022.tb

day_2015_2022.tb <-
  distinct(day_2015_2022.tb, TIMESTAMP, .keep_all = TRUE)

save(day_2015_2022.tb, file = "data-rda/day_2015_2022.tb.rda")

