library(lubridate)
library(ggpmisc)
library(ggrepel)
library(dplyr)

load("data-rda/hour_soil_calc_2015_2023.tb.rda")
range(hour_soil_calc_2015_2023.tb$time)

hour_soil_calc_2015_2023.tb %>%
filter(time > today() - weeks(2)) -> last_weeks.tb
range(last_weeks.tb$time)

ggplot(last_weeks.tb, aes(time, rain_mm_h)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 3,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))


ggplot(last_weeks.tb, aes(time, air_DP_mean)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))


ggplot(last_weeks.tb, aes(time, air_temp_C_mean)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "red", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  stat_valleys(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "blue", angle = 90, hjust = 1.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)))


ggplot(last_weeks.tb, aes(time, wind_speed_mean)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "red", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  stat_valleys(geom = "text_repel", ignore_threshold = 0.01, span = 11,
               colour = "blue", angle = 90, hjust = 1.1,
               x.label.fmt = "%d %b %H %Z", size = 3,
               min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)))

ggplot(last_weeks.tb, aes(time, wind_direction_mean)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "red", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  stat_valleys(geom = "text_repel", ignore_threshold = 0.01, span = 21,
               colour = "blue", angle = 90, hjust = 1.1,
               x.label.fmt = "%d %b %H %Z", size = 3,
               min.segment.length = 0) +
  geom_hline(yintercept = c(0, 360), linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 90, 180, 270, 360),
                     expand = expansion(mult = c(0.25, 0.25)))


ggplot(last_weeks.tb, aes(time, surf_temp_C_mean)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "red", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  stat_valleys(geom = "text_repel", ignore_threshold = 0.01, span = 11,
               colour = "blue", angle = 90, hjust = 1.1,
               x.label.fmt = "%d %b %H %Z", size = 3,
               min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)))

ggplot(last_weeks.tb, aes(time, temp_surf2air_C_mean)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "red", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  stat_valleys(geom = "text_repel", ignore_threshold = 0.01, span = 11,
               colour = "blue", angle = 90, hjust = 1.1,
               x.label.fmt = "%d %b %H %Z", size = 3,
               min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)))


ggplot(last_weeks.tb, aes(time, VWC_3)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_2)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_1)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))



ggplot(last_weeks.tb, aes(time, T_10cm_3)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, T_10cm_2)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, T_10cm_1)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))


ggplot(last_weeks.tb, aes(time, VWC_10cm_3)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_10cm_2)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_10cm_1)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))


ggplot(last_weeks.tb, aes(time, VWC_20cm_3)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_20cm_2)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_20cm_1)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))


ggplot(last_weeks.tb, aes(time, VWC_30cm_3)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_30cm_2)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0)

ggplot(last_weeks.tb, aes(time, VWC_30cm_1)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))


ggplot(last_weeks.tb, aes(time, VWC_40cm_3)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_40cm_2)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_40cm_1)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))


ggplot(last_weeks.tb, aes(time, VWC_50cm_3)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_50cm_2)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(last_weeks.tb, aes(time, VWC_50cm_1)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 15,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

load("fmi-data-wide.Rda")
range(wide_data$time)

wide_data %>%
  filter(time > today() - weeks(2)) -> k_last_weeks.tb
range(last_weeks.tb$time)

ggplot(k_last_weeks.tb, aes(time, PRA_PT1H_ACC)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 3,
             colour = "blue", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25)))

ggplot(k_last_weeks.tb, aes(time, TA_PT1H_AVG)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "red", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  stat_valleys(geom = "text_repel", ignore_threshold = 0.01, span = 11,
               colour = "blue", angle = 90, hjust = 1.1,
               x.label.fmt = "%d %b %H %Z", size = 3,
               min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)))

ggplot(k_last_weeks.tb, aes(time, WS_PT1H_AVG)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "red", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  stat_valleys(geom = "text_repel", ignore_threshold = 0.01, span = 11,
               colour = "blue", angle = 90, hjust = 1.1,
               x.label.fmt = "%d %b %H %Z", size = 3,
               min.segment.length = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.25, 0.25)))

ggplot(k_last_weeks.tb, aes(time, WD_PT1H_AVG)) +
  geom_line() +
  stat_peaks(geom = "text_repel", ignore_threshold = 0.01, span = 11,
             colour = "red", angle = 90, hjust = -0.1,
             x.label.fmt = "%d %b %H %Z", size = 3,
             min.segment.length = 0) +
  stat_valleys(geom = "text_repel", ignore_threshold = 0.01, span = 21,
               colour = "blue", angle = 90, hjust = 1.1,
               x.label.fmt = "%d %b %H %Z", size = 3,
               min.segment.length = 0) +
  geom_hline(yintercept = c(0, 360), linetype = "dashed") +
  scale_y_continuous(breaks = c(0, 90, 180, 270, 360),
                     expand = expansion(mult = c(0.25, 0.25)))

