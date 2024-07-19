library(photobiology)
library(photobiologyWavebands)
library(ggspectra)
library(lubridate)
library(ggpmisc)
library(quantreg)
library(dplyr)
library(patchwork)

# theme_set(theme_bw())
theme_set(theme_classic())

rm(list = ls(pattern = "*"))

photon_as_default()

make_fig_pdfs <- FALSE
whole_year <- FALSE

# data from weather station's broadband sensors and computed sun position
load("data-rda/minute_2015_latest.tb.rda")

colnames(minute_2015_latest.tb)

# filter what rows we use to ensure we use same data for all plots
if (whole_year) {
  minute.tb <- filter(minute_2015_latest.tb,
                      !is.na(blue_umol) &
                        sun_elevation > 0 &
                        PAR_umol < 2250)

} else {
  minute.tb <- filter(minute_2015_latest.tb,
                      !is.na(blue_umol) &
                        sun_elevation > 0 &
                        PAR_umol < 2250 &
                        month_of_year > 4 & month_of_year < 11)

}
range(minute.tb$time)

minute.tb %>%
  select(sun_elevation, red_far_red) %>%
  na.omit() %>%
  ggplot(aes(sun_elevation, red_far_red)) +
  stat_quant_band(formula = y ~ poly(x, 11), method = "rq:fn",
                  quantiles = c(0.05, 0.5, 0.95), alpha = 0.2) +
  stat_quant_band(formula = y ~ poly(x, 11), method = "rq:fn") +
  #  stat_quant_band(formula = y ~ poly(x, 9), method = "rq:fn") +
#  stat_smooth(na.rm = TRUE, level = 0.99) +
  geom_hline(yintercept = 1.16,  linetype = "dashed", color = "red") +
  labs(#title = "R:FR ratio in Viikki, 2020 to 2023",
       x = "Solar elevation angle (degrees)",
       y = "Red to far-red photon ratio (/1)",
       tag = "A") +
  expand_limits(y = c(0.3, 2)) +
  coord_cartesian(ylim = c(0.75, 1.5)) +
  stat_quadrant_counts(quadrants = 0L,
                       label.x = 0.9,
                       label.y = 0.05) +
  theme(plot.tag.position = c(0.1, 0.95),
        plot.tag = element_text(hjust = 0, size = 10))  -> rfr_sun_elev.fig
rfr_sun_elev.fig

minute.tb %>%
  select(sun_elevation, blue_red) %>%
  na.omit() %>%
  ggplot(aes(sun_elevation, blue_red)) +
  stat_quant_band(formula = y ~ poly(x, 11), method = "rq:fn",
                  quantiles = c(0.05, 0.5, 0.95), alpha = 0.2) +
  stat_quant_band(formula = y ~ poly(x, 11), method = "rq:fn") +
  #  stat_smooth(na.rm = TRUE, level = 0.99) +
#  geom_hline(yintercept = 1.16,  linetype = "dashed", color = "red") +
  labs(#title = "B:R ratio in Viikki, 2020 to 2023",
       x = "Solar elevation angle (degrees)",
       y = "Blue to red photon ratio",
       tag = "C") +
#  ylim(0.3, 2) +
  stat_quadrant_counts(quadrants = 0L,
                       label.x = 0.9,
                       label.y = 0.05) +
  theme(plot.tag.position = c(0.1, 0.95),
        plot.tag = element_text(hjust = 0, size = 10)) -> br_sun_elev.fig
br_sun_elev.fig

minute.tb %>%
  select(sun_elevation, UVB_PAR) %>%
  subset(sun_elevation > 0) %>%
  na.omit() %>%
  ggplot(aes(sun_elevation, UVB_PAR * 1e3)) +
  stat_quant_band(formula = y ~ x, method = "rq:fn",
                  quantiles = c(0.05, 0.5, 0.95), alpha = 0.2) +
  stat_quant_band(formula = y ~ x, method = "rq:fn") +
#  stat_quant_band(na.rm = TRUE, formula = y ~ poly(x, 9), method = "rq:fn") +
  #  geom_hline(yintercept = 1.16,  linetype = "dashed", color = "red") +
  labs(#title = "B:R ratio in Viikki, 2020 to 2023",
    x = "Solar elevation angle (degrees)",
    y = "UV-B to PAR photon ratio (/1000)",
    tag = "B") +
  stat_quadrant_counts(quadrants = 0L,
                       label.x = 0.9,
                       label.y = 0.05) +
  expand_limits(x = 0) +
  coord_cartesian(ylim = c(0, 3)) +
  theme(plot.tag.position = c(0.1, 0.95),
        plot.tag = element_text(hjust = 0, size = 10)) -> uvb_par_sun_elev.fig
uvb_par_sun_elev.fig

pdf("sunfleck-review-figs/ratios-fig.pdf", width = 7, height = 8)
rfr_sun_elev.fig / uvb_par_sun_elev.fig +
  plot_layout(axis_titles = "collect_x")
dev.off()

