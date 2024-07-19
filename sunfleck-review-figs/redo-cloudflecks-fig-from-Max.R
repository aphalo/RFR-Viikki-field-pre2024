library(ggplot2)
library(ggpmisc)
library(ggdensity)
library(quantreg)
library(ggspectra)
library(photobiologyWavebands)
library(patchwork)
library(svglite)

options(ggspectra.add.symbols = FALSE)

# theme_set(theme_bw())
theme_set(theme_classic())

dfS <- read.csv("sunfleck-review-figs/dfCloudfleck_figure.csv")

ggplot(data = dfS, aes(percDiff, duration)) +
  geom_point()

p2 <-
  ggplot(data = dfS, aes(percDiff * 100, duration)) +
#  geom_vline(xintercept = 0.6, linetype = "dashed") +
#  stat_quant_band(formula = y ~ poly(x, 7)) +
  stat_quant_band(formula = y ~ qss(x, lambda = 50, constraint = "I"), method = "rqss:sfnc",
                  quantiles = c(0.05, 0.5, 0.95), alpha = 0.2) +
  stat_quant_band(formula = y ~ qss(x, lambda = 50, constraint = "I"), method = "rqss:sfnc") +
  stat_quadrant_counts(quadrants = 0L,
                       label.x = 0.95,
                       label.y = 0.05) +
  labs(x = "PAR attenuation during cloudfleck (%)",
       y = "Cloudfleck duration (min)")
p2 + scale_y_continuous(breaks = c(0, 60, 120, 180, 240, 300))

p2 + scale_y_log10(breaks = c(2, 5, 10, 20, 50, 100, 200, 500))

# pdf("sunfleck-review-figs/cloudflecks-fig.pdf", width = 7, height = 6)
# print(p2 + scale_y_continuous(breaks = c(0, 60, 120, 180, 240, 300)))
# dev.off()

p3 <- ggplot(data = dfS, aes(duration)) +
  stat_density(bw = "nrd", alpha = 0.3) +
  scale_x_log10(breaks = c(2, 5, 10, 20, 50, 100, 200, 500)) +
  expand_limits(x = c(1.7, 500)) +
  labs(x = "Cloudfleck duration (min)",
       y = "Empirical probability density")

p3

pdf("sunfleck-review-figs/cloudflecks-fig.pdf", width = 7, height = 5)
print(p2 + scale_y_continuous(breaks = c(0, 60, 120, 180, 240, 300)) +
  annotate(geom = "plot", x = I(0.05), y = I(1), label = p3, vp.width = 2/3, vp.height = 1/2))
dev.off()

## cloudfleck spectrum

photon_as_default()

CSCLDY.df <- read.csv("sunfleck-review-figs/dfCSCLDY.csv")[ , -1]

names(CSCLDY.df)[names(CSCLDY.df) == "s.q.irrad"] <- "s.e.irrad"

summary(CSCLDY.df)
CSCLDY.df <- subset(CSCLDY.df, w.length > 295 & w.length < 850)
CSCLDY.df$type.label <- gsub("\\.", " ", CSCLDY.df$type)

CSCLDY.spct <- as.source_spct(CSCLDY.df,
                              idfactor = "type",
                              multiple.wl = length(unique(CSCLDY.df$type)))

CSCLDY.mspct <- e2q(subset2mspct(CSCLDY.spct))
names(CSCLDY.mspct)

autoplot(CSCLDY.mspct)

q_irrad(CSCLDY.mspct, w.band = PAR(), scale.factor = 1e6)


p.cloudy.scaled <-
  ggplot(data = fscale(CSCLDY.spct,
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, colour = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.015e-6, ymax = -0.003e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6, scaled = TRUE) +
  scale_x_wl_continuous() +
  scale_colour_manual(values = c("steelblue3", "grey20")) +
  labs(tag = "B") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10)) +
  theme(legend.position = "none")
p.cloudy.scaled

p.cloudy.irrad <-
  ggplot(data = e2q(CSCLDY.spct),
         aes(w.length, s.q.irrad, colour = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.19e-6, ymax = -0.03e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  scale_colour_manual(values = c("steelblue3", "grey20")) +
  labs(tag = "A") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10)) +
  theme(legend.position = "none")
p.cloudy.irrad

p.cloudy <- p.cloudy.irrad / p.cloudy.scaled +
  plot_layout(axis_titles = "collect")
p.cloudy

pdf("sunfleck-review-figs/cloudy-fig.pdf", width = 7, height = 8)
print(p.cloudy)
dev.off()

svg("sunfleck-review-figs/cloudy-fig.svg", width = 7, height = 8)
print(p.cloudy)
dev.off()

