library(ggplot2)
library(ggpmisc)
library(ggdensity)
library(quantreg)
library(ggspectra)
library(photobiologyWavebands)
library(photobiologyPlants)
library(patchwork)

photon_as_default()

# theme_set(theme_bw())
theme_set(theme_classic())

#####

open.df <- read.csv("sunfleck-review-figs/dfOpen.csv")[,-1]

names(open.df)[names(open.df) == "s.q.irrad"] <- "s.e.irrad"
summary(open.df)

ggplot(open.df,
       aes(w.length, s.e.irrad, colour = type)) +
  geom_line()

open.df <- subset(open.df, w.length > 295 & w.length < 850)
open.df$spct.idx <- paste(open.df$type, "open", sep = ".")
open.df$sp <- open.df$type
open.df$type <- "open"

open.df$sp.label <- gsub("\\.", " ", open.df$spct.idx)

open.spct <- as.source_spct(open.df,
                            idfactor = "spct.idx",
                            multiple.wl = length(unique(open.df$spct.idx)))

open.spct <- e2q(open.spct)

open_normalised.spct <- normalize(open.spct, norm = 550)

getNormalisation(open.spct)

autoplot(subset(open.spct, spct.idx == "crops.open"))

q_irrad(open.spct, w.band = PAR())

#####

windflecks.df <- read.csv("sunfleck-review-figs/dfWindflecks.csv")[,-1]

names(windflecks.df)[names(windflecks.df) == "s.q.irrad"] <- "s.e.irrad"
summary(windflecks.df)

windflecks.df <- subset(windflecks.df, w.length > 295 & w.length < 850)
windflecks.df$spct.idx <- with(windflecks.df, paste(sp, type, sep = "_"))
windflecks.df$sp.label <- paste("italic(\"", gsub("\\.", " ", windflecks.df$sp), "\")", sep = "")

windflecks.spct <- as.source_spct(windflecks.df,
                                  idfactor = "spct.idx",
                                  multiple.wl = length(unique(windflecks.df$spct.idx)))

windflecks_open.df <- rbind(open.df, windflecks.df)
windflecks_open.spct <-
  as.source_spct(windflecks_open.df,
                 idfactor = "spct.idx",
                 multiple.wl = length(unique(windflecks_open.df$spct.idx)))
windflecks_open.spct <- e2q(windflecks_open.spct)

q_irrad(windflecks_open.spct, w.band = PAR(), scale.factor = 1e6)

q_irrad(windflecks_open.spct,
        w.band = Plant_bands(), quantity = "total", scale.factor = 1e6)

q_irrad(fscale(windflecks_open.spct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "total")

q_irrad(fscale(windflecks_open.spct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "relative.pc")

q_irrad(fscale(windflecks_open.spct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "contribution.pc")

q_ratio(fscale(windflecks_open.spct, f = q_irrad, target = 100, w.band = PAR()),
        w.band.num = Plant_bands(), w.band.denom = PAR(), scale.factor = 100)

R_FR(windflecks_open.spct)


# ggplot(data = normalise(windflecks_open.spct, norm = 550),
#        aes(w.length, s.q.irrad)) +
#   geom_spct() +
#   facet_grid(cols = vars(type), rows = vars(sp))
#
# ggplot(data = normalise(windflecks_open.spct, norm = "max"),
#        aes(w.length, s.q.irrad, linetype = type)) +
#   geom_line() +
#   facet_wrap(facets = vars(sp), ncol = 1)
#
# ggplot(data = normalise(windflecks_open.spct, norm = 550),
#        aes(w.length, s.q.irrad, linetype = type)) +
#   geom_line() +
#   facet_wrap(facets = vars(sp), ncol = 1)
#
p <-
  ggplot(data = fscale(windflecks.spct, f = q_irrad, target = 100, w.band = PAR()),
       aes(w.length, s.q.irrad, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  geom_line() +
  facet_grid(rows = vars(sp.label), labeller = label_parsed)
p

p.birch.scaled <-
  ggplot(data = fscale(subset(windflecks_open.spct, sp == "Betula.pendula" | sp == "forest"),
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.045e-6, ymax = -0.01e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(b) scaled") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.birch.scaled

p.birch.irrad <-
  ggplot(data = subset(windflecks_open.spct, sp == "Betula.pendula" | sp == "forest"),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.011e-6, ymax = -0.003e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(a) original") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.birch.irrad

p.birch <- p.birch.irrad / p.birch.scaled +
  plot_layout(axis_titles = "collect", guides = "collect")
p.birch

p.birch.short.wl <-
  (p.birch.irrad  + coord_cartesian(xlim = c(295, 685))) /
  (p.birch.scaled + coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.5e-6))) +
  plot_layout(axis_titles = "collect", guides = "collect")
p.birch.short.wl

pdf("sunfleck-review-figs/sunflecks-fig.pdf", width = 7, height = 5)
print(p)
dev.off()

pdf("sunfleck-review-figs/sunflecks-birch-fig.pdf", width = 7, height = 8)
print(p.birch)
dev.off()

# pdf("sunfleck-review-figs/sunflecks-birch-short-wl-fig.pdf", width = 7, height = 8)
# print(p.birch.short.wl)
# dev.off()

####

p.spruce.scaled <-
  ggplot(data = fscale(subset(windflecks_open.spct, sp == "Picea.abies" | sp == "forest"),
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.045e-6, ymax = -0.01e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(b) scaled") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.spruce.scaled

p.spruce.irrad <-
  ggplot(data = subset(windflecks_open.spct, sp == "Picea.abies" | sp == "forest"),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.011e-6, ymax = -0.003e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(a) original") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.spruce.irrad

p.spruce <- p.spruce.irrad / p.spruce.scaled +
  plot_layout(axis_titles = "collect", guides = "collect")
p.spruce

pdf("sunfleck-review-figs/sunflecks-spruce-fig.pdf", width = 7, height = 8)
print(p.spruce)
dev.off()


###
p.faba.scaled <-
  ggplot(data = fscale(subset(windflecks_open.spct, sp == "Vicia.faba" | sp == "crops"),
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.022e-6, ymax = -0.005e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(b) scaled") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.faba.scaled

p.faba.irrad <-
  ggplot(data = subset(windflecks_open.spct, sp == "Vicia.faba" | sp == "crops"),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.02e-6, ymax = -0.003e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(a) original") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.faba.irrad

p.faba <- p.faba.irrad / p.faba.scaled +
  plot_layout(axis_titles = "collect", guides = "collect")
p.faba

p.faba.short.wl <-
  (p.faba.irrad  + coord_cartesian(xlim = c(295, 685))) /
  (p.faba.scaled + coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.5e-6))) +
  plot_layout(axis_titles = "collect", guides = "collect")
p.faba.short.wl

pdf("sunfleck-review-figs/sunflecks-faba-fig.pdf", width = 7, height = 8)
print(p.faba)
dev.off()

###
p.faba.scaled <-
  ggplot(data = fscale(subset(windflecks_open.spct, sp == "Vicia.faba" | sp == "crops"),
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.022e-6, ymax = -0.005e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(b) scaled") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.faba.scaled

p.faba.irrad <-
  ggplot(data = subset(windflecks_open.spct, sp == "Vicia.faba" | sp == "crops"),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.02e-6, ymax = -0.003e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(a) original") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.faba.irrad

p.faba <- p.faba.irrad / p.faba.scaled +
  plot_layout(axis_titles = "collect", guides = "collect")
p.faba

p.faba.short.wl <-
  (p.faba.irrad  + coord_cartesian(xlim = c(295, 685))) /
  (p.faba.scaled + coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.5e-6))) +
  plot_layout(axis_titles = "collect", guides = "collect")
p.faba.short.wl

pdf("sunfleck-review-figs/sunflecks-faba-fig.pdf", width = 7, height = 8)
print(p.faba)
dev.off()

###
p.faba.scaled <-
  ggplot(data = fscale(subset(windflecks_open.spct, sp == "Vicia.faba" | sp == "crops"),
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.022e-6, ymax = -0.005e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(b) scaled") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.faba.scaled

p.faba.irrad <-
  ggplot(data = subset(windflecks_open.spct, sp == "Vicia.faba" | sp == "crops"),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.02e-6, ymax = -0.003e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(a) original") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.faba.irrad

p.faba <- p.faba.irrad / p.faba.scaled +
  plot_layout(axis_titles = "collect", guides = "collect")
p.faba

p.faba.short.wl <-
  (p.faba.irrad  + coord_cartesian(xlim = c(295, 685))) /
  (p.faba.scaled + coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.5e-6))) +
  plot_layout(axis_titles = "collect", guides = "collect")
p.faba.short.wl

pdf("sunfleck-review-figs/sunflecks-faba-fig.pdf", width = 7, height = 8)
print(p.faba)
dev.off()

###
p.rapeseed.scaled <-
  ggplot(data = fscale(subset(windflecks_open.spct, sp == "Brassica.napus" | sp == "crops"),
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.022e-6, ymax = -0.005e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(b) scaled") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.rapeseed.scaled

p.rapeseed.irrad <-
  ggplot(data = subset(windflecks_open.spct, sp == "Brassica.napus" | sp == "crops"),
         aes(w.length, s.q.irrad * 1e-6, linetype = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.02e-6, ymax = -0.003e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  labs(tag = "(a) original") +
  theme(plot.tag.position = c(0.1, 0.95), plot.tag = element_text(hjust = 0, size = 10))
p.rapeseed.irrad

p.rapeseed <- p.rapeseed.irrad / p.rapeseed.scaled +
  plot_layout(axis_titles = "collect", guides = "collect")
p.rapeseed

p.rapeseed.short.wl <-
  (p.rapeseed.irrad  + coord_cartesian(xlim = c(295, 685))) /
  (p.rapeseed.scaled + coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.5e-6))) +
  plot_layout(axis_titles = "collect", guides = "collect")
p.rapeseed.short.wl

pdf("sunfleck-review-figs/sunflecks-rapeseed-fig.pdf", width = 7, height = 8)
print(p.rapeseed)
dev.off()

