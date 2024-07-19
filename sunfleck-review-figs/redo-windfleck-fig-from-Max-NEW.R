library(ggplot2)
library(ggpmisc)
library(ggdensity)
library(quantreg)
library(ggspectra)
library(photobiologyWavebands)
library(photobiologyPlants)
library(patchwork)

photon_as_default()
options(ggspectra.add.symbols = FALSE)

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

open.mspct <- subset2mspct(open.spct)
names(open.mspct)
forest_open.mspct <- open.mspct["forest.open"]

autoplot(forest_open.mspct)

q_irrad(forest_open.mspct, w.band = PAR(), scale.factor = 1e6)

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

## Betula pendula

windflecks.mspct <- subset2mspct(windflecks.spct)
names(windflecks.mspct)
birch_windflecks.mspct <- windflecks.mspct[5:6]

birch_windflecks.mspct <- c(forest_open.mspct, birch_windflecks.mspct)

q_irrad(birch_windflecks.mspct, w.band = PAR(), scale.factor = 1e6)

q_irrad(birch_windflecks.mspct,
        w.band = Plant_bands(), quantity = "total", scale.factor = 1e6)

q_irrad(fscale(birch_windflecks.mspct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "total")

q_irrad(fscale(birch_windflecks.mspct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "relative.pc")

q_irrad(fscale(birch_windflecks.mspct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "contribution.pc")

q_ratio(fscale(birch_windflecks.mspct, f = q_irrad, target = 100, w.band = PAR()),
        w.band.num = Plant_bands(), w.band.denom = PAR(), scale.factor = 100)

R_FR(birch_windflecks.mspct)

### Picea abies

names(windflecks.mspct)
spruce_windflecks.mspct <- windflecks.mspct[7:8]

spruce_windflecks.mspct <- c(forest_open.mspct, spruce_windflecks.mspct)

q_irrad(spruce_windflecks.mspct, w.band = PAR(), scale.factor = 1e6)

q_irrad(spruce_windflecks.mspct,
        w.band = Plant_bands(), quantity = "total", scale.factor = 1e6)

q_irrad(fscale(spruce_windflecks.mspct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "total")

q_irrad(fscale(spruce_windflecks.mspct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "relative.pc")

q_irrad(fscale(spruce_windflecks.mspct, f = q_irrad, target = 100, w.band = PAR()),
        w.band = Plant_bands(), quantity = "contribution.pc")

q_ratio(fscale(spruce_windflecks.mspct, f = q_irrad, target = 100, w.band = PAR()),
        w.band.num = Plant_bands(), w.band.denom = PAR(), scale.factor = 100)

R_FR(spruce_windflecks.mspct)

### Betula plots

p.birch.scaled <-
  ggplot(data = fscale(birch_windflecks.mspct,
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, colour = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.055e-6, ymax = -0.01e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6, scaled = TRUE) +
  scale_x_wl_continuous() +
  scale_colour_manual(values = c(open = "steelblue3", shade = "darkgreen", sunfleck = "yellowgreen")) +
  labs(tag = "B") +
  theme(plot.tag.position = c(0.2, 0.95), plot.tag = element_text(hjust = 0, size = 10),
        legend.position = "none")
p.birch.scaled

p.birch.irrad <-
  ggplot(data = e2q(birch_windflecks.mspct),
         aes(w.length, s.q.irrad, colour = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.2e-6, ymax = -0.03e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  scale_colour_manual(values = c(open = "steelblue3", shade = "darkgreen", sunfleck = "yellowgreen")) +
  labs(tag = "A") +
  theme(plot.tag.position = c(0.2, 0.95), plot.tag = element_text(hjust = 0, size = 10),
        legend.position = "none")
p.birch.irrad

p.birch <- p.birch.irrad / p.birch.scaled +
  plot_layout(axis_titles = "collect", guides = "collect")
p.birch

p.birch.short.wl <-
  (p.birch.irrad  + coord_cartesian(xlim = c(295, 685))) /
  (p.birch.scaled + coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.5e-6))) +
  plot_layout(axis_titles = "collect")
p.birch.short.wl

pdf("sunfleck-review-figs/sunflecks-birch-fig.pdf", width = 7, height = 8)
print(p.birch)
dev.off()


p.birch.scaled.zoom <-
  ggplot(data = fscale(birch_windflecks.mspct,
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, colour = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.015e-6, ymax = -0.003e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6, scaled = TRUE) +
  scale_x_wl_continuous() +
  scale_colour_manual(values = c(open = "steelblue3", shade = "darkgreen", sunfleck = "yellowgreen")) +
  #  labs(tag = "(b) scaled") +
  theme(plot.tag.position = c(0.2, 0.95), plot.tag = element_text(hjust = 0, size = 10)) +
  coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.4e-6))
p.birch.scaled.zoom

pdf("sunfleck-review-figs/sunflecks-birch-detail-fig.pdf", width = 7, height = 5)
print(p.birch.scaled.zoom)
dev.off()


### Picea abies plots

p.spruce.scaled <-
  ggplot(data = fscale(spruce_windflecks.mspct,
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, colour = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.055e-6, ymax = -0.01e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6, scaled = TRUE) +
  scale_x_wl_continuous() +
  scale_colour_manual(values = c(open = "steelblue3", shade = "darkgreen", sunfleck = "yellowgreen")) +
  labs(tag = "D") +
  theme(plot.tag.position = c(0.2, 0.95), plot.tag = element_text(hjust = 0, size = 10),
        legend.position = "none")
p.spruce.scaled

p.spruce.irrad <-
  ggplot(data = e2q(spruce_windflecks.mspct),
         aes(w.length, s.q.irrad, colour = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.2e-6, ymax = -0.03e-6) +
  geom_line() +
  scale_fill_identity() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6) +
  scale_x_wl_continuous() +
  scale_colour_manual(values = c(open = "steelblue3", shade = "darkgreen", sunfleck = "yellowgreen")) +
  labs(tag = "C") +
  theme(plot.tag.position = c(0.2, 0.95), plot.tag = element_text(hjust = 0, size = 10),
        legend.position = "none")
p.spruce.irrad

p.spruce <- p.spruce.irrad / p.spruce.scaled +
  plot_layout(axis_titles = "collect", guides = "collect")
p.spruce

p.spruce.short.wl <-
  (p.spruce.irrad  + coord_cartesian(xlim = c(295, 685))) /
  (p.spruce.scaled + coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.5e-6))) +
  plot_layout(axis_titles = "collect", guides = "collect")
p.spruce.short.wl

pdf("sunfleck-review-figs/sunflecks-spruce-fig.pdf", width = 7, height = 8)
print(p.spruce)
dev.off()

p.spruce.scaled.zoom <-
  ggplot(data = fscale(spruce_windflecks.mspct,
                       f = q_irrad , target = 100, w.band = PAR()),
         aes(w.length, s.q.irrad * 1e-6, colour = type)) +
  geom_vline(xintercept = c(400, 700), linetype = "dotted") +
  stat_wl_strip(ymin = -0.015e-6, ymax = -0.003e-6) +
  scale_fill_identity() +
  geom_line() +
  scale_y_s.q.irrad_continuous(unit.exponent = -6, scaled = TRUE) +
  scale_x_wl_continuous() +
  scale_colour_manual(values = c(open = "steelblue3", shade = "darkgreen", sunfleck = "yellowgreen")) +
  #  labs(tag = "(b) scaled") +
  theme(plot.tag.position = c(0.2, 0.95), plot.tag = element_text(hjust = 0, size = 10)) +
  coord_cartesian(xlim = c(295, 685), ylim = c(0, 0.4e-6))
p.spruce.scaled.zoom

pdf("sunfleck-review-figs/sunflecks-spruce-detail-fig.pdf", width = 7, height = 5)
print(p.spruce.scaled.zoom)
dev.off()

####


p.birch.spruce <- ((p.birch.irrad / (p.birch.scaled + expand_limits(y = 1.8e-6)))  +
                     plot_layout(axis_titles = "collect") |
  (p.spruce.irrad / (p.spruce.scaled)  + expand_limits(y = 1.8e-6)) +
    plot_layout(axis_titles = "collect")) +
  plot_layout(axis_titles = "collect")
p.birch.spruce

pdf("sunfleck-review-figs/sunflecks-birch-spruce-fig.pdf", width = 7, height = 8)
print(p.birch.spruce)
dev.off()

#### photograph insets

birch.img <- magick::image_read("sunfleck-review-figs/_2021.08.30_Birch_MP5_IMG_2149_edited.jpg")

p.birch.scaled.img <-
  p.birch.scaled +
  expand_limits(y = 1.8e-6) +
  annotate(geom = "grob_npc",
                            label = grid::rasterGrob(birch.img),
                            npcx =  0.3,
                            npcy =  0.9,
                            vp.width = 0.5,
                            vp.height = 0.5,
                            hjust = 0.5)
# p.birch.scaled.img

spruce.img <- magick::image_read("sunfleck-review-figs/_2021.08.30_Spruce_MP2_IMG_2112_edited.jpg")

p.spruce.scaled.img <-
  p.spruce.scaled +
  expand_limits(y = 1.8e-6) +
  annotate(geom = "grob_npc",
                             label = grid::rasterGrob(spruce.img),
                             npcx =  0.3,
                             npcy =  0.9,
                             vp.width = 0.5,
                             vp.height = 0.5,
                             hjust = 0.5)
# p.spruce.scaled.img

p.birch.spruce.img <- ((p.birch.irrad / p.birch.scaled.img)  +
                     plot_layout(axis_titles = "collect") |
                     (p.spruce.irrad / p.spruce.scaled.img) +
                     plot_layout(axis_titles = "collect")) +
  plot_layout(axis_titles = "collect")
# p.birch.spruce.img

pdf("sunfleck-review-figs/sunflecks-birch-spruce-img-fig.pdf", width = 7, height = 8)
print(p.birch.spruce.img)
dev.off()

p.birch.spruce.img2 <- ((p.birch.irrad / p.spruce.irrad)  +
                         plot_layout(axis_titles = "collect") |
                         (p.birch.scaled.img / p.spruce.scaled.img) +
                         plot_layout(axis_titles = "collect")) +
  plot_layout(axis_titles = "collect")
# p.birch.spruce.img2

pdf("sunfleck-review-figs/sunflecks-birch-spruce-img2-fig.pdf", width = 7, height = 8)
print(p.birch.spruce.img2)
dev.off()

