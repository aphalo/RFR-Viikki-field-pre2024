

dfS <- read.csv("sunfleck-review-figs/dfCloudfleck_figure.csv")

png("sunfleck-review-figs/cloudflecksv2.png", width = 7000, height = 5000, res = 900)
cols <- c("#6E4B6B", "#537197", "#42A1A5", "#CBC2BB", "#BE867E")
layout(mat = matrix(c(1,3,2,4),nrow = 2), widths = c(3,1), heights = c(1,3))
par(mar = c(0,0,0.5,0.5), oma = c(4,4.5,1,1))
plot(-500, axes = F, xlab = "", ylab = "", xlim = c(-0.05,1.05), ylim = c(0,3), bty = "L", yaxs = "i")
polygon(x = c(density(dfS$percDiff)$x, rev(density(dfS$percDiff)$x)), y = c(density(dfS$percDiff)$y, rep(0, times = length(density(dfS$percDiff)$y))), col = cols[3], border = "black", lwd = 2, density = 30, angle = 45)
polygon(x = c(density(dfS$percDiff)$x, rev(density(dfS$percDiff)$x)), y = c(density(dfS$percDiff)$y, rep(0, times = length(density(dfS$percDiff)$y))), col = cols[3], border = "black", lwd = 2, density = 30, angle = 135)
rug(jitter(dfS$percDiff), ticksize = -0.03, side  = 1)

plot(-500, ylim = c(0,1), xlim = c(0,1), xlab = "", ylab = "", xaxt = "n", yaxt = "n", axes  = F)

plot(-500, xlim = c(-0.05,1.05), ylim = c(-20,310), yaxs = "i", xaxs = "i", bty = "L", xaxt = "n", yaxt = "n")
points(duration ~ percDiff, dfS, cex = 1.2, col = "gray10", bg = cols[3], pch = 21)
axis(side = 1, cex.axis = 1.2, font = 2, at = seq(0,1,0.2), labels = seq(0,100,20))
axis(side = 2, cex.axis = 1.2, font = 2, las = 2)
axis(side = 2, labels = FALSE, tick = TRUE, tcl = -0.2, at = seq(0,500,10))
axis(side = 2, labels = FALSE, tick = TRUE, tcl = -0.5, at = seq(0,500,50))
mtext(text = "Cloud attenuation (%)", line = 2.5, side = 1, font = 4, cex = 1.4)
mtext(text = "Cloudfleck duration (min.)", line = 3, side = 2, font = 4, cex = 1.4)

plot(-500, axes = F, xlab = "", ylab = "", xlim = c(0,0.12), ylim = c(-20,310), bty = "L", xaxs = "i", yaxs = "i")
# polygon(x = c(density(dfS$duration)$y, rep(0, times = length(density(dfS$duration)$y))), y = c(density(dfS$duration)$x, rev(density(dfS$duration)$x)), col = cols[2], border = "black")
polygon(x = c(density(dfS$duration)$y, rep(0, times = length(density(dfS$duration)$y))), y = c(density(dfS$duration)$x, rev(density(dfS$duration)$x)), col = cols[3], border = "black", lwd = 2, density = 30, angle = 45)
polygon(x = c(density(dfS$duration)$y, rep(0, times = length(density(dfS$duration)$y))), y = c(density(dfS$duration)$x, rev(density(dfS$duration)$x)), col = cols[3], border = "black", lwd = 2, density = 30, angle = 135)
rug(jitter(dfS$duration), ticksize = -0.03, side  = 2)
dev.off()
