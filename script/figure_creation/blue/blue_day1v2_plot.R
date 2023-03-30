

dat = read.csv("blue_seine.csv", stringsAsFactors = F)

datsub = dat[dat$period %in% c("before", "after") & dat$event %in% c(1,2),]
unique(datsub$event)
unique(datsub$period)

df = aggregate(datsub['count'], list(lake = datsub$lake, period = datsub$period, 
                                     type = datsub$type, event = datsub$event, 
                                     year = datsub$year, size = datsub$size_class,
                                     tt = datsub$times_treat), sum)
df$period = as.factor(df$period); df$period = relevel(df$period, ref = "before")
sml_mean = with(subset(df, size == "small"), tapply(count, list(event, period, type), mean))
sml_lwr = with(subset(df, size == "small"), tapply(count, list(event, period, type), function(x) quantile(x, 0.1)))
sml_upr = with(subset(df, size == "small"), tapply(count, list(event, period, type), function(x) quantile(x, 0.9)))
big_mean = with(subset(df, size == "big"), tapply(count, list(event, period, type), mean))
big_lwr = with(subset(df, size == "big"), tapply(count, list(event, period, type), function(x) quantile(x, 0.1)))
big_upr = with(subset(df, size == "big"), tapply(count, list(event, period, type), function(x) quantile(x, 0.9)))

ppi = 600
x = c(0.9,2.1)
xc = x + 0.03
xt = x - 0.03
xc2 = x + 0.06
xt2 = x - 0.06
png("blue_day1_day2.png", h = 7 * ppi, w = 4 * ppi, res = ppi)
par(mfrow = c(2,1), mar = c(0.5,1,0.5,1), oma = c(2.5,3,0.5,0), cex.axis = 1, cex.lab = 1.2, mgp = c(1,0.75,0))
plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), ylim = range(sml_lwr, sml_upr) * c(1,1.15),las = 1, xaxt = "n")     # change ylim for log()
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(sml_mean[1,,"control"] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2)
points(sml_mean[1,,"treat"] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2)
points(sml_mean[2,,"control"] ~ xc2, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2, col = "grey50")
points(sml_mean[2,,"treat"] ~ xt2, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "grey50")
arrows(xc, sml_lwr[1,,"control"], xc, sml_upr[1,,"control"], length = 0, lwd = 3, lty = 2)
arrows(xt, sml_lwr[1,,"treat"], xt, sml_upr[1,,"treat"], length = 0, lwd = 3)
arrows(xc2, sml_lwr[2,,"control"], xc2, sml_upr[2,,"control"], length = 0, lwd = 3, lty = 2, col = "grey50")
arrows(xt2, sml_lwr[2,,"treat"], xt2, sml_upr[2,,"treat"], length = 0, lwd = 3, col = "grey50")
axis(side = 1, at = x, labels = rep("", 2))
legend("top", legend = c("Control", "Treatment", "App. 1", "App. 2"), ncol = 2, pch = c(NA, NA, 15, 15), seg.len = c(2.75,2.75,1,1),
       col = c(rep("black", 3), "grey50"), bty = "n", pt.cex = 1.5, lty = c(2,1,NA,NA), lwd = c(3,3,NA,NA), x.intersp = c(1,1,0.25,0.25))
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Small", font = 4)

plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), ylim = range(big_lwr, big_upr) * c(1,1.15),las = 1, xaxt = "n")     # change ylim for log()
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(big_mean[1,,"control"] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2)
points(big_mean[1,,"treat"] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2)
points(big_mean[2,,"control"] ~ xc2, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2, col = "grey50")
points(big_mean[2,,"treat"] ~ xt2, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "grey50")
arrows(xc, big_lwr[1,,"control"], xc, big_upr[1,,"control"], length = 0, lwd = 3, lty = 2)
arrows(xt, big_lwr[1,,"treat"], xt, big_upr[1,,"treat"], length = 0, lwd = 3)
arrows(xc2, big_lwr[2,,"control"], xc2, big_upr[2,,"control"], length = 0, lwd = 3, lty = 2, col = "grey50")
arrows(xt2, big_lwr[2,,"treat"], xt2, big_upr[2,,"treat"], length = 0, lwd = 3, col = "grey50")
axis(side = 1, at = x, labels = rep("", 2))
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Large", font = 4)
mtext(side = 2, outer = T, "Total Bluegill Catch Per Impoundment", line = 1.75, cex = 1.2)                                                   
mtext(side = 1, at = c(0.175,0.815), line = 1.25, outer = T, text = c("Day 1 & 21\nPre-Treatment", "Day 2 & 22\nPost-Treatment"), xpd = T, cex = 1)
dev.off()

