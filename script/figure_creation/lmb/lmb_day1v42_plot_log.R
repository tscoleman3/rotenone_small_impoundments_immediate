
dat = read.csv("data/lmb_seine.csv", stringsAsFactors = F)

datsub = dat[dat$period %in% c("before", "followup") & dat$event %in% c(1,3),]
unique(datsub$event)
unique(datsub$period)

df = aggregate(datsub['count'], list(lake = datsub$lake, period = datsub$period, 
                                  type = datsub$type, event = datsub$event, 
                                  year = datsub$year, size = datsub$size_class), sum)
df$count = log(df$count + 1)
# df = df[df$year == "2018",]
# df = df[df$year == "2017",]

df$period = as.factor(df$period); df$period = relevel(df$period, ref = "before")
sml_mean = with(subset(df, size == "small"), tapply(count, list(period, type), mean))
sml_lwr = with(subset(df, size == "small"), tapply(count, list(period, type), function(x) quantile(x, 0.025)))
sml_upr = with(subset(df, size == "small"), tapply(count, list(period, type), function(x) quantile(x, 0.975)))
big_mean = with(subset(df, size == "big"), tapply(count, list(period, type), mean))
big_lwr = with(subset(df, size == "big"), tapply(count, list(period, type), function(x) quantile(x, 0.025)))
big_upr = with(subset(df, size == "big"), tapply(count, list(period, type), function(x) quantile(x, 0.975)))

ppi = 600
x = c(0.85,2.15)
xc = x + 0.015
xt = x - 0.015
png("lmb_day1_42_log.png", h = 7 * ppi, w = 4 * ppi, res = ppi)
par(mfrow = c(2,1), mar = c(0.5,1,0.5,1), oma = c(1.5,3,0.5,0), cex.axis = 1, cex.lab = 1.2, mgp = c(1,0.75,0))
plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), ylim = range(sml_lwr, sml_upr) * c(1,1.05), las = 1, xaxt = "n")    
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(sml_mean[,"control"] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2)
points(sml_mean[,"treat"] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2)
arrows(xc, sml_lwr[,"control"], xc, sml_upr[,"control"], length = 0, lwd = 3, lty = 2)
arrows(xt, sml_lwr[,"treat"], xt, sml_upr[,"treat"], length = 0, lwd = 3)
axis(side = 1, at = x, labels = rep("", 2))
legend("top", legend = c("Control", "Treatment"), seg.len = c(2.75,2.75),
       bty = "n", lty = c(2,1), lwd = c(3,3))
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Small", font = 4)

plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), ylim = range(sml_lwr, sml_upr) * c(1,1.05), las = 1, xaxt = "n")     
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(big_mean[,"control"] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2)
points(big_mean[,"treat"] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2)
arrows(xc, big_lwr[,"control"], xc, big_upr[,"control"], length = 0, lwd = 3, lty = 2)
arrows(xt, big_lwr[,"treat"], xt, big_upr[,"treat"], length = 0, lwd = 3)
axis(side = 1, at = x, labels = rep("", 2))
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Large", font = 4)
mtext(side = 2, outer = T, "ln Total LMB Catch Per Impoundment", line = 1, cex = 1.2)                                                       
mtext(side = 1, at = c(0.15,0.85), line = 0.25, outer = T, text = c("Day 1", "Day 42"), xpd = T, cex = 1)
dev.off()

