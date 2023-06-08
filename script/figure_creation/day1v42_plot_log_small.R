# bass
dat = read.csv("data/lmb_seine.csv", stringsAsFactors = F)

datsub = dat[dat$period %in% c("before", "followup") & dat$event %in% c(1,3),]
unique(datsub$event)
unique(datsub$period)

df = aggregate(datsub['count'], list(lake = datsub$lake, period = datsub$period, 
                                     type = datsub$type, event = datsub$event, 
                                     year = datsub$year, size = datsub$size_class), sum)
df$count = log(df$count + 1)

df$period = as.factor(df$period); df$period = relevel(df$period, ref = "before")
sml_mean = with(subset(df, size == "small"), tapply(count, list(period, type), mean))
sml_lwr = with(subset(df, size == "small"), tapply(count, list(period, type), function(x) quantile(x, 0.025)))
sml_upr = with(subset(df, size == "small"), tapply(count, list(period, type), function(x) quantile(x, 0.975)))

#bluegill 
bluedat = read.csv("data/blue_seine.csv", stringsAsFactors = F)

bluedatsub = bluedat[bluedat$period %in% c("before", "followup") & bluedat$event %in% c(1,3),]
unique(bluedatsub$event)
unique(bluedatsub$period)

bluedf = aggregate(bluedatsub['count'], list(lake = bluedatsub$lake, period = bluedatsub$period, 
                                     type = bluedatsub$type, event = bluedatsub$event, 
                                     year = bluedatsub$year, size = bluedatsub$size_class), sum)
bluedf$count = log(bluedf$count)
bluedf$period = as.factor(bluedf$period); bluedf$period = relevel(bluedf$period, ref = "before")
blue_mean = with(subset(bluedf, size == "small"), tapply(count, list(period, type), mean))
blue_lwr = with(subset(bluedf, size == "small"), tapply(count, list(period, type), function(x) quantile(x, 0.1)))
blue_upr = with(subset(bluedf, size == "small"), tapply(count, list(period, type), function(x) quantile(x, 0.9)))


ppi = 600
x = c(0.85,2.15)
xc = x + 0.015
xt = x - 0.015
png("writing/figures/day1_42_log_small.png", h = 7 * ppi, w = 4 * ppi, res = ppi)
par(mfrow = c(2,1), mar = c(0.5,1,0.5,1), oma = c(1.5,3,0.5,0), 
    cex.axis = 1, cex.lab = 1.2, mgp = c(1,0.75,0))

# bass
plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), 
     ylim = range(sml_lwr, sml_upr, blue_lwr, blue_upr) * c(1, 1.05), las = 1, xaxt = "n")    
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(sml_mean[,"control"] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2)
points(sml_mean[,"treat"] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2)
arrows(xc, sml_lwr[,"control"], xc, sml_upr[,"control"], length = 0, lwd = 3, lty = 2)
arrows(xt, sml_lwr[,"treat"], xt, sml_upr[,"treat"], length = 0, lwd = 3)
axis(side = 1, at = x, labels = rep("", 2))
legend(usr[1], usr[4] + usr[3], legend = c("Control", "Treatment"), seg.len = c(2.75,2.75),
       bty = "n", lty = c(2,1), lwd = c(3,3))
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Largemouth Bass", font = 4)

# bluegill
plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), 
     ylim = range(sml_lwr, sml_upr, blue_lwr, blue_upr) * c(1, 1.05), las = 1, xaxt = "n")    
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(blue_mean[,"control"] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2)
points(blue_mean[,"treat"] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2)
arrows(xc, blue_lwr[,"control"], xc, blue_upr[,"control"], length = 0, lwd = 3, lty = 2)
arrows(xt, blue_lwr[,"treat"], xt, blue_upr[,"treat"], length = 0, lwd = 3)
axis(side = 1, at = x, labels = rep("", 2))
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Bluegill", font = 4)
mtext(side = 2, outer = T, "ln Total Catch Per Impoundment", line = 1, cex = 1.2)                                                       
mtext(side = 1, at = c(0.175,0.815), line = 0.25, outer = T, 
      text = c("Day 1", "Day 42"), xpd = T, cex = 1)

dev.off()