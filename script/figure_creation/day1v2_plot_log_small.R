# bass
dat = read.csv("data/lmb_seine.csv", stringsAsFactors = F)

datsub = dat[dat$period %in% c("before", "after") & dat$event %in% c(1,2),]
unique(datsub$event)
unique(datsub$period)

df = aggregate(datsub['count'], list(lake = datsub$lake, period = datsub$period, 
                                     type = datsub$type, event = datsub$event, 
                                     year = datsub$year, size = datsub$size_class,
                                     tt = datsub$times_treat), sum)
df$count = log(df$count + 1)
df$period = as.factor(df$period); df$period = relevel(df$period, ref = "before")
sml_mean = with(subset(df, size == "small"), 
                tapply(count, list(event, period, type), mean))
sml_lwr = with(subset(df, size == "small"), 
               tapply(count, list(event, period, type), function(x) quantile(x, 0.025)))
sml_upr = with(subset(df, size == "small"), 
               tapply(count, list(event, period, type), function(x) quantile(x, 0.975)))

# bluegill
bluedat = read.csv("data/blue_seine.csv", stringsAsFactors = F)

bluedatsub = bluedat[bluedat$period %in% c("before", "after") & bluedat$event %in% c(1,2),]
unique(bluedatsub$event)
unique(bluedatsub$period)

bluedf = aggregate(bluedatsub['count'], list(lake = bluedatsub$lake, period = bluedatsub$period, 
                                             type = bluedatsub$type, event = bluedatsub$event, 
                                             year = bluedatsub$year, size = bluedatsub$size_class,
                                             tt = bluedatsub$times_treat), sum)
bluedf$count = log(bluedf$count + 1)
bluedf$period = as.factor(bluedf$period); bluedf$period = relevel(bluedf$period, ref = "before")
blue_mean = with(subset(bluedf, size == "small"), tapply(count, list(event, period, type), mean))
blue_lwr = with(subset(bluedf, size == "small"), tapply(count, list(event, period, type), function(x) quantile(x, 0.025)))
blue_upr = with(subset(bluedf, size == "small"), tapply(count, list(event, period, type), function(x) quantile(x, 0.975)))


ppi = 600
x = c(0.9,2.1)
xc = x + 0.015
xt = x - 0.015
xc2 = x + 0.045
xt2 = x - 0.045
png("writing/figures/day1_day2_log_small.png", h = 7 * ppi, w = 4 * ppi, res = ppi)
par(mfrow = c(2,1), mar = c(0.5,1,0.5,1), oma = c(2.5,3,0.5,0),
    cex.axis = 1, cex.lab = 1.2, mgp = c(1,0.75,0))

# bass
plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), 
     ylim = range(sml_lwr, sml_upr, blue_lwr, blue_upr) * c(1,1.15),las = 1, xaxt = "n")   
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(sml_mean[1,,"control"] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2)
points(sml_mean[1,,"treat"] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2)
points(sml_mean[2,,"control"] ~ xc2, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2, col = "grey75")
points(sml_mean[2,,"treat"] ~ xt2, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "grey75")
arrows(xc, sml_lwr[1,,"control"], xc, sml_upr[1,,"control"], length = 0, lwd = 3, lty = 2)
arrows(xt, sml_lwr[1,,"treat"], xt, sml_upr[1,,"treat"], length = 0, lwd = 3)
arrows(xc2, sml_lwr[2,,"control"], xc2, sml_upr[2,,"control"], length = 0, lwd = 3, lty = 2, col = "grey75")
arrows(xt2, sml_lwr[2,,"treat"], xt2, sml_upr[2,,"treat"], length = 0, lwd = 3, col = "grey75")
axis(side = 1, at = x, labels = rep("", 2))
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Largemouth Bass", font = 4)
legend(usr[1], usr[4] + usr[3] - 0.25, legend = c("Control", "Treatment", "App. 1", "App. 2"), 
       ncol = 2, pch = c(NA, NA, 15, 15), seg.len = c(2.75,2.75,1,1),
       col = c(rep("black", 3), "grey75"), bty = "n", pt.cex = 1.5, 
       lty = c(2,1,NA,NA), lwd = c(3,3,NA,NA), x.intersp = c(1,1,0.25,0.25))

mtext(side = 2, outer = T, "ln Total Catch Per Impoundment", line = 1, cex = 1.2)                                                       
mtext(side = 1, at = c(0.175,0.815), line = 1.25, outer = T,
      text = c("Day 1 & 21\nPre-Treatment", "Day 2 & 22\nPost-Treatment"), xpd = T, cex = 1)

# bluegill
plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), 
     ylim = range(sml_lwr, sml_upr, blue_lwr, blue_upr) * c(1,1.15),las = 1, xaxt = "n")   
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(blue_mean[1,,"control"] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2)
points(blue_mean[1,,"treat"] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2)
points(blue_mean[2,,"control"] ~ xc2, type = "l", pch = 16, lwd = 3, cex = 1.2, lty = 2, col = "grey75")
points(blue_mean[2,,"treat"] ~ xt2, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "grey75")
arrows(xc, blue_lwr[1,,"control"], xc, blue_upr[1,,"control"], length = 0, lwd = 3, lty = 2)
arrows(xt, blue_lwr[1,,"treat"], xt, blue_upr[1,,"treat"], length = 0, lwd = 3)
arrows(xc2, blue_lwr[2,,"control"], xc2, blue_upr[2,,"control"], length = 0, lwd = 3, lty = 2, col = "grey75")
arrows(xt2, blue_lwr[2,,"treat"], xt2, blue_upr[2,,"treat"], length = 0, lwd = 3, col = "grey75")
axis(side = 1, at = x, labels = rep("", 2))
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Bluegill", font = 4)

dev.off()





