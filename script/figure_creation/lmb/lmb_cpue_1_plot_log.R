
dat = read.csv("lmb_output.csv")
dat$new = ifelse(dat$lake %in% c("drummond3", "lm", "promise", "mark"), 1, 0)
dat$drop = ifelse(dat$lake %in% c("lee county lake", "barbour county lake", "washington county lake"), 1, 0)

yvar = "cpue_1"
dat$cpue_1 = log(dat$cpue_1 + 1)

datsub = dat[,c("year", "type", "times_treat", "size_class", "new", "drop", yvar)]

library(dplyr)

plot_dat_sml = datsub %>% filter(size_class == "small") %>%
  group_by(year, type, new, times_treat) %>%
  summarize(
    mn = mean(cpue_1, na.rm = T),
    lwr = quantile(cpue_1, 0.025, na.rm = T),
    upr = quantile(cpue_1, 0.975, na.rm = T)
    ) %>% ungroup
plot_dat_big = datsub %>% filter(size_class == "big") %>%
  group_by(year, type, drop, times_treat) %>%
  summarize(
    mn = mean(cpue_1, na.rm = T),
    lwr = quantile(cpue_1, 0.025, na.rm = T),
    upr = quantile(cpue_1, 0.975, na.rm = T)
    ) %>% ungroup

plot_dat_sml$year = as.numeric(plot_dat_sml$year)
plot_dat_big$year = as.numeric(plot_dat_big$year)

off_c1 = -0.0975
off_c2 = -0.0325
off_t2 = 0.0325
off_t1 = 0.0975
ppi = 600
png("lmb_cpue_1_log.png", h = 7 * ppi, w = 4 * ppi, res = ppi)

# small lakes
par(mfrow = c(2,1), mar = c(0.5,1,0.5,1), oma = c(1.5,3,0.5,0), cex.axis = 1, cex.lab = 1.2, mgp = c(1,0.75,0))
plot(1,1, type = "n", xlim = c(2017,2019) + range(c(off_c1, off_c2, off_t1, off_t2)), ylim = c(0,4.6) * c(1, 1.15),
     xaxt = "n", las = 1, ylab = "")
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Small", font = 4)

# controls (not new entries)
ind = c(1,3,7)
lines(mn ~ I(year + off_c1), data = plot_dat_sml[ind,], lwd = 2, lty = 2, type = "l")
with(plot_dat_sml[ind,], segments(I(year + off_c1), lwr, I(year + off_c1), upr, lty = 2, lwd = 2))
points(mn ~ I(year + off_c1), data = plot_dat_sml[ind,], pch = 21, bg = "white", cex = 1.2)

# treats (not new entries)
ind = c(2,6,10)
lines(mn ~ I(year + off_t1), data = plot_dat_sml[ind,], lwd = 2, type = "l")
with(plot_dat_sml[ind,], segments(I(year + off_t1), lwr, I(year + off_t1), upr, lwd = 2))
points(mn ~ I(year + off_t1), data = plot_dat_sml[ind,], pch = 21, bg = c("white", "black", "black"), cex = 1.2)

# controls (new entry 2018)
ind = c(4,8)
lines(mn ~ I(year + off_c2), data = plot_dat_sml[ind,], lwd = 2, lty = 2, type = "l")
with(plot_dat_sml[ind,], segments(I(year + off_c2), lwr, I(year + off_c2), upr, lty = 2, lwd = 2))
points(mn ~ I(year + off_c2), data = plot_dat_sml[ind,], pch = 21, bg = "white", cex = 1.2)

# treats (new entry 2018)
ind = c(5,9)
lines(mn ~ I(year + off_t2), data = plot_dat_sml[ind,], lwd = 2, type = "l")
with(plot_dat_sml[ind,], segments(I(year + off_t2), lwr, I(year + off_t2), upr, lwd = 2))
points(mn ~ I(year + off_t2), data = plot_dat_sml[ind,], pch = 21, bg = c("white", "black"), cex = 1.2)

axis(side = 1, at = 2017:2019, labels = rep("", 3))
legend("topleft", legend = c("Control", "Treatment", "Untreated", "Treated"), ncol = 2,
       pch = c(NA, NA, 21, 21), pt.cex = c(NA, NA, 1.2, 1.2), pt.bg = c(NA, NA, "white", "black"),
       bty = "n", lty = c(2,1,NA,NA), lwd = c(2,2,NA,NA), x.intersp = c(0.5, 0.5, 0.25, 0.25))

# big lakes
plot_dat_big
plot(1,1, type = "n", xlim = c(2017,2019) + range(c(off_c1, off_c2, off_t1, off_t2)), ylim = c(2,5.1) * c(1,1.15),
     xaxt = "n", las = 1, ylab = "")
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
text(x = usr[2] - xdiff * 0, y = usr[4] - ydiff * 0.05, pos = 2, "Large", font = 4)
# controls (not new entries)
ind = c(1,5,9)
lines(mn ~ I(year + off_c1), data = plot_dat_big[ind,], lwd = 2, lty = 2, type = "l")
with(plot_dat_big[ind,], segments(I(year + off_c1), lwr, I(year + off_c1), upr, lty = 2, lwd = 2))
points(mn ~ I(year + off_c1), data = plot_dat_big[ind,], pch = 21, bg = "white", cex = 1.2)

# treats (not new entries)
ind = c(3,7,10)
lines(mn ~ I(year + off_t1), data = plot_dat_big[ind,], lwd = 2, type = "l")
with(plot_dat_big[ind,], segments(I(year + off_t1), lwr, I(year + off_t1), upr, lwd = 2))
points(mn ~ I(year + off_t1), data = plot_dat_big[ind,], pch = 21, bg = c("white", "black", "black"), cex = 1.2)

# controls (new entry 2018)
ind = c(2,6)
lines(mn ~ I(year + off_c2), data = plot_dat_big[ind,], lwd = 2, lty = 2, type = "l")
with(plot_dat_big[ind,], segments(I(year + off_c2), lwr, I(year + off_c2), upr, lty = 2, lwd = 2))
points(mn ~ I(year + off_c2), data = plot_dat_big[ind,], pch = 21, bg = "white", cex = 1.2)

# treats (new entry 2018)
ind = c(4,8)
lines(mn ~ I(year + off_t2), data = plot_dat_big[ind,], lwd = 2, type = "l")
with(plot_dat_big[ind,], segments(I(year + off_t2), lwr, I(year + off_t2), upr, lwd = 2))
points(mn ~ I(year + off_t2), data = plot_dat_big[ind,], pch = 21, bg = c("white", "black"), cex = 1.2)

axis(side = 1, at = 2017:2019, labels = 2017:2019)
mtext(side = 2, outer = T, "ln Age-1 LMB Electrofishing CPUE", line = 1.75, cex = 1.2) 
dev.off()


