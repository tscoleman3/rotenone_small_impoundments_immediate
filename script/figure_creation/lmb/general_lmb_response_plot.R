
dat = output

yvar = "cpue_1"

datsub = dat[,c("year", "type", "times_treat", yvar)]

library(dplyr)

plot_dat = datsub %>% group_by(year, type, times_treat) %>%
  summarize(
    mn = mean(cpue_1, na.rm = T),
    lwr = quantile(cpue_1, 0.025, na.rm = T),
    upr = quantile(cpue_1, 0.975, na.rm = T)
    ) %>% ungroup

plot_dat$year = as.numeric(plot_dat$year)
plot_dat

off_ctrl = -0.05
off_trt1 = 0.05

ppi = 600
png("cpue_1.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.2)
plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(plot_dat[,c("lwr", "upr")]),
     xaxt = "n", las = 1, ylab = "Electrofishing CPUE of Age-1 LMB")

lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)

with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
with(plot_dat[c(4,7),], segments(I(year + off_trt1), lwr, I(year + off_trt1), upr, col = "red", lwd = 2))

axis(side = 1, at = 2017:2019, labels = 2017:2019)
legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
       col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n")
dev.off()

