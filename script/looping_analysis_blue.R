### BLUEGILL ###

rm(list = ls(all = T))
library(lme4)              # glmer 
library(nlme)              # lme
library(lmerTest)          # lmer pvalues

# setwd("~/Documents/git/rotenone")

blue_dat = read.csv("data/3.27.19data.csv")
names(blue_dat)[1] = "lake"
### keep only bluegill
blue_dat = blue_dat[blue_dat$species == "blue",]
### keep only efishing
blue_dat = blue_dat[blue_dat$gear == "efishing",]
### remove empty cells
blue_dat = droplevels(blue_dat)
lakenames = as.character(unique(blue_dat$lake))
years = unique(blue_dat$year)
# L = 1
# Y = 1

# highlight and run all code in funcs.R (redneck package)
source("code/funcs_blue.R")
# highlight and run all code for AICc table
source("code/AICc_table_func.R")

##### LOOP EVERYTHING ------------------------------------------------------------------------------
output = NULL
for (L in 1:length(lakenames)) {
  for (Y in 1:length(years)) {
    cat("\nLake:", lakenames[L], "; Year:", years[Y])
    
    datsub_ef = subset(blue_dat, lake == lakenames[L] & year == years[Y] & gear == "efishing")
    if (nrow(datsub_ef) == 0) {
      cat("skip ef\n")
      next
    } else {
      
      cpue = get_cpue(datsub_ef, min_length = 80)
      wr = get_wr(datsub_ef, min_length = 80, std_a = -5.374, std_b = 3.316)
      psd_q = get_psd(datsub_ef, stock_length = 80, other_length = 150)   # this is PSD
      psd_p = get_psd(datsub_ef, stock_length = 80, other_length = 200)
      psd_m = get_psd(datsub_ef, stock_length = 80, other_length = 250)
      psd_t = get_psd(datsub_ef, stock_length = 80, other_length = 300)
      
      tmp = data.frame(lake = lakenames[L], year = years[Y],
                        cpue, wr, psd_q, psd_p, psd_m, psd_t)
    }

      output = rbind(output, tmp)
      
    }
}

# combine lake id data with fish data output
output = output[order(output$lake, output$year),]
ids = read.csv("./data/lake_year_ids.csv", stringsAsFactors = F)
output = merge(ids, output, by = c("lake", "year"))
output = output[output$year != 2016,]                      # take out 2016 data
output$ba = relevel(factor(output$ba), ref = "before")     # make "before" ref group
output$year = as.character(output$year)
output$times_treat = as.factor(output$times_treat)
output$size_class = ifelse(output$size >30, "big", "small")   # make size categorical 
output$size_class = relevel(factor(output$size_class), ref = "small")   # this might not work?
lakenames[!(lakenames %in% ids$lake)]    # show which lake names are not found in the id data set
# write.csv(output, "blue_output.csv", row.names = FALSE)        # write dataset into a .csv to send



##### BACI analysis: cpue > 80mm ----------------------------------------------------------------------------------
hist(output$cpue)
hist(log(output$cpue))     # normal
# look at dist of hist(resid(fit))

## fit data in a model
output_cpue = output    # create new dataset to manipulate
min(output_cpue$cpue)   # no 0's

## fit data in a model
fit_cpue = lmer(log(cpue) ~ times_treat + size_class + times_treat:size_class + (1 | lake) + (1 | year), data = output_cpue, REML = F)
fit_cpue = lmer(log(cpue) ~ times_treat + size_class + (1 | lake) + (1 | year), data = output_cpue, REML = F)
fit_cpue = lmer(log(cpue) ~ times_treat + (1 | lake) + (1 | year), data = output_cpue, REML = F)
fit_cpue = lmer(log(cpue) ~ relevel(times_treat, ref = "1") + (1 | lake) + (1 | year), data = output_cpue, REML = F)
fit_cpue = lmer(log(cpue) ~ relevel(times_treat, ref = "2") + (1 | lake) + (1 | year), data = output_cpue, REML = F)
fit_cpue = lmer(log(cpue) ~ relevel(times_treat, ref = "1") + relevel(size_class, ref = "big") + (1 | lake) + (1 | year), data = output_cpue, REML = F)

# AICc table of models
# aic_cpue = aic_table(AICc(lmer_cpue_full, lmer_cpue_no_int, lmer_cpue_no_size, lmer_cpue_null))    # model w/o interaciton is best           

summary(fit_cpue)
anova(fit_cpue)
coefs = summary(fit_cpue)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

# ## plot ##
# dat = output
# 
# yvar = "cpue"
# 
# datsub = dat[,c("year", "type", "times_treat", yvar)]
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat) %>%
#   summarize(
#     mn = mean(cpue, na.rm = T),
#     lwr = quantile(cpue, 0.025, na.rm = T),
#     upr = quantile(cpue, 0.975, na.rm = T)
#   ) %>% ungroup
# 
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
# 
# off_ctrl = -0.05
# off_trt1 = 0.05
# 
# ppi = 600
# png("blue_cpue.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.2)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "Electrofishing CPUE of Bluegill (>80mm)")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
# 
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1), lwr, I(year + off_trt1), upr, col = "red", lwd = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.15)
# dev.off()










##### BACI analysis: Relative Weight (Wr) -------------------------------------------------------------------------
hist(log(output$wr))
hist(output$wr)
# check hist(resid(fit)) also
min(output$wr)

## fit data in a model ##
fit_wr = lmer(log(wr) ~ times_treat + size_class + times_treat:size_class + (1 | lake) + (1 | year), data = output, REML = F)
fit_wr = lmer(log(wr) ~ times_treat + size_class + (1 | lake) + (1 | year), data = output, REML = F)
fit_wr = lmer(log(wr) ~ times_treat + (1 | lake) + (1 | year), data = output, REML = F)
fit_wr = lmer(log(wr) ~ relevel(times_treat, ref = "2") + size_class + (1 | lake) + (1 | year), data = output, REML = F)
fit_wr = lmer(log(wr) ~ times_treat + relevel(size_class, ref = "big") + (1 | lake) + (1 | year), data = output, REML = F)
fit_wr = lmer(log(wr) ~ relevel(times_treat, ref = "1") + relevel(size_class, ref = "big") + (1 | lake) + (1 | year), data = output, REML = F)

# lmer_wr_full = lmer(wr ~ times_treat + size_class + size_class:times_treat + (1 | lake) + (1 | year), data = output, REML = F)
# lmer_wr_no_int = lmer(wr ~ times_treat + size_class + (1 | lake) + (1 | year), data = output, REML = F)
# lmer_wr_no_size = lmer(wr ~ times_treat + (1 | lake) + (1 | year), data = output, REML = F)
# lmer_wr_null = lmer(wr ~ 1 + (1 | lake) + (1 | year), data = output, REML = F)
# # aicc table
# aic_wr = aic_table(AICc(lmer_wr_full, lmer_wr_no_int, lmer_wr_no_size, lmer_wr_null))       # model w/o interaction is best

# hist(resid(fit_wr))      # test for normal dist

summary(fit_wr)
anova(fit_wr)
coefs = summary(fit_wr)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

# ## plot ##
# dat = output
# 
# yvar = "wr"
# 
# datsub = dat[,c("year", "type", "times_treat", yvar)]
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat) %>%
#   summarize(
#     mn = mean(wr, na.rm = T),
#     lwr = quantile(wr, 0.025, na.rm = T),
#     upr = quantile(wr, 0.975, na.rm = T)
#   ) %>% ungroup
# 
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
# 
# off_ctrl = -0.05
# off_trt1 = 0.05
# 
# ppi = 600
# png("blue_wr.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "")
# mtext(side = 2, "Bluegill Relative Weight (Wr)", line = 2.8, cex = 1.4)
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
# 
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1), lwr, I(year + off_trt1), upr, col = "red", lwd = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()









##### BACI analysis: PSD & PSD-P ----------------------------------------------------------------------------------
### PSD
output_psd_q = output
output_psd_q$psd_q = output_psd_q$psd_q + 1      # add 1 to PSD's for log()

hist(output_psd_q$psd_q)
hist(log(output_psd_q$psd_q))

## fit data in a model ##
## fit data in a model ##
fit_psd = lmer(log(psd_q) ~ times_treat + size_class + times_treat:size_class + (1 | lake) + (1 | year), data = output_psd_q, REML = F)
fit_psd = lmer(log(psd_q) ~ times_treat + size_class + (1 | lake) + (1 | year), data = output_psd_q, REML = F)
fit_psd = lmer(log(psd_q) ~ times_treat + (1 | lake) + (1 | year), data = output_psd_q, REML = F)
fit_psd = lmer(log(psd_q) ~ relevel(times_treat, ref = "1") + (1 | lake) + (1 | year), data = output_psd_q, REML = F)

summary(fit_psd)
anova(fit_psd)
coefs = summary(fit_psd)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lpsd CI %'s

# ## plot ##
# dat = output
# 
# yvar = "psd_q"
# 
# datsub = dat[,c("year", "type", "times_treat", yvar)]
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat) %>%
#   summarize(
#     mn = mean(psd_q, na.rm = T),
#     lwr = quantile(psd_q, 0.025, na.rm = T),
#     upr = quantile(psd_q, 0.975, na.rm = T)
#   ) %>% ungroup
# 
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
# 
# off_ctrl = -0.05
# off_trt1 = 0.05
# 
# ppi = 600
# png("blue_psd.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "")
# mtext(side = 2, "Bluegill PSD", line = 2.8, cex = 1.4)
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
# 
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1), lwr, I(year + off_trt1), upr, col = "red", lwd = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()









### PSD-P
output_psd_p = output
output_psd_p$psd_p = output_psd_p$psd_p + 1

hist(output_psd_p$psd_p)
hist(log(output_psd_p$psd_p))

## fit data in a model ##
fit_psd_p = lmer(log(psd_p) ~ times_treat + size_class + times_treat:size_class + (1 | lake) + (1 | year), data = output_psd_p, REML = F)
fit_psd_p = lmer(log(psd_p) ~ times_treat + size_class + (1 | lake) + (1 | year), data = output_psd_p, REML = F)
fit_psd_p = lmer(log(psd_p) ~ times_treat + (1 | lake) + (1 | year), data = output_psd_p, REML = F)
fit_psd_p = lmer(log(psd_p) ~ relevel(times_treat, ref = "1") + (1 | lake) + (1 | year), data = output_psd_p, REML = F)

summary(fit_psd_p)
anova(fit_psd_p)
coefs = summary(fit_psd_p)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

## plot ##
dat = output

yvar = "psd_p"

datsub = dat[,c("year", "type", "times_treat", yvar)]

library(dplyr)

plot_dat = datsub %>% group_by(year, type, times_treat) %>%
  summarize(
    mn = mean(psd_p, na.rm = T),
    lwr = quantile(psd_p, 0.025, na.rm = T),
    upr = quantile(psd_p, 0.975, na.rm = T)
  ) %>% ungroup

plot_dat$year = as.numeric(plot_dat$year)
plot_dat

off_ctrl = -0.05
off_trt1 = 0.05

ppi = 600
png("blue_psd_p.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(plot_dat[,c("lwr", "upr")]),
     xaxt = "n", las = 1, ylab = "")
mtext(side = 2, "Bluegill PSD-P", line = 2.8, cex = 1.4)

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
       col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
dev.off()





