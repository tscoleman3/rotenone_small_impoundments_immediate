### LARGEMOUTH BASS ###

rm(list = ls(all = T))
library(lme4)              # glmer 
library(nlme)              # lme
library(lmerTest)          # lmer pvalues
library(MuMIn)             # AICc 

# library(FSAdata)         # Ogle MLA data
# library(FSA)             # Ogle MLA functions
# library(dplyr)
# library(nnet)
# library(MASS)
# library(plyr)

# setwd("~/Documents/git/rotenone")

# highlight and run all code in funcs.R
source("script/funcs_lmb.R")
# highlight and run all code for AICc table
source("script/AICc_table_func.R")

## ALL BACI
## cpue 1
## cpue 2+
## mla 1 
## PSD
## P-PSD
## Wr (all stock and above fish)
## TTEST
## survival (age-1 cpue / followup seine previous yr)


##### LETS LOOP EVERYTHING -------------------------------------------------------------------------
mla_dat = read.csv("data/3.27.19data.csv")        # read in dataset
names(mla_dat)[1] = "lake"                          # this is important for non-Mac users
mla_dat = mla_dat[mla_dat$species == "lmb",]        # keep only LMB
mla_dat = droplevels(mla_dat)                       # remove empty cells 
# change the 1.5, 2.5 ages
mla_dat$age = ifelse(mla_dat$age %in% c(1.5, 2.5), mla_dat$age - 0.5, mla_dat$age)  
maxage = max(unique(mla_dat$age), na.rm = T)
lakenames = as.character(unique(mla_dat$lake))
years = unique(mla_dat$year)
# L = 13
# Y = 2

## create loop using functions in "lmb_funcs.R"
output = NULL
for (L in 1:length(lakenames)) {
  for (Y in 1:length(years)) {
    cat("\nLake:", lakenames[L], "; Year:", years[Y])
    
    datsub_ef = subset(mla_dat, lake == lakenames[L] & year == years[Y] & gear == "efishing")
    datsub_sn = subset(mla_dat, lake == lakenames[L] & year == years[Y] - 1 & gear == "seine")
    if (nrow(datsub_ef) == 0) {
      cat("skip ef\n")
      next
    } else {
      
      mla_sub = get_mla(datsub_ef)
      cpue_1 = round(get_cpue(datsub_ef, age = 1))
      cpue_2p = round(get_cpue(datsub_ef, age = 2)) # any age >1 returns a 2+ category
      wr = get_wr(datsub_ef, min_length = 150, std_a = -5.528, std_b = 3.273)
      psd_q = get_psd(datsub_ef, stock_length = 200, other_length = 300)   # this is PSD
      psd_p = get_psd(datsub_ef, stock_length = 200, other_length = 380)
      psd_m = get_psd(datsub_ef, stock_length = 200, other_length = 510)
      psd_t = get_psd(datsub_ef, stock_length = 200, other_length = 630)
      
      tmp = data.frame(lake = lakenames[L], year = years[Y],
                       cpue_1, cpue_2p, wr, psd_q, psd_p, psd_m, psd_t)
      
      tmp = cbind(tmp, mla_sub)
      
    }
    
    if (nrow(datsub_sn) == 0) {
      cat("skip seine\n")
      surv = NA
    } else {
      surv = get_surv(datsub_ef, datsub_sn)
    }
    tmp = cbind(tmp, surv)
    
    output = rbind(output, tmp)
  }
}

# combine lake id data with fish data
output = output[order(output$lake, output$year),]                  # orders output by lake and yr       
ids = read.csv("data/lake_year_ids.csv", stringsAsFactors = F)     # read in id file
output = merge(ids, output, by = c("lake", "year"))   # merge output and id files by lake and year
output = output[output$year != 2016,]                              # take out 2016 data
output$ba = relevel(factor(output$ba), ref = "before")             # make "before" ref group
output$year = as.character(output$year)                    
output$times_treat = as.factor(output$times_treat)         
output$size_class = ifelse(output$size >30, "big", "small")   # make size categorical 
output$size_class = relevel(factor(output$size_class), ref = "small")   # this might not work?
lakenames[!(lakenames %in% ids$lake)]    # show which lake names are not found in the id data set
output$type_treat = ifelse(output$times_treat == "0", "control", "treat")
output$cpue_1[is.na(output$cpue_1)] = 0                   # add a 0 in place of NA bc 0 caught fish
# write.csv(output, "lmb_output.csv", row.names = FALSE)        # write dataset into a .csv to send

## now can do stuff like this
# summary(lm(mla_1 ~ type, data = subset(output, ba == "after")))
## these two lines do the same thing: calculate mean surv index by type
# with(output, tapply(surv, type, mean, na.rm = T))
# tapply(output$surv, output$type, mean, na.rm = T)
# summary(lm(surv ~ type, data = subset(output, ba == "after")))



### old ###
### MATT: Use full model and report pvalue for int then drop int, then use no_int model and report 
###       pvalue for size, 
###       then drop size and report times_treat pvalue ==> all if the no_size model is best.
###       If null model is best, then we did not see an effect on this x-variable, easy to 
###       explain -> do what is said above and show no sign effect of times_treat.
###       Therefore, do not use (ie, report) AICc tables. Run models and take out non-sign effects 
###       in anova() results.

##### BACI analysis: MLA, age-1 --------------------------------------------------------------------
output_mla_1 = output   # rename output to manipulate for this analysis only
output_mla_1$mla_1[is.na(output_mla_1$mla_1)] = 0       # add a 0 in place of NA for removal
output_mla_1 = output_mla_1[output_mla_1$mla_1 != 0,]   # remove 0 in mla_1
unique(output_mla_1$mla_1)                              # 0's and NA's absent 

hist(output_mla_1$mla_1)
hist(log(output_mla_1$mla_1))

## fit data in a model ##
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + size_class + 
#                     size_class:times_treat + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
#                     relevel(size_class, ref = "big") + 
#                     relevel(size_class, ref = "big"):times_treat +
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "1") + 
#                     size_class + 
#                     size_class:relevel(times_treat, ref = "1") + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + size_class + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat * size_class + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "1") + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1_no_int = lmer(log(mla_1) ~ times_treat + size_class + 
#                            (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1_no_size = lmer(log(mla_1) ~ times_treat +
#                             (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1_base = lmer(log(mla_1) ~ 1 +
#                          (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1_yr = lmer(log(mla_1) ~ times_treat + year + 
#                        size_class + size_class:times_treat +
#                        (1 | lake), data = output_mla_1, REML = F)

# aicc table
# aic_mla_1 = aic_table(AICc(lmer_mla_1_full, 
#                            lmer_mla_1_no_size, 
#                            lmer_mla_1_no_int, 
#                            lmer_mla_1_base, 
#                            lmer_mla_1_yr))      # model w/o size is best

# hist(resid(lmer_mla_1_full))
# plot(resid(lmer_mla_1_full) ~ fitted(lmer_mla_1_full))      
# shows we want log()? -- it is close to the same logged and not



##### MLA-1 BACI MODELS FOR MANUSCRIPT ##### -------------------------------------------------------
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat * size_class + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + size_class + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "1") + size_class + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "2") + size_class + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + relevel(size_class, ref = "big") + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "1") + relevel(size_class, ref = "big") + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "2") + relevel(size_class, ref = "big") + 
#                     (1 | lake) + (1 | year), data = output_mla_1, REML = F)


datsub_small <- output_mla_1[output_mla_1$size_class %in% "small",]  # for small sized analysis
datsub_big <- output_mla_1[output_mla_1$size_class %in% "big",]    # for large sized analysis
# lmer_mla_1 = lmer(log(mla_1) ~ ba * type + 
#                     (1 | lake), data = datsub_small, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
#                     (1 | lake) + (1 | year), data = datsub_small, REML = F)
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
#                     (1 | year), data = datsub_small, REML = F)
lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
                    (1 | lake), data = datsub_small, REML = F)
lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "1") + 
                    (1 | lake), data = datsub_small, REML = F)
fit <- glmmPQL(log(mla_1) ~ times_treat, random = ~1|lake, 
               data = datsub_small, family = "quasipoisson")
summary(fit)

# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
#                     (1 | lake), data = datsub_big, REML = F)
lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
                    (1 | year), data = datsub_big, REML = F)
lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "1") + 
                    (1 | year), data = datsub_big, REML = F)
fit <- glmmPQL(log(mla_1) ~ times_treat, random = ~1|year, 
               data = datsub_big, family = "quasipoisson")
summary(fit)

# lmer_mla_1 = lm(log(mla_1) ~ times_treat, data = datsub_big)
# lmer_mla_1 = lm(log(mla_1) ~ times_treat, data = datsub_small)

summary(lmer_mla_1)
anova(lmer_mla_1)
coefs = summary(lmer_mla_1)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

# plot(exp(coefs[2:4,"Estimate"]), ylim = range(lwrs[2:4], uprs[2:4]))
# segments(1:7, lwrs[2:4], 1:7, uprs[2:4])
# abline(h = 1)

summary(fit)
anova(fit)

df$pred = exp(predict(fit, re.form = NA))

means = tapply(df$pred, list(df$period, df$type, df$event), mean)

means["followup","control",2]/means["before","control",1]  # period effects by treat level
means["followup","treat",2]/means["before","treat",1] 

## plot residuals from prediction to test for assumptions
resfit = resid(fit)
hist(resfit)
plot(log(df$count), resfit, ylab="Residuals", xlab="Count") 
abline(0, 0)              
## look at predictions from model
predict(fit)
df$pred = exp(predict(fit))
df$predicted = predict(fit)    # Save the predicted values
df$residuals = residuals(fit)  # Save the residual values
# Quick look at the actual, predicted, and residual values
pred_df = df %>% select(count, predicted, residuals) %>% head()
pred_df$predicted = exp(pred_df$predicted)
pred_df$residuals = exp(pred_df$residuals)
plot(fitted(fit) ~ df$count); abline(0,1)

## plot ##
dat = output

yvar = "mla_1"

datsub = dat[,c("year", "type", "times_treat", "size_class", yvar)]
datsub = na.omit(datsub)


library(dplyr)

plot_dat = datsub %>% group_by(year, type, times_treat, size_class) %>%
  summarize(
    mn = mean(mla_1, na.rm = T),
    lwr = quantile(mla_1, 0.025, na.rm = T),
    upr = quantile(mla_1, 0.975, na.rm = T)
  ) %>% ungroup

plot_dat$year = as.numeric(plot_dat$year)
plot_dat

off_ctrl = -0.05
off_trt1 = 0.05

## small lakes
ppi = 600
png("mla_1_small.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.3)
plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1),
     ylim = range(plot_dat[,c("lwr", "upr")]),
     xaxt = "n", las = 1, 
     ylab = "Electrofishing MLA-1 LMB (mm)",
     main = "Small Impoundments <12ha")

lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,5,10),], col = "blue", lwd = 2)
lines(mn ~ year, data = plot_dat[c(3,8),], col = "red", lwd = 2)
lines(mn ~ I(year + off_trt1), data = plot_dat[c(7,12),], col = "red", lwd = 2)
lines(mn ~ year, data = plot_dat[c(8,13),], lty = 2, col = "red", lwd = 2)

with(plot_dat[c(1,5,10),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl), 
                                    upr, col = "blue", lwd = 2))
with(plot_dat[c(3,8),], segments(year, lwr, year, upr, col = "red", lwd = 2))
with(plot_dat[c(7,12),], segments(I(year + off_trt1), lwr, I(year + off_trt1), 
                                  upr, col = "red", lwd = 2))
with(plot_dat[c(8,13),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))

axis(side = 1, at = 2017:2019, labels = 2017:2019)
legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
       col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
dev.off()

## large lakes
ppi = 600
png("mla_1_large.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.3)
plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), 
     ylim = range(plot_dat[,c("lwr", "upr")]),
     xaxt = "n", las = 1, ylab = "Electrofishing MLA-1 LMB (mm)", 
     main = "Small Impoundments <12ha")

## small lakes
lines(mn ~ I(year + off_ctrl), data = plot_dat[c(2,6,11),], col = "blue", lwd = 2)
lines(mn ~ year, data = plot_dat[c(4,9),], col = "red", lwd = 2)
lines(mn ~ year, data = plot_dat[c(9,14),], lty = 2, col = "red", lwd = 2)

with(plot_dat[c(2,6,11),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl),
                                    upr, col = "blue", lwd = 2))
with(plot_dat[c(4,9),], segments(year, lwr, year, upr, col = "red", lwd = 2))
with(plot_dat[c(9,14),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))

axis(side = 1, at = 2017:2019, labels = 2017:2019)
legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
       col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
dev.off()

# ## plot ##
# dat = output
#
# yvar = "mla_1"
#
# datsub = dat[,c("year", "type", "times_treat", yvar)]
# datsub = na.omit(datsub)
#
#
# library(dplyr)
#
# plot_dat = datsub %>% group_by(year, type, times_treat) %>%
#   summarize(
#     mn = mean(mla_1, na.rm = T),
#     lwr = quantile(mla_1, 0.025, na.rm = T),
#     upr = quantile(mla_1, 0.975, na.rm = T)
#   ) %>% ungroup
#
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
#
# off_ctrl = -0.05
# off_trt1 = 0.05
#
# ppi = 600
# png("mla_1.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1),
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "Electrofishing MLA-1 LMB (mm)")
#
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
#
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl),
#                                    upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1), lwr, I(year + off_trt1), 
#                                  upr, col = "red", lwd = 2))
#
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()










##### BACI analysis: cpue age-1 --------------------------------------------------------------------
hist(output$cpue_1)              # non-normal distribution
hist(log(output$cpue_1))         # normal distribution
# look at dist of hist(resid(fit)) also

output_cpue_1 = output    # create new dataset to manipulate
output_cpue_1$cpue_1[is.na(output_cpue_1$cpue_1)] = 0    # add a 0 (in place of NA) bc NA = 0 fish
output_cpue_1$cpue_1 = output_cpue_1$cpue_1 + 1          # add 1 to all cpue_1 to log() later
datsub_small <- output_cpue_1[output_cpue_1$size_class %in% "small",]  # for small sized analysis
datsub_big <- output_cpue_1[output_cpue_1$size_class %in% "big",]    # for large sized analysis

# same models as above due to singularity 
fit_cpue_1 = lmer(log(cpue_1) ~ times_treat + 
                    (1 | lake), data = datsub_small, REML = F)
fit_cpue_1 = lmer(log(cpue_1) ~ relevel(times_treat, ref = "1") + 
                    (1 | lake), data = datsub_small, REML = F)
fit <- glmmPQL(log(cpue_1) ~ times_treat, random = ~1|lake, 
               data = datsub_small, family = "quasipoisson")
summary(fit)


fit_cpue_1 = lmer(log(cpue_1) ~ times_treat + 
                    (1 | year), data = datsub_big, REML = F)
fit_cpue_1 = lmer(log(cpue_1) ~ relevel(times_treat, ref = "1") + 
                    (1 | year), data = datsub_big, REML = F)
fit <- glmmPQL(log(cpue_1) ~ times_treat, random = ~1|year, 
               data = datsub_big, family = "quasipoisson")
summary(fit)

summary(fit_cpue_1)
anova(fit_cpue_1)
coefs = summary(fit_cpue_1)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s



### old ###

## fit data in a model
# fit_cpue_1 = lmer(log(cpue_1) ~ times_treat + size_class + times_treat:size_class + 
#                     (1 | lake) + (1 | year), data = output_cpue_1, REML = F)
# fit_cpue_1 = lmer(log(cpue_1) ~ times_treat + times_treat:size_class + (1 | lake) +
#                     (1 | year), data = output_cpue_1, REML = F)
# fit_cpue_1 = lmer(log(cpue_1) ~ times_treat  + (1 | lake) + (1 | year), 
#                   data = output_cpue_1, REML = F)
# fit_cpue_1 = lmer(log(cpue_1) ~ relevel(times_treat, ref = "1") + 
#                     (1 | lake) + (1 | year), data = output_cpue_1, REML = F)
# fit_cpue_1 = lmer(log(cpue_1) ~ times_treat + size_class + 
#                     (1 | lake) + (1 | year), data = output_cpue_1, REML = F)

# fit_cpue_1 = lmer(log(cpue_1) ~ relevel(times_treat, ref = "1") + size_class + 
#                     (1 | lake) + (1 | year), data = output_cpue_1, REML = F)
# fit_cpue_1 = lmer(log(cpue_1) ~ relevel(times_treat, ref = "2") + size_class +
#                     (1 | lake) + (1 | year), data = output_cpue_1, REML = F)
# fit_cpue_1 = lmer(log(cpue_1) ~ times_treat + relevel(size_class, ref = "big") + 
#                     (1 | lake) + (1 | year), data = output_cpue_1, REML = F)
# fit_cpue_1 = lmer(log(cpue_1) ~ relevel(times_treat, ref = "1") + 
#                     relevel(size_class, ref = "big") + (1 | lake) + 
#                     (1 | year), data = output_cpue_1, REML = F)

# AICc table of models
# aic_cpue_1 = aic_table(AICc(lmer_cpue_1_full, 
#                             lmer_cpue_1_no_int, 
#                             lmer_cpue_1_no_size, 
#                             lmer_cpue_1_null))    # model w/o interaciton is best
# anova(fit_cpue_1a, fit_cpue_1i)

# ## plot ##
# dat = output
# 
# yvar = "cpue_1"
# 
# datsub = dat[,c("year", "type", "times_treat", "size_class", yvar)]
# datsub$cpue_1[is.na(datsub$cpue_1)] = 0    # add a 0 (in place of NA) bc NA = 0 fish
# 
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat, size_class) %>%
#   summarize(
#     mn = mean(cpue_1, na.rm = T),
#     lwr = quantile(cpue_1, 0.025, na.rm = T),
#     upr = quantile(cpue_1, 0.975, na.rm = T)
#   ) %>% ungroup
# 
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
# 
# off_ctrl = -0.05
# off_trt1 = 0.05
# 
# ## small lakes
# ppi = 600
# png("cpue_1_small.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(0, 100),
#      xaxt = "n", las = 1, ylab = "Electrofishing CPUE Age-1 LMB (mm)", 
#      main = "Small Impoundments <12ha")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,5,10),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(3,8),], col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(7,12),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(8,13),], lty = 2, col = "red", lwd = 2)
# 
# with(plot_dat[c(1,5,10),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl), 
#                                     upr, col = "blue", lwd = 2))
# with(plot_dat[c(3,8),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(7,12),], segments(I(year + off_trt1), lwr, I(year + off_trt1),
#                                   upr, col = "red", lwd = 2))
# with(plot_dat[c(8,13),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()
# 
# ## large lakes
# ppi = 600
# png("cpue_1_large.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), 
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "Electrofishing CPUE Age-1 LMB (mm)", 
#      main = "Large Impoundments >33ha")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(2,6,11),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(4,9),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(9,14),], lty = 2, col = "red", lwd = 2)
# 
# with(plot_dat[c(2,6,11),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl),
#                                     upr, col = "blue", lwd = 2))
# with(plot_dat[c(4,9),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(9,14),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()
# 
# ## plot ##
# dat = output
# 
# yvar = "cpue_1"
# 
# datsub = dat[,c("year", "type", "times_treat", yvar)]
# datsub$cpue_1[is.na(datsub$cpue_1)] = 0    # add a 0 (in place of NA) bc NA = 0 fish
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat) %>%
#   summarize(
#     mn = mean(cpue_1, na.rm = T),
#     lwr = quantile(cpue_1, 0.025, na.rm = T),
#     upr = quantile(cpue_1, 0.975, na.rm = T)
#   ) %>% ungroup
# 
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
# 
# off_ctrl = -0.05
# off_trt1 = 0.05
# 
# ppi = 600
# png("lmb_cpue_1.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), 
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "Electrofishing LMB Age-1 CPUE")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
# 
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl),
#                                    upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1), lwr, I(year + off_trt1), 
#                                  upr, col = "red", lwd = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()








##### BACI analysis: cpue age-2+ -------------------------------------------------------------------
hist(output$cpue_2p)             
hist(log(output$cpue_2p))
# check hist(resid(fit)) also

output_cpue_2p = output   # create new dataset to manipulate
output_cpue_2p$cpue_2p[is.na(output_cpue_2p$cpue_2p)] = 0     # add a 0 (in place of NA) 
output_cpue_2p$cpue_2p = output_cpue_2p$cpue_2p + 1           # add 1 to all cpue_2p to log() later

## fit data in a model 

## fit data in a model
fit_cpue_2p = lmer(log(cpue_1) ~ times_treat + size_class + times_treat:size_class + 
                     (1 | lake) + (1 | year), data = output_cpue_2p, REML = F)
fit_cpue_2p = lmer(log(cpue_2p) ~ times_treat + size_class +
                     (1 | lake) + (1 | year), data = output_cpue_2p, REML = F)
fit_cpue_2p = lmer(log(cpue_2p) ~ times_treat + 
                     (1 | lake) + (1 | year), data = output_cpue_2p, REML = F)
fit_cpue_2p = lmer(log(cpue_2p) ~ x +
                     (1 | lake) + (1 | year), data = output_cpue_2p, REML = F)
fit_cpue_2p = lmer(log(cpue_2p) ~ relevel(times_treat, ref = "1") + 
                     (1 | lake) + (1 | year), data = output_cpue_2p, REML = F)

# AICc table of models
# aic_cpue_2p = aic_table(AICc(lmer_cpue_2p_full, 
#                              lmer_cpue_2p_no_int, 
#                              lmer_cpue_2p_no_size, 
#                              lmer_cpue_2p_null))    # model w/o interaciton is best

summary(fit_cpue_2p)
anova(fit_cpue_2p)
coefs = summary(fit_cpue_2p)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

# ## plot ##
# dat = output
# 
# yvar = "cpue_2p"
# 
# datsub = dat[,c("year", "type", "times_treat", yvar)]
# datsub = na.omit(datsub)
# 
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat) %>%
#   summarize(
#     mn = mean(cpue_2p, na.rm = T),
#     lwr = quantile(cpue_2p, 0.025, na.rm = T),
#     upr = quantile(cpue_2p, 0.975, na.rm = T)
#   ) %>% ungroup
# 
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
# 
# off_ctrl = -0.05
# off_trt1 = 0.05
# 
# ppi = 600
# png("lmb_cpue_2p.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1),
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "Electrofishing LMB Age-2+ CPUE")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
# 
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl),
#                                    upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1), lwr, I(year + off_trt1), 
#                                  upr, col = "red", lwd = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()

# ## plot ##
# dat = output
# 
# yvar = "cpue_2p"
# 
# datsub = dat[,c("year", "type", "times_treat", "size_class", yvar)]
# datsub = na.omit(datsub)
# 
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat, size_class) %>%
#   summarize(
#     mn = mean(cpue_2p, na.rm = T),
#     lwr = quantile(cpue_2p, 0.025, na.rm = T),
#     upr = quantile(cpue_2p, 0.975, na.rm = T)
#   ) %>% ungroup
# 
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
# 
# off_ctrl = -0.05
# off_trt1 = 0.05
# 
# ## small lakes
# ppi = 600
# png("cpue_2p_small.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(0, 90),
#      xaxt = "n", las = 1, ylab = "Electrofishing CPUE Age-2+ LMB (mm)",
#      main = "Small Impoundments <12ha")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,5,10),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(3,8),], col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(7,12),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(8,13),], lty = 2, col = "red", lwd = 2)
# 
# with(plot_dat[c(1,5,10),], segments(I(year + off_ctrl), 
#                                     lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(3,8),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(7,12),], segments(I(year + off_trt1),
#                                   lwr, I(year + off_trt1), upr, col = "red", lwd = 2))
# with(plot_dat[c(8,13),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()
# 
# ## large lakes
# ppi = 600
# png("cpue_2p_large.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), 
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "Electrofishing CPUE Age-2+ LMB (mm)", 
#      main = "Large Impoundments >33ha")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(2,6,11),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(4,9),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(9,14),], lty = 2, col = "red", lwd = 2)
# 
# with(plot_dat[c(2,6,11),], segments(I(year + off_ctrl), 
#                                     lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(4,9),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(9,14),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()









##### BACI analysis: Relative Weight (Wr) ----------------------------------------------------------
hist(log(output$wr))
hist(output$wr)
# check hist(resid(fit))
min(output$wr)

## fit data in a model ##
fit_wr = lmer(log(wr) ~ times_treat + size_class + times_treat:size_class + 
                (1 | lake) + (1 | year), data = output, REML = F)
fit_wr = lmer(log(wr) ~ times_treat + size_class + 
                (1 | lake) + (1 | year), data = output, REML = F)
fit_wr = lmer(log(wr) ~ times_treat + 
                (1 | lake) + (1 | year), data = output, REML = F)
fit_wr = lmer(log(wr) ~ relevel(times_treat, ref = "1") + 
                (1 | lake) + (1 | year), data = output, REML = F)

# lmer_wr_full = lmer(wr ~ times_treat + size_class + size_class:times_treat +
#                       (1 | lake) + (1 | year), data = output, REML = F)
# lmer_wr_no_int = lmer(wr ~ times_treat + size_class + 
#                         (1 | lake) + (1 | year), data = output, REML = F)
# lmer_wr_no_size = lmer(wr ~ times_treat + 
#                          (1 | lake) + (1 | year), data = output, REML = F)
# lmer_wr_null = lmer(wr ~ 1 + 
#                       (1 | lake) + (1 | year), data = output, REML = F)
# # aicc table
# aic_wr = aic_table(AICc(lmer_wr_full, 
#                         lmer_wr_no_int, 
#                         lmer_wr_no_size, 
#                         lmer_wr_null))       # model w/o interaction is best

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
# datsub = na.omit(datsub)
# 
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
# png("lmb_wr.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), 
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "LMB Relative Weight (Wr)")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
# 
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), 
#                                    lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1), 
#                                  lwr, I(year + off_trt1), upr, col = "red", lwd = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()

# ## plot ##
# dat = output
# 
# yvar = "wr"
# 
# datsub = dat[,c("year", "type", "times_treat", "size_class", yvar)]
# datsub = na.omit(datsub)
# 
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat, size_class) %>%
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
# ## small lakes
# ppi = 600
# png("lmb_wr_small.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.4)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), 
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "LMB Relative Weight (Wr)", main = "Small Impoundments <12ha")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,5,10),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(3,8),], col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(7,12),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(8,13),], lty = 2, col = "red", lwd = 2)
# 
# with(plot_dat[c(1,5,10),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl), 
#                                     upr, col = "blue", lwd = 2))
# with(plot_dat[c(3,8),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(7,12),], segments(I(year + off_trt1), lwr, I(year + off_trt1), 
#                                   upr, col = "red", lwd = 2))
# with(plot_dat[c(8,13),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()
# 
# ## large lakes
# ppi = 600
# png("lmb_wr_large.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.4)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(85, 95),
#      xaxt = "n", las = 1, ylab = "LMB Relative Weight (Wr)", main = "Large Impoundments >33ha")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(2,6,11),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(4,9),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(9,14),], lty = 2, col = "red", lwd = 2)
# 
# with(plot_dat[c(2,6,11),], segments(I(year + off_ctrl), 
#                                     lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(4,9),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(9,14),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()









##### BACI analysis: PSD & PSD-P -------------------------------------------------------------------
### PSD
output_psd = output        # create new dataset to manipulate              
output_psd$psd_q = output_psd$psd_q + 1          # add 1 to all cpue_1 to log() later

hist(output_psd$psd_q)
hist(log(output_psd$psd_q))

## fit data in a model ##
fit_psd = lmer(log(psd_q) ~ times_treat + size_class + times_treat:size_class +
                 (1 | lake) + (1 | year), data = output_psd, REML = F)
fit_psd = lmer(log(psd_q) ~ times_treat + size_class + 
                 (1 | lake) + (1 | year), data = output_psd, REML = F)
fit_psd = lmer(log(psd_q) ~ times_treat + relevel(size_class, ref = "big") +
                 (1 | lake) + (1 | year), data = output_psd, REML = F)
fit_psd = lmer(log(psd_q) ~ relevel(times_treat, ref = "1") + relevel(size_class, ref = "big") + 
                 (1 | lake) + (1 | year), data = output_psd, REML = F)
fit_psd = lmer(log(psd_q) ~ times_treat +
                 (1 | lake) + (1 | year), data = output_psd, REML = F)
fit_psd = lmer(log(psd_q) ~ x +
                 (1 | lake) + (1 | year), data = output_psd, REML = F)

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
# datsub = na.omit(datsub)
# 
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
# png("lmb_psd.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), 
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "LMB PSD")
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
# 
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), 
#                                    lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1),
#                                  lwr, I(year + off_trt1), upr, col = "red", lwd = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()

# ## plot ##
# dat = output
# 
# yvar = "psd_q"
# 
# datsub = dat[,c("year", "type", "times_treat", "size_class", yvar)]
# datsub = na.omit(datsub)
# 
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat, size_class) %>%
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
# ## small lakes
# ppi = 600
# png("lmb_psd_small.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.4)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1),
#      ylim = range(plot_dat[, c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "", main = "Small Impoundments <12ha")
# mtext(side = 2, "LMB PSD", line = 2.8, cex = 1.4)
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,5,10),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(3,8),], col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(7,12),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(8,13),], lty = 2, col = "red", lwd = 2)
# 
# with(plot_dat[c(1,5,10),], segments(I(year + off_ctrl),
#                                     lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(3,8),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(7,12),], segments(I(year + off_trt1), 
#                                   lwr, I(year + off_trt1), upr, col = "red", lwd = 2))
# with(plot_dat[c(8,13),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()
# 
# ## large lakes
# ppi = 600
# png("lmb_psd_large.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4.1,1.25,1), cex.axis = 1.2, cex.lab = 1.4)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), ylim = range(10, 60),
#      xaxt = "n", las = 1, ylab = "", main = "Large Impoundments >33ha")
# mtext(side = 2, "LMB PSD", line = 2.8, cex = 1.4)
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(2,6,11),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(4,9),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(9,14),], lty = 2, col = "red", lwd = 2)
# 
# with(plot_dat[c(2,6,11),], segments(I(year + off_ctrl), 
#                                     lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(4,9),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(9,14),], segments(year, lwr, year, upr, col = "red", lwd = 2, lty = 2))
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
# fit_psd_p = lmer(log(psd_p) ~ times_treat + size_class + times_treat:size_class +
#                    (1 | lake) + (1 | year), data = output_psd_p, REML = F)
# fit_psd_p = lmer(log(psd_p) ~ times_treat + size_class +
#                    (1 | lake) + (1 | year), data = output_psd_p, REML = F)
fit_psd_p = lmer(log(psd_p) ~ times_treat + (1 | lake) + (1 | year), data = output_psd_p, REML = F)

summary(fit_psd_p)
anova(fit_psd_p)
coefs = summary(fit_psd_p)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

# ## plot ##
# dat = output
# 
# yvar = "psd_p"
# 
# datsub = dat[,c("year", "type", "times_treat", yvar)]
# 
# library(dplyr)
# 
# plot_dat = datsub %>% group_by(year, type, times_treat) %>%
#   summarize(
#     mn = mean(psd_p, na.rm = T),
#     lwr = quantile(psd_p, 0.025, na.rm = T),
#     upr = quantile(psd_p, 0.975, na.rm = T)
#   ) %>% ungroup
# 
# plot_dat$year = as.numeric(plot_dat$year)
# plot_dat
# 
# off_ctrl = -0.05
# off_trt1 = 0.05
# 
# ppi = 600
# png("lmb_psd_p.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1), 
#      ylim = range(plot_dat[,c("lwr", "upr")]),
#      xaxt = "n", las = 1, ylab = "")
# mtext(side = 2, "LMB PSD-P", line = 2.8, cex = 1.4)
# 
# lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,3,6),], col = "blue", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(2,5),], col = "red", lwd = 2)
# lines(mn ~ year, data = plot_dat[c(5,8),], lty = 2, col = "red", lwd = 2)
# lines(mn ~ I(year + off_trt1), data = plot_dat[c(4,7),], col = "red", lwd = 2)
# 
# with(plot_dat[c(1,3,6),], segments(I(year + off_ctrl), 
#                                    lwr, I(year + off_ctrl), upr, col = "blue", lwd = 2))
# with(plot_dat[c(2,5),], segments(year, lwr, year, upr, col = "red", lwd = 2))
# with(plot_dat[c(5,8),], segments(year, lwr, year, upr, lty = 2, col = "red", lwd = 2))
# with(plot_dat[c(4,7),], segments(I(year + off_trt1), 
#                                  lwr, I(year + off_trt1), upr, col = "red", lwd = 2))
# 
# axis(side = 1, at = 2017:2019, labels = 2017:2019)
# legend("topright", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)
# dev.off()









##### Survival Index -------------------------------------------------------------------------------
# hist(output$surv)
# hist(log(output$surv))
# 
# survival = output
# survival = survival[survival$ba != "before",]
# survival$surv[is.na(survival$surv)] = 0
# 
## fit data in a model ##
# survival = output[!is.na(output$surv),]
# fit = lmer(log(surv) ~ times_treat + size_class + times_treat:size_class +
#              (1 | lake) + (1 | year), data = survival, REML = F)
# fit = lmer(log(surv) ~ times_treat + size_class + (1 | lake) + (1 | year), data = survival, REML = F)
# fit = lmer(log(surv) ~ times_treat + (1 | lake) + (1 | year), data = survival, REML = F)
# fit = lm(log(surv) ~ times_treat + size_class, data = survival)
# fit = lm(log(surv) ~ times_treat, data = survival)
# summary(fit)
# anova(fit)
# 
# 
# ## ttest ##
# c_surv = subset(survival, type == "control" & ba == "after")
# c_surv = c_surv[, c("lake", "year", "ba", "size", "type_times_treat", "surv")]
# 
# t_surv = subset(survival, type == "treat" & ba == "after")
# t_surv = t_surv[, c("lake", "year", "ba", "size", "type_times_treat", "surv")]
# # t_surv$surv[is.na(t_surv$surv)] = 0   # add a 0 in place of NA ; not a 0, it is an NA
# 
# hist(c_surv$surv)
# hist(log(c_surv$surv))
# hist(t_surv$surv)
# hist(log(t_surv$surv))
# 
# t_test = t.test(log(c_surv$surv), log(t_surv$surv))
# exp(t_test$estimate)

## fit raw data in a regression ##
survival = output[output$ba != "before",]      # remove "before" bc not needed
survival$cpue_1[is.na(survival$cpue_1)] = 0
colnames(survival)
survival = survival[,c("lake", "year", "size_class", "cpue_1", "times_treat")]
# # df followup seine is now a .csv file (lmb_followup.csv)
# df = df                                              # from "seine_dataanalysis_lmb.R"
# df = df[df$period != "before",]                      # remove "before" bc not needed
# df$year = as.numeric(df$year) + 1                    # make df years = to survival years to combine datasets
# write.csv(df, "lmb_followup.csv", row.names = FALSE) 
# # df day1 seine is now a .csv file (lmb_day1.csv)
# df = df
# df = df[df$period != "after",]
# df = df[df$event != "2",]
# df$year = as.numeric(df$year) + 1
# write.csv(df, "lmb_day1.csv", row.names = FALSE)

# combine df and survival datasets
surv_dat = survival[order(survival$lake, survival$year),]        # order dat by lake and yr
followup = read.csv("lmb_followup.csv", stringsAsFactors = F)    # read in followup data
colnames(followup)[colnames(followup) == "count"] = "sn42_cpue"  # rename followup column
# day1 = read.csv("lmb_day1.csv", stringsAsFactors = F)            # read in day1 data
# colnames(day1)[colnames(day1) == "count"] = "sn1_cpue"           # rename day1 column
# day1 = day1[c("lake", "year", "sn1_cpue")]            # only keep lake, yr, and cpue before merge
surv_dat = merge(followup, surv_dat, by = c("lake", "year"))     # merge files by lake and year
# surv_dat = merge(day1, surv_dat, by = c("lake", "year"))         # merge files by lake and year
surv_dat = surv_dat[c("lake", "year", "sn42_cpue",
                      "cpue_1", "type", "times_treat", "size")]  # keep these columns only

surv_dat$size = relevel(factor(surv_dat$size), ref = "small") 
surv_dat$year = as.character(surv_dat$year)                    
surv_dat$times_treat = as.factor(surv_dat$times_treat)         

## plot ##
par(mar = c(5,5,1,1))
# plot(log(cpue_1 + 1) ~ log(sn42_cpue + 1),
#      col = c("blue", "red", "black")[times_treat], data = surv_dat)
plot(log(cpue_1 + 1) ~ log(sn42_cpue + 1), col = c("black", "black", "black")[times_treat],
     pch = c(21, 19, 15)[times_treat],
     cex = 1.75, cex.lab = 1.75, cex.axis = 1.5, las = 1, data = surv_dat,
     ylab = "ln LMB Electrofishing CPUE",
     xlab = "ln Previous Year LMB Follow-Up Seine CPUE")
legend("topleft", legend = c("Untreated", "Treated Once", "Treated Twice"),
       pch = c(21, 19, 15), pt.cex = c(1.5, 1.5, 1.5), pt.bg = c("white", "black", "black"),
       bty = "n", cex = 1.5)
abline(0,1)
# plot(log(cpue_1 + 1) ~ log(sn1_cpue + 1),
#      col = c("blue", "red", "black")[times_treat], data = surv_dat, pch = 16)
# abline(0,1)

# surv_dat$sn1_cpue = surv_dat$sn1_cpue + 1
surv_dat$sn42_cpue = surv_dat$sn42_cpue + 1    # no undefined values
# surv_dat$cpue_1 = surv_dat$cpue_1 + 1

surv_dat$surv_42 = (surv_dat$cpue_1) / (surv_dat$sn42_cpue)
# surv_dat$surv_1 =  (surv_dat$cpue_1) / (surv_dat$sn1_cpue)

fit = lm(surv_42 ~ times_treat + size, data = surv_dat)
fit = lm(surv_42 ~ times_treat, data = surv_dat)
# fit = lm(surv_42 ~ x, data = surv_dat)
summary(fit)
anova(fit)
abline(fit)

## boxplot ##
c0 = surv_dat[surv_dat$times_treat == 0,]
c0_mean = sum(c0$surv_42) / 14
c0_lwr = quantile(c0$surv_42, 0.025, na.rm = T)
c0_upr = quantile(c0$surv_42, 0.975, na.rm = T)

t1 = surv_dat[surv_dat$times_treat == 1,]
t1_mean = sum(t1$surv_42) / 10
t1_lwr = quantile(t1$surv_42, 0.025, na.rm = T)
t1_upr = quantile(t1$surv_42, 0.975, na.rm = T)

t2 = surv_dat[surv_dat$times_treat == 2,]
t2_mean = sum(t2$surv_42) / 4
t2_lwr = quantile(t2$surv_42, 0.025, na.rm = T)
t2_upr = quantile(t2$surv_42, 0.975, na.rm = T)

m <- matrix(NA, 1, 3)
m[1,1] = c0_mean
m[1,2] = t1_mean
m[1,3] = t2_mean
ppi = 600
png("surv_plot.png", h = 5*ppi, w = 5*ppi, res = ppi)
par(mar = c(4,4,0,1))
mp = barplot(m, beside = T, ylim = c(0, max(c(c0_upr, t1_upr, t2_upr), na.rm = T) + 1), las = 1, 
             cex.axis = 1.2, 
             col = c("white", "black", "grey60"))
lines(c(mp[1], mp[1]),c(c0_lwr, c0_upr), lwd = 2, col = "grey")
lines(c(mp[2], mp[2]),c(t1_lwr, t1_upr), col = "grey", lwd = 2)
lines(c(mp[3], mp[3]),c(t2_lwr, t2_upr), lwd = 2, col = "grey")

mtext(side = 2, "Survival Index", line = 2.6, cex = 1.5)
mtext(side = 1, "Times Treated", line = 2.6, cex = 1.5)
mp <- as.numeric(mp) 
mp <- mp[1:3]

axis(side = 1, at = mp, labels = (c("0", "1", "2")), cex.axis =1.2)
dev.off()






##### BACI analysis: MLA, age-2 ---------------------CAN NOT USE------------------------------------
# output_mla_2 = output                                   # rename output to manipulate for this analysis only
# output_mla_2$mla_2[is.na(output_mla_2$mla_2)] = 0       # add a 0 in place of NA for removal
# output_mla_2 = output_mla_2[output_mla_2$mla_2 != 0,]   # remove 0 in mla_2
# unique(output_mla_2$mla_2)                              # 0's and NA's absent
# 
# hist(output_mla_2$mla_2)
# hist(log(output_mla_2$mla_2))
# 
# ## fit data in a model ##
# lmer_mla_2_full = lmer(mla_2 ~ times_treat + size + size:times_treat + (1 | lake) + (1 | year), data = output_mla_2, REML = F)
# lmer_mla_2_no_int = lmer(mla_2 ~ times_treat + size + (1 | lake) + (1 | year), data = output_mla_2, REML = F)
# lmer_mla_2_no_size = lmer(mla_2 ~ times_treat + (1 | lake) + (1 | year), data = output_mla_2, REML = F)
# 
# # aicc table
# aic_mla_2 = aic_table(AICc(lmer_mla_2_full, lmer_mla_2_no_size, lmer_mla_2_no_int))      # model w/o interaction is best
# 
# hist(resid(lmer_mla_2_no_int))
# 
# summary(lmer_mla_2_full)
# summary(lmer_mla_2_no_int)
# ranova(lmer_mla_2_no_int)
# 
# # bootMer to get CIs
# plotfunc = function(.) {
#   beta = unname(fixef(.))
# 
#   c(
#     before = beta[1],                  # before
#     treat1 = (beta[1] + beta[2]),      # treat1
#     treat2 = (beta[1] + beta[3])       # treat2
#   )
# 
# }
# 
# start = Sys.time()
# rand = bootMer(x = lmer_mla_2_no_int, FUN = plotfunc, nsim = 1000)$t
# Sys.time() - start
# 
# summ = apply(rand, 2, function(x) c(mean = mean(x, na.rm = TRUE),quantile(x, c(0.025, 0.975), na.rm = TRUE)))
# 
# 
# ## plot ##




##### OLD; everything below is commented out #####

### went a different direction for looping ###
# create container for output from each lake/year combo
# output = expand.grid(lake = lakenames, year = years)
# m = matrix(NA, nrow(output), maxage); colnames(m) = paste("mla", 1:maxage, sep = "_")
# output = cbind(output, m)
# output$cpue_1 = NA
# output$cpue_2p = NA
# output$wr = NA
# output$psd = NA
# output$psd_p = NA



## wrong analyses below
## LMER -- log() y
# global_lmer_cpue_1 = lmer(log(cpue_1) ~ ba + type_times_treat + size + ba:type_times_treat + ba:size + type_times_treat:size + 
#                        ba:type_times_treat:size + (1 | lake), data = output)
# global_lmer_cpue_1b = lmer(log(cpue_1) ~ ba + type + size + ba:type + ba:size + type:size + 
#                             ba:type:size + (1 | lake), data = output)
# 
# no_treatsize_interaction_lmer_cpue_1 = lmer(log(cpue_1) ~ ba + type_times_treat + size + ba:type_times_treat +  ba:size + 
#                                             ba:type_times_treat:size + (1 | lake), data = output)
# no_treatsize_interaction_lmer_cpue_1b = lmer(log(cpue_1) ~ ba + type + size + ba:type + ba:size + 
#                                                ba:type:size + (1 | lake), data = output)
# 
# no_3way_lmer_cpue_1 = lmer(log(cpue_1) ~ ba + type_times_treat + size + ba:type_times_treat + ba:size + (1 | lake), data = output)
# no_3way_lmer_cpue_1b = lmer(log(cpue_1) ~ ba + type + size + ba:type + ba:size + (1 | lake), data = output)
# 
# treattime_interaction_lmer_cpue_1 = lmer(log(cpue_1) ~ ba + type_times_treat + size + ba:type_times_treat + (1 | lake), data = output)
# treattime_interaction_lmer_cpue_1b = lmer(log(cpue_1) ~ ba + type + size + ba:type + (1 | lake), data = output)
# 
# size_base_lmer_cpue_1 = lmer(log(cpue_1) ~ ba + type_times_treat + size + (1 | lake), data = output)
# size_base_lmer_cpue_1b = lmer(log(cpue_1) ~ ba + type + size + (1 | lake), data = output)
# 
# base_lmer_cpue_1 = lmer(log(cpue_1) ~ ba + type_times_treat + (1 | lake), data = output)
# base_lmer_cpue_1b = lmer(log(cpue_1) ~ ba + type + (1 | lake), data = output)
# 
# fit = lmer(log(cpue_1) ~ ba + type + ba:type + (1 | lake), data = output)
# 
# 
# MuMIn::AICc(global_lmer_cpue_1, global_lmer_cpue_1b, no_treatsize_interaction_lmer_cpue_1, no_treatsize_interaction_lmer_cpue_1b,
#             no_3way_lmer_cpue_1, no_3way_lmer_cpue_1b, treattime_interaction_lmer_cpue_1, treattime_interaction_lmer_cpue_1b, 
#             size_base_lmer_cpue_1, size_base_lmer_cpue_1b, base_lmer_cpue_1, base_lmer_cpue_1b, fit)
# ## summary of best fitting model
# summary(treattime_interaction_lmer_cpue_1b)
# plot(treattime_interaction_lmer_cpue_1b)
# 
# 
# quantile(output$size, c(0.1, 0.9))  # 10% (.1) of lakes are smaller than and 90% (.9) of lakes are smaller than these sizes
# 
# dat.test = expand.grid(ba = unique(output$ba), type_times_treat = unique(output$type_times_treat), size = quantile(output$size, c(0.1, 0.9)))
# 
# #dat.test = data.frame(ba = "after", type_times_treat = "control", size = mean(output$size))
# pred = predict(size_fit_lmer_cpue_1, newdata = dat.test, re.form = NA)
# dat.test$pred =exp(pred)


# fit_lmer_cpue_1 = lmer(log(cpue_1) ~ year + times_treat + year*times_treat*size + (1 | lake), data = output)
# fit_lmer_cpue_1 = lmer(log(cpue_1) ~ ba + type_times_treat + ba*type_times_treat + (1 | lake), data = output)
# anova(fit_lmer_cpue_1)
# fixef(fit_lmer_cpue_1)
# VarCorr(fit_lmer_cpue_1)
# str(fit_lmer_cpue_1)
# names(fit_lmer_cpue_1)
# show_tests(anova(fit_lmer_cpue_1), fractions = TRUE)

## GLMER.NB -- do not log() y
# fit_glmer.nb_cpue_1 = glmer.nb(cpue_1 ~ ba + type + times_treat + size + ba*type + (1 | lake), data = output, 
#                         glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)))
# summary(fit_glmer.nb_cpue_1)


# # TREATMENT #
# ##### WASHINGTON COUNTY LAKE
# ### 2017 ###
# was17 = read.csv("2.12.19data.csv")
# ### remove seine data 
# was17 = was17[was17$gear != "seine",]
# ### keep only washington county lake 2017 data
# was17 = was17[(was17$lake == "washington county lake" & was17$year == "2017"),]
# ### keep only lmb
# was17 = was17[was17$species == "lmb",]
# ### remove empty levels 
# was17 = droplevels(was17)
# headtail(was17)
# 
# ### mean length-at-age prediction 
# was17$cmgrp = floor(was17$tl / 25) * 25
# was17_count_cmgrp = table(was17$cmgrp)
# was17_count_cmgrp_age = table(was17$cmgrp, was17$age)
# was17_prob_cmgrp_age = prop.table(was17_count_cmgrp_age, margin = 1)
# was17_n_cmgrp_age = as.matrix(was17_prob_cmgrp_age) * as.numeric(was17_count_cmgrp)
# was17_n_age = apply(was17_n_cmgrp_age, 2, sum, na.rm = T)
# was17_lengths = as.numeric(row.names(was17_n_cmgrp_age))
# was17_products = was17_n_cmgrp_age * was17_lengths
# was17_mean_length = apply(was17_products, 2, sum, na.rm = T) / was17_n_age
# 
# ### 2018 ###
# was18 = read.csv("2.12.19data.csv")
# headtail(was18)
# ### remove seine data 
# was18 = was18[was18$gear != "seine",]
# ### keep only washington county lake 2018 data
# was18 = was18[(was18$lake == "washington county lake" & was18$year == "2018"),]
# ### keep only lmb
# was18 = was18[was18$species == "lmb",]
# ### remove empty levels 
# was18 = droplevels(was18)
# headtail(was18)
# 
# ### mean length-at-age prediction 
# was18$cmgrp = floor(was18$tl / 25) * 25
# was18_count_cmgrp = table(was18$cmgrp)
# was18_count_cmgrp_age = table(was18$cmgrp, was18$age)
# was18_prob_cmgrp_age = prop.table(was18_count_cmgrp_age, margin = 1)
# was18_n_cmgrp_age = as.matrix(was18_prob_cmgrp_age) * as.numeric(was18_count_cmgrp)
# was18_n_age = apply(was18_n_cmgrp_age, 2, sum, na.rm = T)
# was18_lengths = as.numeric(row.names(was18_n_cmgrp_age))
# was18_products = was18_n_cmgrp_age * was18_lengths
# was18_mean_length = apply(was18_products, 2, sum, na.rm = T) / was18_n_age
# 
# ##### BARBOUR COUNTY LAKE
# ### 2017 ###
# bar17 = read.csv("2.12.19data.csv")
# headtail(bar17)
# ### remove seine data 
# bar17 = bar17[bar17$gear != "seine",]
# ### keep only barbour county lake 2017 data
# bar17 = bar17[(bar17$lake == "barbour county lake" & bar17$year == "2017"),]
# ### keep only lmb
# bar17 = bar17[bar17$species == "lmb",]
# ### remove empty levels 
# bar17 = droplevels(bar17)
# headtail(bar17)
# 
# ### mean length-at-age prediction 
# bar17$cmgrp = floor(bar17$tl / 25) * 25
# bar17_count_cmgrp = table(bar17$cmgrp)
# bar17_count_cmgrp_age = table(bar17$cmgrp, bar17$age)
# bar17_prob_cmgrp_age = prop.table(bar17_count_cmgrp_age, margin = 1)
# bar17_n_cmgrp_age = as.matrix(bar17_prob_cmgrp_age) * as.numeric(bar17_count_cmgrp)
# bar17_n_age = apply(bar17_n_cmgrp_age, 2, sum, na.rm = T)
# bar17_lengths = as.numeric(row.names(bar17_n_cmgrp_age))
# bar17_products = bar17_n_cmgrp_age * bar17_lengths
# bar17_mean_length = apply(bar17_products, 2, sum, na.rm = T) / bar17_n_age
# 
# ### 2018 ###
# bar18 = read.csv("2.12.19data.csv")
# headtail(bar18)
# ### remove seine data 
# bar18 = bar18[bar18$gear != "seine",]
# ### keep only barbour county lake 2018 data
# bar18 = bar18[(bar18$lake == "barbour county lake" & bar18$year == "2018"),]
# ### keep only lmb
# bar18 = bar18[bar18$species == "lmb",]
# ### remove empty levels 
# bar18 = droplevels(bar18)
# headtail(bar18)
# 
# ### mean length-at-age prediction 
# bar18$cmgrp = floor(bar18$tl / 25) * 25
# bar18_count_cmgrp = table(bar18$cmgrp)
# bar18_count_cmgrp_age = table(bar18$cmgrp, bar18$age)
# bar18_prob_cmgrp_age = prop.table(bar18_count_cmgrp_age, margin = 1)
# bar18_n_cmgrp_age = as.matrix(bar18_prob_cmgrp_age) * as.numeric(bar18_count_cmgrp)
# bar18_n_age = apply(bar18_n_cmgrp_age, 2, sum, na.rm = T)
# bar18_lengths = as.numeric(row.names(bar18_n_cmgrp_age))
# bar18_products = bar18_n_cmgrp_age * bar18_lengths
# bar18_mean_length = apply(bar18_products, 2, sum, na.rm = T) / bar18_n_age
# 
# ##### MONROE COUNTY LAKE
# ### 2017 ###
# mon17 = read.csv("2.12.19data.csv")
# ### remove seine data 
# mon17 = mon17[mon17$gear != "seine",]
# ### keep only monroe county lake 2017 data
# mon17 = mon17[(mon17$lake == "monroe county lake" & mon17$year == "2017"),]
# ### keep only lmb
# mon17 = mon17[mon17$species == "lmb",]
# ### remove empty levels 
# mon17 = droplevels(mon17)
# headtail(mon17)
# 
# ### mean length-at-age prediction 
# mon17$cmgrp = floor(mon17$tl / 25) * 25
# mon17_count_cmgrp = table(mon17$cmgrp)
# mon17_count_cmgrp_age = table(mon17$cmgrp, mon17$age)
# mon17_prob_cmgrp_age = prop.table(mon17_count_cmgrp_age, margin = 1)
# mon17_n_cmgrp_age = as.matrix(mon17_prob_cmgrp_age) * as.numeric(mon17_count_cmgrp)
# mon17_n_age = apply(mon17_n_cmgrp_age, 2, sum, na.rm = T)
# mon17_lengths = as.numeric(row.names(mon17_n_cmgrp_age))
# mon17_products = mon17_n_cmgrp_age * mon17_lengths
# mon17_mean_length = apply(mon17_products, 2, sum, na.rm = T) / mon17_n_age
# 
# ### 2018 ###
# mon18 = read.csv("2.12.19data.csv")
# headtail(mon18)
# ### remove seine data 
# mon18 = mon18[mon18$gear != "seine",]
# ### keep only monroe county lake 2018 data
# mon18 = mon18[(mon18$lake == "monroe county lake" & mon18$year == "2018"),]
# ### keep only lmb
# mon18 = mon18[mon18$species == "lmb",]
# ### remove empty levels 
# mon18 = droplevels(mon18)
# headtail(mon18)
# 
# ### mean length-at-age prediction 
# mon18$cmgrp = floor(mon18$tl / 25) * 25
# mon18_count_cmgrp = table(mon18$cmgrp)
# mon18_count_cmgrp_age = table(mon18$cmgrp, mon18$age)
# mon18_prob_cmgrp_age = prop.table(mon18_count_cmgrp_age, margin = 1)
# mon18_n_cmgrp_age = as.matrix(mon18_prob_cmgrp_age) * as.numeric(mon18_count_cmgrp)
# mon18_n_age = apply(mon18_n_cmgrp_age, 2, sum, na.rm = T)
# mon18_lengths = as.numeric(row.names(mon18_n_cmgrp_age))
# mon18_products = mon18_n_cmgrp_age * mon18_lengths
# mon18_mean_length = apply(mon18_products, 2, sum, na.rm = T) / mon18_n_age
# 
# # ### 2019 ###
# # mon19 = read.csv("2.12.19data.csv")
# # headtail(mon19)
# # ### remove seine data 
# # mon19 = mon19[mon19$gear != "seine",]
# # ### keep only monroe county lake 2019 data
# # mon19 = mon19[(mon19$lake == "monroe county lake" & mon19$year == "2019"),]
# # ### keep only lmb
# # mon19 = mon19[mon19$species == "lmb",]
# # ### remove empty levels 
# # mon19 = droplevels(mon19)
# # headtail(mon19)
# # 
# # ### mean length-at-age prediction 
# # mon19$cmgrp = floor(mon19$tl / 25) * 25
# # mon19_count_cmgrp = table(mon19$cmgrp)
# # mon19_count_cmgrp_age = table(mon19$cmgrp, mon19$age)
# # mon19_prob_cmgrp_age = prop.table(mon19_count_cmgrp_age, margin = 1)
# # mon19_n_cmgrp_age = as.matrix(mon19_prob_cmgrp_age) * as.numeric(mon19_count_cmgrp)
# # mon19_n_age = apply(mon19_n_cmgrp_age, 2, sum, na.rm = T)
# # mon19_lengths = as.numeric(row.names(mon19_n_cmgrp_age))
# # mon19_products = mon19_n_cmgrp_age * mon19_lengths
# # mon19_mean_length = apply(mon19_products, 2, sum, na.rm = T) / mon19_n_age
# 
# ##### LITTLE PIT
# ### 2017 ###
# lit17 = read.csv("2.12.19data.csv")
# ### remove seine data 
# lit17 = lit17[lit17$gear != "seine",]
# ### keep only little pit 2017 data
# lit17 = lit17[(lit17$lake == "little pit" & lit17$year == "2017"),]
# ### keep only lmb
# lit17 = lit17[lit17$species == "lmb",]
# ### remove empty levels 
# lit17 = droplevels(lit17)
# headtail(lit17)
# 
# ### mean length-at-age prediction 
# lit17$cmgrp = floor(lit17$tl / 25) * 25
# lit17_count_cmgrp = table(lit17$cmgrp)
# lit17_count_cmgrp_age = table(lit17$cmgrp, lit17$age)
# lit17_prob_cmgrp_age = prop.table(lit17_count_cmgrp_age, margin = 1)
# lit17_n_cmgrp_age = as.matrix(lit17_prob_cmgrp_age) * as.numeric(lit17_count_cmgrp)
# lit17_n_age = apply(lit17_n_cmgrp_age, 2, sum, na.rm = T)
# lit17_lengths = as.numeric(row.names(lit17_n_cmgrp_age))
# lit17_products = lit17_n_cmgrp_age * lit17_lengths
# lit17_mean_length = apply(lit17_products, 2, sum, na.rm = T) / lit17_n_age
# 
# ### 2018 ###
# lit18 = read.csv("2.12.19data.csv")
# ### remove seine data 
# lit18 = lit18[lit18$gear != "seine",]
# ### keep only little pit 2018 data
# lit18 = lit18[(lit18$lake == "little pit" & lit18$year == "2018"),]
# ### keep only lmb
# lit18 = lit18[lit18$species == "lmb",]
# ### remove empty levels 
# lit18 = droplevels(lit18)
# headtail(lit18)
# 
# ### mean length-at-age prediction 
# lit18$cmgrp = floor(lit18$tl / 25) * 25
# lit18_count_cmgrp = table(lit18$cmgrp)
# lit18_count_cmgrp_age = table(lit18$cmgrp, lit18$age)
# lit18_prob_cmgrp_age = prop.table(lit18_count_cmgrp_age, margin = 1)
# lit18_n_cmgrp_age = as.matrix(lit18_prob_cmgrp_age) * as.numeric(lit18_count_cmgrp)
# lit18_n_age = apply(lit18_n_cmgrp_age, 2, sum, na.rm = T)
# lit18_lengths = as.numeric(row.names(lit18_n_cmgrp_age))
# lit18_products = lit18_n_cmgrp_age * lit18_lengths
# lit18_mean_length = apply(lit18_products, 2, sum, na.rm = T) / lit18_n_age
# 
# # ### 2019 ###
# # lit19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # lit19 = lit19[lit19$gear != "seine",]
# # ### keep only little pit 2019 data
# # lit19 = lit19[(lit19$lake == "little pit" & lit19$year == "2019"),]
# # ### keep only lmb
# # lit19 = lit19[lit19$species == "lmb",]
# # ### remove empty levels 
# # lit19 = droplevels(lit19)
# # headtail(lit19)
# # 
# # ### mean length-at-age prediction 
# # lit19$cmgrp = floor(lit19$tl / 25) * 25
# # lit19_count_cmgrp = table(lit19$cmgrp)
# # lit19_count_cmgrp_age = table(lit19$cmgrp, lit19$age)
# # lit19_prob_cmgrp_age = prop.table(lit19_count_cmgrp_age, margin = 1)
# # lit19_n_cmgrp_age = as.matrix(lit19_prob_cmgrp_age) * as.numeric(lit19_count_cmgrp)
# # lit19_n_age = apply(lit19_n_cmgrp_age, 2, sum, na.rm = T)
# # lit19_lengths = as.numeric(row.names(lit19_n_cmgrp_age))
# # lit19_products = lit19_n_cmgrp_age * lit19_lengths
# # lit19_mean_length = apply(lit19_products, 2, sum, na.rm = T) / lit19_n_age
# 
# ##### HORSESHOE POND
# ### 2017 ###
# hor17 = read.csv("2.12.19data.csv")
# ### remove seine data 
# hor17 = hor17[hor17$gear != "seine",]
# ### keep only horseshoe pond 2017 data
# hor17 = hor17[(hor17$lake == "horseshoe pond" & hor17$year == "2017"),]
# ### keep only lmb
# hor17 = hor17[hor17$species == "lmb",]
# ### remove empty levels 
# hor17 = droplevels(hor17)
# headtail(hor17)
# 
# ### mean length-at-age prediction 
# hor17$cmgrp = floor(hor17$tl / 25) * 25
# hor17_count_cmgrp = table(hor17$cmgrp)
# hor17_count_cmgrp_age = table(hor17$cmgrp, hor17$age)
# hor17_prob_cmgrp_age = prop.table(hor17_count_cmgrp_age, margin = 1)
# hor17_n_cmgrp_age = as.matrix(hor17_prob_cmgrp_age) * as.numeric(hor17_count_cmgrp)
# hor17_n_age = apply(hor17_n_cmgrp_age, 2, sum, na.rm = T)
# hor17_lengths = as.numeric(row.names(hor17_n_cmgrp_age))
# hor17_products = hor17_n_cmgrp_age * hor17_lengths
# hor17_mean_length = apply(hor17_products, 2, sum, na.rm = T) / hor17_n_age
# 
# ### 2018 ###
# hor18 = read.csv("2.12.19data.csv")
# headtail(hor18)
# ### remove seine data 
# hor18 = hor18[hor18$gear != "seine",]
# ### keep only horseshoe pond 2018 data
# hor18 = hor18[(hor18$lake == "horseshoe pond" & hor18$year == "2018"),]
# ### keep only lmb
# hor18 = hor18[hor18$species == "lmb",]
# ### remove empty levels 
# hor18 = droplevels(hor18)
# headtail(hor18)
# 
# ### mean length-at-age prediction 
# hor18$cmgrp = floor(hor18$tl / 25) * 25
# hor18_count_cmgrp = table(hor18$cmgrp)
# hor18_count_cmgrp_age = table(hor18$cmgrp, hor18$age)
# hor18_prob_cmgrp_age = prop.table(hor18_count_cmgrp_age, margin = 1)
# hor18_n_cmgrp_age = as.matrix(hor18_prob_cmgrp_age) * as.numeric(hor18_count_cmgrp)
# hor18_n_age = apply(hor18_n_cmgrp_age, 2, sum, na.rm = T)
# hor18_lengths = as.numeric(row.names(hor18_n_cmgrp_age))
# hor18_products = hor18_n_cmgrp_age * hor18_lengths
# hor18_mean_length = apply(hor18_products, 2, sum, na.rm = T) / hor18_n_age
# 
# # ### 2019 ###
# # hor19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # hor19 = hor19[hor19$gear != "seine",]
# # ### keep only horseshoe pond 2019 data
# # hor19 = hor19[(hor19$lake == "horseshoe pond" & hor19$year == "2019"),]
# # ### keep only lmb
# # hor19 = hor19[hor19$species == "lmb",]
# # ### remove empty levels 
# # hor19 = droplevels(hor19)
# # headtail(hor19)
# # 
# # ### mean length-at-age prediction 
# # hor19$cmgrp = floor(hor19$tl / 25) * 25
# # hor19_count_cmgrp = table(hor19$cmgrp)
# # hor19_count_cmgrp_age = table(hor19$cmgrp, hor19$age)
# # hor19_prob_cmgrp_age = prop.table(hor19_count_cmgrp_age, margin = 1)
# # hor19_n_cmgrp_age = as.matrix(hor19_prob_cmgrp_age) * as.numeric(hor19_count_cmgrp)
# # hor19_n_age = apply(hor19_n_cmgrp_age, 2, sum, na.rm = T)
# # hor19_lengths = as.numeric(row.names(hor19_n_cmgrp_age))
# # hor19_products = hor19_n_cmgrp_age * hor19_lengths
# # hor19_mean_length = apply(hor19_products, 2, sum, na.rm = T) / hor19_n_age
# 
# ##### S3
# ### 2017 ###
# s317 = read.csv("2.12.19data.csv")
# ### remove seine data 
# s317 = s317[s317$gear != "seine",]
# ### keep only s3 2017 data
# s317 = s317[(s317$lake == "s3" & s317$year == "2017"),]
# ### keep only lmb
# s317 = s317[s317$species == "lmb",]
# ### remove empty levels 
# s317 = droplevels(s317)
# headtail(s317)
# 
# ### mean length-at-age prediction 
# s317$cmgrp = floor(s317$tl / 25) * 25
# s317_count_cmgrp = table(s317$cmgrp)
# s317_count_cmgrp_age = table(s317$cmgrp, s317$age)
# s317_prob_cmgrp_age = prop.table(s317_count_cmgrp_age, margin = 1)
# s317_n_cmgrp_age = as.matrix(s317_prob_cmgrp_age) * as.numeric(s317_count_cmgrp)
# s317_n_age = apply(s317_n_cmgrp_age, 2, sum, na.rm = T)
# s317_lengths = as.numeric(row.names(s317_n_cmgrp_age))
# s317_products = s317_n_cmgrp_age * s317_lengths
# s317_mean_length = apply(s317_products, 2, sum, na.rm = T) / s317_n_age
# 
# ### 2018 ###
# s318 = read.csv("2.12.19data.csv")
# headtail(s318)
# ### remove seine data 
# s318 = s318[s318$gear != "seine",]
# ### keep only s3 2018 data
# s318 = s318[(s318$lake == "s3" & s318$year == "2018"),]
# ### keep only lmb
# s318 = s318[s318$species == "lmb",]
# ### remove empty levels 
# s318 = droplevels(s318)
# headtail(s318)
# 
# ### mean length-at-age prediction 
# s318$cmgrp = floor(s318$tl / 25) * 25
# s318_count_cmgrp = table(s318$cmgrp)
# s318_count_cmgrp_age = table(s318$cmgrp, s318$age)
# s318_prob_cmgrp_age = prop.table(s318_count_cmgrp_age, margin = 1)
# s318_n_cmgrp_age = as.matrix(s318_prob_cmgrp_age) * as.numeric(s318_count_cmgrp)
# s318_n_age = apply(s318_n_cmgrp_age, 2, sum, na.rm = T)
# s318_lengths = as.numeric(row.names(s318_n_cmgrp_age))
# s318_products = s318_n_cmgrp_age * s318_lengths
# s318_mean_length = apply(s318_products, 2, sum, na.rm = T) / s318_n_age
# 
# # ### 2019 ###
# # s319 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # s319 = s319[s319$gear != "seine",]
# # ### keep only s3 2019 data
# # s319 = s319[(s319$lake == "s3" & s319$year == "2019"),]
# # ### keep only lmb
# # s319 = s319[s319$species == "lmb",]
# # ### remove empty levels 
# # s319 = droplevels(s319)
# # headtail(s319)
# # 
# # ### mean length-at-age prediction 
# # s319$cmgrp = floor(s319$tl / 25) * 25
# # s319_count_cmgrp = table(s319$cmgrp)
# # s319_count_cmgrp_age = table(s319$cmgrp, s319$age)
# # s319_prob_cmgrp_age = prop.table(s319_count_cmgrp_age, margin = 1)
# # s319_n_cmgrp_age = as.matrix(s319_prob_cmgrp_age) * as.numeric(s319_count_cmgrp)
# # s319_n_age = apply(s319_n_cmgrp_age, 2, sum, na.rm = T)
# # s319_lengths = as.numeric(row.names(s319_n_cmgrp_age))
# # s319_products = s319_n_cmgrp_age * s319_lengths
# # s319_mean_length = apply(s319_products, 2, sum, na.rm = T) / s319_n_age
# 
# ##### DRUMMOND 1
# # ### 2016 ###
# # dru116 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # dru116 = dru116[dru116$gear != "seine",]
# # ### keep only drummond 1 2016 data -- do not have 2017 data
# # dru116 = dru116[(dru116$lake == "drummond1" & dru116$year == "2016"),]
# # ### keep only lmb
# # dru116 = dru116[dru116$species == "lmb",]
# # ### remove empty levels 
# # dru116 = droplevels(dru116)
# # headtail(dru116)
# # 
# # ### mean length-at-age prediction 
# # dru116$cmgrp = floor(dru116$tl / 25) * 25
# # dru116_count_cmgrp = table(dru116$cmgrp)
# # dru116_count_cmgrp_age = table(dru116$cmgrp, dru116$age)
# # dru116_prob_cmgrp_age = prop.table(dru116_count_cmgrp_age, margin = 1)
# # dru116_n_cmgrp_age = as.matrix(dru116_prob_cmgrp_age) * as.numeric(dru116_count_cmgrp)
# # dru116_n_age = apply(dru116_n_cmgrp_age, 2, sum, na.rm = T)
# # dru116_lengths = as.numeric(row.names(dru116_n_cmgrp_age))
# # dru116_products = dru116_n_cmgrp_age * dru116_lengths
# # dru116_mean_length = apply(dru116_products, 2, sum, na.rm = T) / dru116_n_age
# 
# ### 2018 ###
# dru118 = read.csv("2.12.19data.csv")
# headtail(dru118)
# ### remove seine data 
# dru118 = dru118[dru118$gear != "seine",]
# ### keep only drummond 1 2018 data
# dru118 = dru118[(dru118$lake == "drummond1" & dru118$year == "2018"),]
# ### keep only lmb
# dru118 = dru118[dru118$species == "lmb",]
# ### remove empty levels 
# dru118 = droplevels(dru118)
# headtail(dru118)
# 
# ### mean length-at-age prediction 
# dru118$cmgrp = floor(dru118$tl / 25) * 25
# dru118_count_cmgrp = table(dru118$cmgrp)
# dru118_count_cmgrp_age = table(dru118$cmgrp, dru118$age)
# dru118_prob_cmgrp_age = prop.table(dru118_count_cmgrp_age, margin = 1)
# dru118_n_cmgrp_age = as.matrix(dru118_prob_cmgrp_age) * as.numeric(dru118_count_cmgrp)
# dru118_n_age = apply(dru118_n_cmgrp_age, 2, sum, na.rm = T)
# dru118_lengths = as.numeric(row.names(dru118_n_cmgrp_age))
# dru118_products = dru118_n_cmgrp_age * dru118_lengths
# dru118_mean_length = apply(dru118_products, 2, sum, na.rm = T) / dru118_n_age
# 
# # ### 2019 ###
# # dru119 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # dru119 = dru119[dru119$gear != "seine",]
# # ### keep only drummond 1 2019 data
# # dru119 = dru119[(dru119$lake == "drummond1" & dru119$year == "2019"),]
# # ### keep only lmb
# # dru119 = dru119[dru119$species == "lmb",]
# # ### remove empty levels 
# # dru119 = droplevels(dru119)
# # headtail(dru119)
# # 
# # ### mean length-at-age prediction 
# # dru119$cmgrp = floor(dru119$tl / 25) * 25
# # dru119_count_cmgrp = table(dru119$cmgrp)
# # dru119_count_cmgrp_age = table(dru119$cmgrp, dru119$age)
# # dru119_prob_cmgrp_age = prop.table(dru119_count_cmgrp_age, margin = 1)
# # dru119_n_cmgrp_age = as.matrix(dru119_prob_cmgrp_age) * as.numeric(dru119_count_cmgrp)
# # dru119_n_age = apply(dru119_n_cmgrp_age, 2, sum, na.rm = T)
# # dru119_lengths = as.numeric(row.names(dru119_n_cmgrp_age))
# # dru119_products = dru119_n_cmgrp_age * dru119_lengths
# # dru119_mean_length = apply(dru119_products, 2, sum, na.rm = T) / dru119_n_age
# 
# ##### BRITTON POND
# ### 2018 ###
# bri18 = read.csv("2.12.19data.csv")
# headtail(bri18)
# ### remove seine data 
# bri18 = bri18[bri18$gear != "seine",]
# ### keep only britton 2018 data
# bri18 = bri18[(bri18$lake == "britton" & bri18$year == "2018"),]
# ### keep only lmb
# bri18 = bri18[bri18$species == "lmb",]
# ### remove empty levels 
# bri18 = droplevels(bri18)
# headtail(bri18)
# 
# ### mean length-at-age prediction 
# bri18$cmgrp = floor(bri18$tl / 25) * 25
# bri18_count_cmgrp = table(bri18$cmgrp)
# bri18_count_cmgrp_age = table(bri18$cmgrp, bri18$age)
# bri18_prob_cmgrp_age = prop.table(bri18_count_cmgrp_age, margin = 1)
# bri18_n_cmgrp_age = as.matrix(bri18_prob_cmgrp_age) * as.numeric(bri18_count_cmgrp)
# bri18_n_age = apply(bri18_n_cmgrp_age, 2, sum, na.rm = T)
# bri18_lengths = as.numeric(row.names(bri18_n_cmgrp_age))
# bri18_products = bri18_n_cmgrp_age * bri18_lengths
# bri18_mean_length = apply(bri18_products, 2, sum, na.rm = T) / bri18_n_age
# 
# # ### 2019 ###
# # bri19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # bri19 = bri19[bri19$gear != "seine",]
# # ### keep only britton 2019 data
# # bri19 = bri19[(bri19$lake == "britton" & bri19$year == "2019"),]
# # ### keep only lmb
# # bri19 = bri19[bri19$species == "lmb",]
# # ### remove empty levels 
# # bri19 = droplevels(bri19)
# # headtail(bri19)
# # 
# # ### mean length-at-age prediction 
# # bri19$cmgrp = floor(bri19$tl / 25) * 25
# # bri19_count_cmgrp = table(bri19$cmgrp)
# # bri19_count_cmgrp_age = table(bri19$cmgrp, bri19$age)
# # bri19_prob_cmgrp_age = prop.table(bri19_count_cmgrp_age, margin = 1)
# # bri19_n_cmgrp_age = as.matrix(bri19_prob_cmgrp_age) * as.numeric(bri19_count_cmgrp)
# # bri19_n_age = apply(bri19_n_cmgrp_age, 2, sum, na.rm = T)
# # bri19_lengths = as.numeric(row.names(bri19_n_cmgrp_age))
# # bri19_products = bri19_n_cmgrp_age * bri19_lengths
# # bri19_mean_length = apply(bri19_products, 2, sum, na.rm = T) / bri19_n_age
# 
# ##### DEAD LAKE
# ### 2018 ###
# dea18 = read.csv("2.12.19data.csv")
# headtail(dea18)
# ### remove seine data 
# dea18 = dea18[dea18$gear != "seine",]
# ### keep only dead lake 2018 data
# dea18 = dea18[(dea18$lake == "dead" & dea18$year == "2018"),]
# ### keep only lmb
# dea18 = dea18[dea18$species == "lmb",]
# ### remove empty levels 
# dea18 = droplevels(dea18)
# headtail(dea18)
# 
# ### mean length-at-age prediction 
# dea18$cmgrp = floor(dea18$tl / 25) * 25
# dea18_count_cmgrp = table(dea18$cmgrp)
# dea18_count_cmgrp_age = table(dea18$cmgrp, dea18$age)
# dea18_prob_cmgrp_age = prop.table(dea18_count_cmgrp_age, margin = 1)
# dea18_n_cmgrp_age = as.matrix(dea18_prob_cmgrp_age) * as.numeric(dea18_count_cmgrp)
# dea18_n_age = apply(dea18_n_cmgrp_age, 2, sum, na.rm = T)
# dea18_lengths = as.numeric(row.names(dea18_n_cmgrp_age))
# dea18_products = dea18_n_cmgrp_age * dea18_lengths
# dea18_mean_length = apply(dea18_products, 2, sum, na.rm = T) / dea18_n_age
# 
# # ### 2019 ###
# # dea19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # dea19 = dea19[dea19$gear != "seine",]
# # ### keep only dead lake 2019 data
# # dea19 = dea19[(dea19$lake == "dead" & dea19$year == "2019"),]
# # ### keep only lmb
# # dea19 = dea19[dea19$species == "lmb",]
# # ### remove empty levels 
# # dea19 = droplevels(dea19)
# # headtail(dea19)
# # 
# # ### mean length-at-age prediction 
# # dea19$cmgrp = floor(dea19$tl / 25) * 25
# # dea19_count_cmgrp = table(dea19$cmgrp)
# # dea19_count_cmgrp_age = table(dea19$cmgrp, dea19$age)
# # dea19_prob_cmgrp_age = prop.table(dea19_count_cmgrp_age, margin = 1)
# # dea19_n_cmgrp_age = as.matrix(dea19_prob_cmgrp_age) * as.numeric(dea19_count_cmgrp)
# # dea19_n_age = apply(dea19_n_cmgrp_age, 2, sum, na.rm = T)
# # dea19_lengths = as.numeric(row.names(dea19_n_cmgrp_age))
# # dea19_products = dea19_n_cmgrp_age * dea19_lengths
# # dea19_mean_length = apply(dea19_products, 2, sum, na.rm = T) / dea19_n_age
# 
# ##### GRIGGS POND 2
# ### 2018 ###
# gri18 = read.csv("2.12.19data.csv")
# headtail(gri18)
# ### remove seine data 
# gri18 = gri18[gri18$gear != "seine",]
# ### keep only griggs 2 2018 data
# gri18 = gri18[(gri18$lake == "griggs2" & gri18$year == "2018"),]
# ### keep only lmb
# gri18 = gri18[gri18$species == "lmb",]
# ### remove empty levels 
# gri18 = droplevels(gri18)
# headtail(gri18)
# 
# ### mean length-at-age prediction 
# gri18$cmgrp = floor(gri18$tl / 25) * 25
# gri18_count_cmgrp = table(gri18$cmgrp)
# gri18_count_cmgrp_age = table(gri18$cmgrp, gri18$age)
# gri18_prob_cmgrp_age = prop.table(gri18_count_cmgrp_age, margin = 1)
# gri18_n_cmgrp_age = as.matrix(gri18_prob_cmgrp_age) * as.numeric(gri18_count_cmgrp)
# gri18_n_age = apply(gri18_n_cmgrp_age, 2, sum, na.rm = T)
# gri18_lengths = as.numeric(row.names(gri18_n_cmgrp_age))
# gri18_products = gri18_n_cmgrp_age * gri18_lengths
# gri18_mean_length = apply(gri18_products, 2, sum, na.rm = T) / gri18_n_age
# 
# # ### 2019 ###
# # gri19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # gri19 = gri19[gri19$gear != "seine",]
# # ### keep only griggs 2 2019 data
# # gri19 = gri19[(gri19$lake == "griggs2" & gri19$year == "2019"),]
# # ### keep only lmb
# # gri19 = gri19[gri19$species == "lmb",]
# # ### remove empty levels 
# # gri19 = droplevels(gri19)
# # headtail(gri19)
# # 
# # ### mean length-at-age prediction 
# # gri19$cmgrp = floor(gri19$tl / 25) * 25
# # gri19_count_cmgrp = table(gri19$cmgrp)
# # gri19_count_cmgrp_age = table(gri19$cmgrp, gri19$age)
# # gri19_prob_cmgrp_age = prop.table(gri19_count_cmgrp_age, margin = 1)
# # gri19_n_cmgrp_age = as.matrix(gri19_prob_cmgrp_age) * as.numeric(gri19_count_cmgrp)
# # gri19_n_age = apply(gri19_n_cmgrp_age, 2, sum, na.rm = T)
# # gri19_lengths = as.numeric(row.names(gri19_n_cmgrp_age))
# # gri19_products = gri19_n_cmgrp_age * gri19_lengths
# # gri19_mean_length = apply(gri19_products, 2, sum, na.rm = T) / gri19_n_age
# 
# ##### CONTROL
# ##### ANDERSON
# ### 2017 ###
# and17 = read.csv("2.12.19data.csv")
# ### remove seine data 
# and17 = and17[and17$gear != "seine",]
# ### keep only anderson 2017 data
# and17 = and17[(and17$lake == "anderson county lake" & and17$year == "2017"),]
# ### keep only lmb
# and17 = and17[and17$species == "lmb",]
# ### remove empty levels 
# and17 = droplevels(and17)
# headtail(and17)
# 
# ### mean length-at-age prediction 
# and17$cmgrp = floor(and17$tl / 25) * 25
# and17_count_cmgrp = table(and17$cmgrp)
# and17_count_cmgrp_age = table(and17$cmgrp, and17$age)
# and17_prob_cmgrp_age = prop.table(and17_count_cmgrp_age, margin = 1)
# and17_n_cmgrp_age = as.matrix(and17_prob_cmgrp_age) * as.numeric(and17_count_cmgrp)
# and17_n_age = apply(and17_n_cmgrp_age, 2, sum, na.rm = T)
# and17_lengths = as.numeric(row.names(and17_n_cmgrp_age))
# and17_products = and17_n_cmgrp_age * and17_lengths
# and17_mean_length = apply(and17_products, 2, sum, na.rm = T) / and17_n_age
# 
# ### 2018 ###
# and18 = read.csv("2.12.19data.csv")
# headtail(and18)
# ### remove seine data 
# and18 = and18[and18$gear != "seine",]
# ### keep only anderson 2018 data
# and18 = and18[(and18$lake == "anderson county lake" & and18$year == "2018"),]
# ### keep only lmb
# and18 = and18[and18$species == "lmb",]
# ### remove empty levels 
# and18 = droplevels(and18)
# headtail(and18)
# 
# ### mean length-at-age prediction 
# and18$cmgrp = floor(and18$tl / 25) * 25
# and18_count_cmgrp = table(and18$cmgrp)
# and18_count_cmgrp_age = table(and18$cmgrp, and18$age)
# and18_prob_cmgrp_age = prop.table(and18_count_cmgrp_age, margin = 1)
# and18_n_cmgrp_age = as.matrix(and18_prob_cmgrp_age) * as.numeric(and18_count_cmgrp)
# and18_n_age = apply(and18_n_cmgrp_age, 2, sum, na.rm = T)
# and18_lengths = as.numeric(row.names(and18_n_cmgrp_age))
# and18_products = and18_n_cmgrp_age * and18_lengths
# and18_mean_length = apply(and18_products, 2, sum, na.rm = T) / and18_n_age
# 
# ##### LEE COUNTY LAKE
# ### 2017 ###
# lee17 = read.csv("2.12.19data.csv")
# ### remove seine data 
# lee17 = lee17[lee17$gear != "seine",]
# ### keep only lee county lake 2017 data
# lee17 = lee17[(lee17$lake == "lee county lake" & lee17$year == "2017"),]
# ### keep only lmb
# lee17 = lee17[lee17$species == "lmb",]
# ### remove empty levels 
# lee17 = droplevels(lee17)
# headtail(lee17)
# 
# ### mean length-at-age prediction 
# lee17$cmgrp = floor(lee17$tl / 25) * 25
# lee17_count_cmgrp = table(lee17$cmgrp)
# lee17_count_cmgrp_age = table(lee17$cmgrp, lee17$age)
# lee17_prob_cmgrp_age = prop.table(lee17_count_cmgrp_age, margin = 1)
# lee17_n_cmgrp_age = as.matrix(lee17_prob_cmgrp_age) * as.numeric(lee17_count_cmgrp)
# lee17_n_age = apply(lee17_n_cmgrp_age, 2, sum, na.rm = T)
# lee17_lengths = as.numeric(row.names(lee17_n_cmgrp_age))
# lee17_products = lee17_n_cmgrp_age * lee17_lengths
# lee17_mean_length = apply(lee17_products, 2, sum, na.rm = T) / lee17_n_age
# 
# ### 2018 ###
# lee18 = read.csv("2.12.19data.csv")
# headtail(lee18)
# ### remove seine data 
# lee18 = lee18[lee18$gear != "seine",]
# ### keep only lee county lake 2018 data
# lee18 = lee18[(lee18$lake == "lee county lake" & lee18$year == "2018"),]
# ### keep only lmb
# lee18 = lee18[lee18$species == "lmb",]
# ### remove empty levels 
# lee18 = droplevels(lee18)
# headtail(lee18)
# 
# ### mean length-at-age prediction 
# lee18$cmgrp = floor(lee18$tl / 25) * 25
# lee18_count_cmgrp = table(lee18$cmgrp)
# lee18_count_cmgrp_age = table(lee18$cmgrp, lee18$age)
# lee18_prob_cmgrp_age = prop.table(lee18_count_cmgrp_age, margin = 1)
# lee18_n_cmgrp_age = as.matrix(lee18_prob_cmgrp_age) * as.numeric(lee18_count_cmgrp)
# lee18_n_age = apply(lee18_n_cmgrp_age, 2, sum, na.rm = T)
# lee18_lengths = as.numeric(row.names(lee18_n_cmgrp_age))
# lee18_products = lee18_n_cmgrp_age * lee18_lengths
# lee18_mean_length = apply(lee18_products, 2, sum, na.rm = T) / lee18_n_age
# 
# ##### DALE COUNTY LAKE
# ### 2017 ###
# dal17 = read.csv("2.12.19data.csv")
# ### remove seine data 
# dal17 = dal17[dal17$gear != "seine",]
# ### keep only dale county lake 2017 data
# dal17 = dal17[(dal17$lake == "dale county lake" & dal17$year == "2017"),]
# ### keep only lmb
# dal17 = dal17[dal17$species == "lmb",]
# ### remove empty levels 
# dal17 = droplevels(dal17)
# headtail(dal17)
# 
# ### mean length-at-age prediction 
# dal17$cmgrp = floor(dal17$tl / 25) * 25
# dal17_count_cmgrp = table(dal17$cmgrp)
# dal17_count_cmgrp_age = table(dal17$cmgrp, dal17$age)
# dal17_prob_cmgrp_age = prop.table(dal17_count_cmgrp_age, margin = 1)
# dal17_n_cmgrp_age = as.matrix(dal17_prob_cmgrp_age) * as.numeric(dal17_count_cmgrp)
# dal17_n_age = apply(dal17_n_cmgrp_age, 2, sum, na.rm = T)
# dal17_lengths = as.numeric(row.names(dal17_n_cmgrp_age))
# dal17_products = dal17_n_cmgrp_age * dal17_lengths
# dal17_mean_length = apply(dal17_products, 2, sum, na.rm = T) / dal17_n_age
# 
# ### 2018 ###
# dal18 = read.csv("2.12.19data.csv")
# headtail(dal18)
# ### remove seine data 
# dal18 = dal18[dal18$gear != "seine",]
# ### keep only dale county lake 2018 data
# dal18 = dal18[(dal18$lake == "dale county lake" & dal18$year == "2018"),]
# ### keep only lmb
# dal18 = dal18[dal18$species == "lmb",]
# ### remove empty levels 
# dal18 = droplevels(dal18)
# headtail(dal18)
# 
# ### mean length-at-age prediction 
# dal18$cmgrp = floor(dal18$tl / 25) * 25
# dal18_count_cmgrp = table(dal18$cmgrp)
# dal18_count_cmgrp_age = table(dal18$cmgrp, dal18$age)
# dal18_prob_cmgrp_age = prop.table(dal18_count_cmgrp_age, margin = 1)
# dal18_n_cmgrp_age = as.matrix(dal18_prob_cmgrp_age) * as.numeric(dal18_count_cmgrp)
# dal18_n_age = apply(dal18_n_cmgrp_age, 2, sum, na.rm = T)
# dal18_lengths = as.numeric(row.names(dal18_n_cmgrp_age))
# dal18_products = dal18_n_cmgrp_age * dal18_lengths
# dal18_mean_length = apply(dal18_products, 2, sum, na.rm = T) / dal18_n_age
# 
# # ### 2019 ###
# # dal19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # dal19 = dal19[dal19$gear != "seine",]
# # ### keep only dale county lake 2019 data
# # dal19 = dal19[(dal19$lake == "dale county lake" & dal19$year == "2019"),]
# # ### keep only lmb
# # dal19 = dal19[dal19$species == "lmb",]
# # ### remove empty levels 
# # dal19 = droplevels(dal19)
# # headtail(dal19)
# # 
# # ### mean length-at-age prediction 
# # dal19$cmgrp = floor(dal19$tl / 25) * 25
# # dal19_count_cmgrp = table(dal19$cmgrp)
# # dal19_count_cmgrp_age = table(dal19$cmgrp, dal19$age)
# # dal19_prob_cmgrp_age = prop.table(dal19_count_cmgrp_age, margin = 1)
# # dal19_n_cmgrp_age = as.matrix(dal19_prob_cmgrp_age) * as.numeric(dal19_count_cmgrp)
# # dal19_n_age = apply(dal19_n_cmgrp_age, 2, sum, na.rm = T)
# # dal19_lengths = as.numeric(row.names(dal19_n_cmgrp_age))
# # dal19_products = dal19_n_cmgrp_age * dal19_lengths
# # dal19_mean_length = apply(dal19_products, 2, sum, na.rm = T) / dal19_n_age
# 
# ##### BIG PIT
# ### 2017 ###
# big17 = read.csv("2.12.19data.csv")
# ### remove seine data 
# big17 = big17[big17$gear != "seine",]
# ### keep only big pit 2017 data
# big17 = big17[(big17$lake == "big pit" & big17$year == "2017"),]
# ### keep only lmb
# big17 = big17[big17$species == "lmb",]
# ### remove empty levels 
# big17 = droplevels(big17)
# headtail(big17)
# 
# ### mean length-at-age prediction 
# big17$cmgrp = floor(big17$tl / 25) * 25
# big17_count_cmgrp = table(big17$cmgrp)
# big17_count_cmgrp_age = table(big17$cmgrp, big17$age)
# big17_prob_cmgrp_age = prop.table(big17_count_cmgrp_age, margin = 1)
# big17_n_cmgrp_age = as.matrix(big17_prob_cmgrp_age) * as.numeric(big17_count_cmgrp)
# big17_n_age = apply(big17_n_cmgrp_age, 2, sum, na.rm = T)
# big17_lengths = as.numeric(row.names(big17_n_cmgrp_age))
# big17_products = big17_n_cmgrp_age * big17_lengths
# big17_mean_length = apply(big17_products, 2, sum, na.rm = T) / big17_n_age
# 
# ### 2018 ###
# big18 = read.csv("2.12.19data.csv")
# headtail(big18)
# ### remove seine data 
# big18 = big18[big18$gear != "seine",]
# ### keep only big pit 2018 data
# big18 = big18[(big18$lake == "big pit" & big18$year == "2018"),]
# ### keep only lmb
# big18 = big18[big18$species == "lmb",]
# ### remove empty levels 
# big18 = droplevels(big18)
# headtail(big18)
# 
# ### mean length-at-age prediction 
# big18$cmgrp = floor(big18$tl / 25) * 25
# big18_count_cmgrp = table(big18$cmgrp)
# big18_count_cmgrp_age = table(big18$cmgrp, big18$age)
# big18_prob_cmgrp_age = prop.table(big18_count_cmgrp_age, margin = 1)
# big18_n_cmgrp_age = as.matrix(big18_prob_cmgrp_age) * as.numeric(big18_count_cmgrp)
# big18_n_age = apply(big18_n_cmgrp_age, 2, sum, na.rm = T)
# big18_lengths = as.numeric(row.names(big18_n_cmgrp_age))
# big18_products = big18_n_cmgrp_age * big18_lengths
# big18_mean_length = apply(big18_products, 2, sum, na.rm = T) / big18_n_age
# 
# # ### 2019 ###
# # big19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # big19 = big19[big19$gear != "seine",]
# # ### keep only big pit 2019 data
# # big19 = big19[(big19$lake == "big pit" & big19$year == "2019"),]
# # ### keep only lmb
# # big19 = big19[big19$species == "lmb",]
# # ### remove empty levels 
# # big19 = droplevels(big19)
# # headtail(big19)
# # 
# # ### mean length-at-age prediction 
# # big19$cmgrp = floor(big19$tl / 25) * 25
# # big19_count_cmgrp = table(big19$cmgrp)
# # big19_count_cmgrp_age = table(big19$cmgrp, big19$age)
# # big19_prob_cmgrp_age = prop.table(big19_count_cmgrp_age, margin = 1)
# # big19_n_cmgrp_age = as.matrix(big19_prob_cmgrp_age) * as.numeric(big19_count_cmgrp)
# # big19_n_age = apply(big19_n_cmgrp_age, 2, sum, na.rm = T)
# # big19_lengths = as.numeric(row.names(big19_n_cmgrp_age))
# # big19_products = big19_n_cmgrp_age * big19_lengths
# # big19_mean_length = apply(big19_products, 2, sum, na.rm = T) / big19_n_age
# 
# ##### FP3
# ### 2017 ###
# fp317 = read.csv("2.12.19data.csv")
# ### remove seine data 
# fp317 = fp317[fp317$gear != "seine",]
# ### keep only fp3 2017 data
# fp317 = fp317[(fp317$lake == "fp3" & fp317$year == "2017"),]
# ### keep only lmb
# fp317 = fp317[fp317$species == "lmb",]
# ### remove empty levels 
# fp317 = droplevels(fp317)
# headtail(fp317)
# 
# ### mean length-at-age prediction 
# fp317$cmgrp = floor(fp317$tl / 25) * 25
# fp317_count_cmgrp = table(fp317$cmgrp)
# fp317_count_cmgrp_age = table(fp317$cmgrp, fp317$age)
# fp317_prob_cmgrp_age = prop.table(fp317_count_cmgrp_age, margin = 1)
# fp317_n_cmgrp_age = as.matrix(fp317_prob_cmgrp_age) * as.numeric(fp317_count_cmgrp)
# fp317_n_age = apply(fp317_n_cmgrp_age, 2, sum, na.rm = T)
# fp317_lengths = as.numeric(row.names(fp317_n_cmgrp_age))
# fp317_products = fp317_n_cmgrp_age * fp317_lengths
# fp317_mean_length = apply(fp317_products, 2, sum, na.rm = T) / fp317_n_age
# 
# ### 2018 ###
# fp318 = read.csv("2.12.19data.csv")
# ### remove seine data 
# fp318 = fp318[fp318$gear != "seine",]
# ### keep only fp3 2018 data
# fp318 = fp318[(fp318$lake == "fp3" & fp318$year == "2018"),]
# ### keep only lmb
# fp318 = fp318[fp318$species == "lmb",]
# ### remove empty levels 
# fp318 = droplevels(fp318)
# headtail(fp318)
# 
# ### mean length-at-age prediction 
# fp318$cmgrp = floor(fp318$tl / 25) * 25
# fp318_count_cmgrp = table(fp318$cmgrp)
# fp318_count_cmgrp_age = table(fp318$cmgrp, fp318$age)
# fp318_prob_cmgrp_age = prop.table(fp318_count_cmgrp_age, margin = 1)
# fp318_n_cmgrp_age = as.matrix(fp318_prob_cmgrp_age) * as.numeric(fp318_count_cmgrp)
# fp318_n_age = apply(fp318_n_cmgrp_age, 2, sum, na.rm = T)
# fp318_lengths = as.numeric(row.names(fp318_n_cmgrp_age))
# fp318_products = fp318_n_cmgrp_age * fp318_lengths
# fp318_mean_length = apply(fp318_products, 2, sum, na.rm = T) / fp318_n_age
# 
# # ### 2019 ###
# # fp319 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # fp319 = fp319[fp319$gear != "seine",]
# # ### keep only fp3 2019 data
# # fp319 = fp319[(fp319$lake == "fp3" & fp319$year == "2019"),]
# # ### keep only lmb
# # fp319 = fp319[fp319$species == "lmb",]
# # ### remove empty levels 
# # fp319 = droplevels(fp319)
# # headtail(fp319)
# # 
# # ### mean length-at-age prediction 
# # fp319$cmgrp = floor(fp319$tl / 25) * 25
# # fp319_count_cmgrp = table(fp319$cmgrp)
# # fp319_count_cmgrp_age = table(fp319$cmgrp, fp319$age)
# # fp319_prob_cmgrp_age = prop.table(fp319_count_cmgrp_age, margin = 1)
# # fp319_n_cmgrp_age = as.matrix(fp319_prob_cmgrp_age) * as.numeric(fp319_count_cmgrp)
# # fp319_n_age = apply(fp319_n_cmgrp_age, 2, sum, na.rm = T)
# # fp319_lengths = as.numeric(row.names(fp319_n_cmgrp_age))
# # fp319_products = fp319_n_cmgrp_age * fp319_lengths
# # fp319_mean_length = apply(fp319_products, 2, sum, na.rm = T) / fp319_n_age
# 
# ##### AE1
# ### 2017 ###
# ae117 = read.csv("2.12.19data.csv")
# ### remove seine data 
# ae117 = ae117[ae117$gear != "seine",]
# ### keep only AE1 2017 data
# ae117 = ae117[(ae117$lake == "ae1" & ae117$year == "2017"),]
# ### keep only lmb
# ae117 = ae117[ae117$species == "lmb",]
# ### remove empty levels 
# ae117 = droplevels(ae117)
# headtail(ae117)
# 
# ### mean length-at-age prediction 
# ae117$cmgrp = floor(ae117$tl / 25) * 25
# ae117_count_cmgrp = table(ae117$cmgrp)
# ae117_count_cmgrp_age = table(ae117$cmgrp, ae117$age)
# ae117_prob_cmgrp_age = prop.table(ae117_count_cmgrp_age, margin = 1)
# ae117_n_cmgrp_age = as.matrix(ae117_prob_cmgrp_age) * as.numeric(ae117_count_cmgrp)
# ae117_n_age = apply(ae117_n_cmgrp_age, 2, sum, na.rm = T)
# ae117_lengths = as.numeric(row.names(ae117_n_cmgrp_age))
# ae117_products = ae117_n_cmgrp_age * ae117_lengths
# ae117_mean_length = apply(ae117_products, 2, sum, na.rm = T) / ae117_n_age
# 
# ### 2018 ###
# ae118 = read.csv("2.12.19data.csv")
# headtail(ae118)
# ### remove seine data 
# ae118 = ae118[ae118$gear != "seine",]
# ### keep only ae1 2018 data
# ae118 = ae118[(ae118$lake == "ae1" & ae118$year == "2018"),]
# ### keep only lmb
# ae118 = ae118[ae118$species == "lmb",]
# ### remove empty levels 
# ae118 = droplevels(ae118)
# headtail(ae118)
# 
# ### mean length-at-age prediction 
# ae118$cmgrp = floor(ae118$tl / 25) * 25
# ae118_count_cmgrp = table(ae118$cmgrp)
# ae118_count_cmgrp_age = table(ae118$cmgrp, ae118$age)
# ae118_prob_cmgrp_age = prop.table(ae118_count_cmgrp_age, margin = 1)
# ae118_n_cmgrp_age = as.matrix(ae118_prob_cmgrp_age) * as.numeric(ae118_count_cmgrp)
# ae118_n_age = apply(ae118_n_cmgrp_age, 2, sum, na.rm = T)
# ae118_lengths = as.numeric(row.names(ae118_n_cmgrp_age))
# ae118_products = ae118_n_cmgrp_age * ae118_lengths
# ae118_mean_length = apply(ae118_products, 2, sum, na.rm = T) / ae118_n_age
# 
# # ### 2019 ###
# # ae119 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # ae119 = ae119[ae119$gear != "seine",]
# # ### keep only ae1 2019 data
# # ae119 = ae119[(ae119$lake == "ae1" & ae119$year == "2019"),]
# # ### keep only lmb
# # ae119 = ae119[ae119$species == "lmb",]
# # ### remove empty levels 
# # ae119 = droplevels(ae119)
# # headtail(ae119)
# # 
# # ### mean length-at-age prediction 
# # ae119$cmgrp = floor(ae119$tl / 25) * 25
# # ae119_count_cmgrp = table(ae119$cmgrp)
# # ae119_count_cmgrp_age = table(ae119$cmgrp, ae119$age)
# # ae119_prob_cmgrp_age = prop.table(ae119_count_cmgrp_age, margin = 1)
# # ae119_n_cmgrp_age = as.matrix(ae119_prob_cmgrp_age) * as.numeric(ae119_count_cmgrp)
# # ae119_n_age = apply(ae119_n_cmgrp_age, 2, sum, na.rm = T)
# # ae119_lengths = as.numeric(row.names(ae119_n_cmgrp_age))
# # ae119_products = ae119_n_cmgrp_age * ae119_lengths
# # ae119_mean_length = apply(ae119_products, 2, sum, na.rm = T) / ae119_n_age
# 
# ##### DRUMMOND 3
# ### 2016 ###
# dru316 = read.csv("2.12.19data.csv")
# ### remove seine data
# dru316 = dru316[dru316$gear != "seine",]
# ### keep only drummond 3 2016 data ; dont have 2017
# dru316 = dru316[(dru316$lake == "drummond3" & dru316$year == "2016"),]
# ### keep only lmb
# dru316 = dru316[dru316$species == "lmb",]
# ### remove empty levels
# dru316 = droplevels(dru316)
# headtail(dru316)
# 
# ### mean length-at-age prediction
# dru316$cmgrp = floor(dru316$tl / 25) * 25
# dru316_count_cmgrp = table(dru316$cmgrp)
# dru316_count_cmgrp_age = table(dru316$cmgrp, dru316$age)
# dru316_prob_cmgrp_age = prop.table(dru316_count_cmgrp_age, margin = 1)
# dru316_n_cmgrp_age = as.matrix(dru316_prob_cmgrp_age) * as.numeric(dru316_count_cmgrp)
# dru316_n_age = apply(dru316_n_cmgrp_age, 2, sum, na.rm = T)
# dru316_lengths = as.numeric(row.names(dru316_n_cmgrp_age))
# dru316_products = dru316_n_cmgrp_age * dru316_lengths
# dru316_mean_length = apply(dru316_products, 2, sum, na.rm = T) / dru316_n_age
# 
# ### 2018 ###
# dru318 = read.csv("2.12.19data.csv")
# ### remove seine data 
# dru318 = dru318[dru318$gear != "seine",]
# ### keep only drummond 3 2018 data
# dru318 = dru318[(dru318$lake == "drummond3" & dru318$year == "2018"),]
# ### keep only lmb
# dru318 = dru318[dru318$species == "lmb",]
# ### remove empty levels 
# dru318 = droplevels(dru318)
# headtail(dru318)
# 
# ### mean length-at-age prediction 
# dru318$cmgrp = floor(dru318$tl / 25) * 25
# dru318_count_cmgrp = table(dru318$cmgrp)
# dru318_count_cmgrp_age = table(dru318$cmgrp, dru318$age)
# dru318_prob_cmgrp_age = prop.table(dru318_count_cmgrp_age, margin = 1)
# dru318_n_cmgrp_age = as.matrix(dru318_prob_cmgrp_age) * as.numeric(dru318_count_cmgrp)
# dru318_n_age = apply(dru318_n_cmgrp_age, 2, sum, na.rm = T)
# dru318_lengths = as.numeric(row.names(dru318_n_cmgrp_age))
# dru318_products = dru318_n_cmgrp_age * dru318_lengths
# dru318_mean_length = apply(dru318_products, 2, sum, na.rm = T) / dru318_n_age
# 
# # ### 2019 ###
# # dru319 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # dru319 = dru319[dru319$gear != "seine",]
# # ### keep only drummond 3 2019 data
# # dru319 = dru319[(dru319$lake == "drummond3" & dru319$year == "2019"),]
# # ### keep only lmb
# # dru319 = dru319[dru319$species == "lmb",]
# # ### remove empty levels 
# # dru319 = droplevels(dru319)
# # headtail(dru319)
# # 
# # ### mean length-at-age prediction 
# # dru319$cmgrp = floor(dru319$tl / 25) * 25
# # dru319_count_cmgrp = table(dru319$cmgrp)
# # dru319_count_cmgrp_age = table(dru319$cmgrp, dru319$age)
# # dru319_prob_cmgrp_age = prop.table(dru319_count_cmgrp_age, margin = 1)
# # dru319_n_cmgrp_age = as.matrix(dru319_prob_cmgrp_age) * as.numeric(dru319_count_cmgrp)
# # dru319_n_age = apply(dru319_n_cmgrp_age, 2, sum, na.rm = T)
# # dru319_lengths = as.numeric(row.names(dru319_n_cmgrp_age))
# # dru319_products = dru319_n_cmgrp_age * dru319_lengths
# # dru319_mean_length = apply(dru319_products, 2, sum, na.rm = T) / dru319_n_age
# 
# ##### LEE MERRIWEATHER (LM) POND
# ### 2018 ###
# lm18 = read.csv("2.12.19data.csv")
# ### remove seine data 
# lm18 = lm18[lm18$gear != "seine",]
# ### keep only lee merriweather pond 2018 data
# lm18 = lm18[(lm18$lake == "lm" & lm18$year == "2018"),]
# ### keep only lmb
# lm18 = lm18[lm18$species == "lmb",]
# ### remove empty levels 
# lm18 = droplevels(lm18)
# headtail(lm18)
# 
# ### mean length-at-age prediction 
# lm18$cmgrp = floor(lm18$tl / 25) * 25
# lm18_count_cmgrp = table(lm18$cmgrp)
# lm18_count_cmgrp_age = table(lm18$cmgrp, lm18$age)
# lm18_prob_cmgrp_age = prop.table(lm18_count_cmgrp_age, margin = 1)
# lm18_n_cmgrp_age = as.matrix(lm18_prob_cmgrp_age) * as.numeric(lm18_count_cmgrp)
# lm18_n_age = apply(lm18_n_cmgrp_age, 2, sum, na.rm = T)
# lm18_lengths = as.numeric(row.names(lm18_n_cmgrp_age))
# lm18_products = lm18_n_cmgrp_age * lm18_lengths
# lm18_mean_length = apply(lm18_products, 2, sum, na.rm = T) / lm18_n_age
# 
# # ### 2019 ###
# # lm19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # lm19 = lm19[lm19$gear != "seine",]
# # ### keep only lm 2019 data
# # lm19 = lm19[(lm19$lake == "lm" & lm19$year == "2019"),]
# # ### keep only lmb
# # lm19 = lm19[lm19$species == "lmb",]
# # ### remove empty levels 
# # lm19 = droplevels(lm19)
# # headtail(lm19)
# # 
# # ### mean length-at-age prediction 
# # lm19$cmgrp = floor(lm19$tl / 25) * 25
# # lm19_count_cmgrp = table(lm19$cmgrp)
# # lm19_count_cmgrp_age = table(lm19$cmgrp, lm19$age)
# # lm19_prob_cmgrp_age = prop.table(lm19_count_cmgrp_age, margin = 1)
# # lm19_n_cmgrp_age = as.matrix(lm19_prob_cmgrp_age) * as.numeric(lm19_count_cmgrp)
# # lm19_n_age = apply(lm19_n_cmgrp_age, 2, sum, na.rm = T)
# # lm19_lengths = as.numeric(row.names(lm19_n_cmgrp_age))
# # lm19_products = lm19_n_cmgrp_age * lm19_lengths
# # lm19_mean_length = apply(lm19_products, 2, sum, na.rm = T) / lm19_n_age
# 
# ##### PROMISE LAKE
# ### 2018 ###
# pro18 = read.csv("2.12.19data.csv")
# ### remove seine data 
# pro18 = pro18[pro18$gear != "seine",]
# ### keep only promise lake 2018 data
# pro18 = pro18[(pro18$lake == "promise" & pro18$year == "2018"),]
# ### keep only lmb
# pro18 = pro18[pro18$species == "lmb",]
# ### remove empty levels 
# pro18 = droplevels(pro18)
# headtail(pro18)
# 
# ### mean length-at-age prediction 
# pro18$cmgrp = floor(pro18$tl / 25) * 25
# pro18_count_cmgrp = table(pro18$cmgrp)
# pro18_count_cmgrp_age = table(pro18$cmgrp, pro18$age)
# pro18_prob_cmgrp_age = prop.table(pro18_count_cmgrp_age, margin = 1)
# pro18_n_cmgrp_age = as.matrix(pro18_prob_cmgrp_age) * as.numeric(pro18_count_cmgrp)
# pro18_n_age = apply(pro18_n_cmgrp_age, 2, sum, na.rm = T)
# pro18_lengths = as.numeric(row.names(pro18_n_cmgrp_age))
# pro18_products = pro18_n_cmgrp_age * pro18_lengths
# pro18_mean_length = apply(pro18_products, 2, sum, na.rm = T) / pro18_n_age
# 
# # ### 2019 ###
# # pro19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # pro19 = pro19[pro19$gear != "seine",]
# # ### keep only promise 2019 data
# # pro19 = pro19[(pro19$lake == "promise" & pro19$year == "2019"),]
# # ### keep only lmb
# # pro19 = pro19[pro19$species == "lmb",]
# # ### remove empty levels 
# # pro19 = droplevels(pro19)
# # headtail(pro19)
# # 
# # ### mean length-at-age prediction 
# # pro19$cmgrp = floor(pro19$tl / 25) * 25
# # pro19_count_cmgrp = table(pro19$cmgrp)
# # pro19_count_cmgrp_age = table(pro19$cmgrp, pro19$age)
# # pro19_prob_cmgrp_age = prop.table(pro19_count_cmgrp_age, margin = 1)
# # pro19_n_cmgrp_age = as.matrix(pro19_prob_cmgrp_age) * as.numeric(pro19_count_cmgrp)
# # pro19_n_age = apply(pro19_n_cmgrp_age, 2, sum, na.rm = T)
# # pro19_lengths = as.numeric(row.names(pro19_n_cmgrp_age))
# # pro19_products = pro19_n_cmgrp_age * pro19_lengths
# # pro19_mean_length = apply(pro19_products, 2, sum, na.rm = T) / pro19_n_age
# 
# ##### MARK WILLIAMS POND
# ### 2018 ###
# mar18 = read.csv("2.12.19data.csv")
# ### remove seine data 
# mar18 = mar18[mar18$gear != "seine",]
# ### keep only mark pond 2018 data
# mar18 = mar18[(mar18$lake == "mark" & mar18$year == "2018"),]
# ### keep only lmb
# mar18 = mar18[mar18$species == "lmb",]
# ### remove empty levels 
# mar18 = droplevels(mar18)
# headtail(mar18)
# 
# ### mean length-at-age prediction 
# mar18$cmgrp = floor(mar18$tl / 25) * 25
# mar18_count_cmgrp = table(mar18$cmgrp)
# mar18_count_cmgrp_age = table(mar18$cmgrp, mar18$age)
# mar18_prob_cmgrp_age = prop.table(mar18_count_cmgrp_age, margin = 1)
# mar18_n_cmgrp_age = as.matrix(mar18_prob_cmgrp_age) * as.numeric(mar18_count_cmgrp)
# mar18_n_age = apply(mar18_n_cmgrp_age, 2, sum, na.rm = T)
# mar18_lengths = as.numeric(row.names(mar18_n_cmgrp_age))
# mar18_products = mar18_n_cmgrp_age * mar18_lengths
# mar18_mean_length = apply(mar18_products, 2, sum, na.rm = T) / mar18_n_age
# 
# # ### 2019 ###
# # mar19 = read.csv("2.12.19data.csv")
# # ### remove seine data 
# # mar19 = mar19[mar19$gear != "seine",]
# # ### keep only mark 2019 data
# # mar19 = mar19[(mar19$lake == "mark" & mar19$year == "2019"),]
# # ### keep only lmb
# # mar19 = mar19[mar19$species == "lmb",]
# # ### remove empty levels 
# # mar19 = droplevels(mar19)
# # headtail(mar19)
# # 
# # ### mean length-at-age prediction 
# # mar19$cmgrp = floor(mar19$tl / 25) * 25
# # mar19_count_cmgrp = table(mar19$cmgrp)
# # mar19_count_cmgrp_age = table(mar19$cmgrp, mar19$age)
# # mar19_prob_cmgrp_age = prop.table(mar19_count_cmgrp_age, margin = 1)
# # mar19_n_cmgrp_age = as.matrix(mar19_prob_cmgrp_age) * as.numeric(mar19_count_cmgrp)
# # mar19_n_age = apply(mar19_n_cmgrp_age, 2, sum, na.rm = T)
# # mar19_lengths = as.numeric(row.names(mar19_n_cmgrp_age))
# # mar19_products = mar19_n_cmgrp_age * mar19_lengths
# # mar19_mean_length = apply(mar19_products, 2, sum, na.rm = T) / mar19_n_age
# 
# ##### CALL OUT MLA FOR EACH LAKE BY YEAR 
# ### TREATMENTS ###
# was17_mean_length
# was18_mean_length
# bar17_mean_length
# bar18_mean_length
# mon17_mean_length
# mon18_mean_length
# # mon19_mean_length
# lit17_mean_length
# lit18_mean_length
# # lit19_mean_length
# hor17_mean_length
# hor18_mean_length
# # hor19_mean_length
# s317_mean_length
# s318_mean_length
# # s319_mean_length
# dru116_mean_length
# dru118_mean_length
# # dru119_mean_length
# bri18_mean_length
# # bri19_mean_length
# dea18_mean_length
# # dea19_mean_length
# gri18_mean_length
# # gri19_mean_length
# ### CONTROLS ###
# and17_mean_length
# and18_mean_length
# lee17_mean_length
# lee18_mean_length
# dal17_mean_length
# dal18_mean_length
# # dal19_mean_length
# big17_mean_length
# big18_mean_length
# # big19_mean_length
# fp317_mean_length
# fp318_mean_length
# # fp319_mean_length
# ae117_mean_length
# ae118_mean_length
# # ae119_mean_length
# dru316_mean_length
# dru318_mean_length
# # dru319_mean_length
# lm18_mean_length
# # lm19_mean_length
# pro18_mean_length
# # pro19_mean_length
# mar18_mean_length
# # mar19_mean_length
# 
# 
# ##### MLA BACI ANALYSIS
# # attributes(and17_mean_length)
# # and17_mean_length[["1.5"]]
# # controls_pre = c(and17_mean_length[["1.5"]], lee17_mean_length[["1"]], dal17_mean_length[["1"]], big17_mean_length[["1"]],
# #                  fp317_mean_length[["1"]], ae117_mean_length[["1"]])
# # treat_pre = c(was17_mean_length[["1"]], bar17_mean_length[["1"]], mon17_mean_length[["1"]], lit17_mean_length[["1"]], 
# #               hor17_mean_length[["1"]], s317_mean_length[["1"]])
# # controls_post = c(and18_mean_length[["1"]], lee18_mean_length[["1"]], dal18_mean_length[["1"]], big18_mean_length[["1"]],
# #                  fp318_mean_length[["1"]], ae118_mean_length[["1"]])
# # treat_post = c(was18_mean_length[["1"]], bar18_mean_length[["1"]], mon18_mean_length[["1"]], 
# #               hor18_mean_length[["1"]])
# # dfmla = as.data.frame(list(controls_pre, treat_pre, controls_post), col.names = c("control", "treat"))
# # dfmla1 = as.data.frame(list(and17_mean_length[["1.5"]], lee17_mean_length[["1"]], dal17_mean_length[["1"]], big17_mean_length[["1"]],
# #                             fp317_mean_length[["1"]], ae117_mean_length[["1"]], and18_mean_length[["1"]], lee18_mean_length[["1"]], 
# #                             dal18_mean_length[["1"]], big18_mean_length[["1"]], fp318_mean_length[["1"]], ae118_mean_length[["1"]]),
# #                        col.names = c(1:12))
# 
# ### CREATE NEW DF WITH MLAs ###
# # mladf = data.frame(matrix(ncol = 5, nrow = 0))
# # names = c("lake", "type", "year", "age", "mla")
# # colnames(mladf) = names
# ## same as above
# mladf = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("lake", "type", "year", "age", "mla"))
# 
# 
# 
# fit = glmer.nb(count ~ period * type * event + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# 
# ##### control ALK 
# datum.control <- read.csv("1.21.19data.csv")
# head(datum.control)
# tail(datum.control)
# summary(datum.control)
# # datum.control[is.na(datum.control$lake),]
# # datum.control <- datum.control[!is.na(datum.control$lake),]
# # unique(datum.control$lake)
# 
# ### remove seine data
# datum.control = datum.control[datum.control$gear != "seine",]
# 
# ### remove griggs1 data
# datum.control = datum.control[datum.control$lake != "griggs1",]
# 
# ### remove harris data
# datum.control = datum.control[datum.control$lake != "harris",]
# 
# ### remove larry data
# datum.control = datum.control[datum.control$lake != "larry",]
# 
# ## remove s6 data
# datum.control = datum.control[datum.control$lake != "s6",]
# 
# ## remove anderson county lake 2017 bc of efing time of year
# datum.control = datum.control[!(datum.control$lake == "anderson county lake" & datum.control$year == "2017"),]
# 
# ### keep only lmb
# datum.control = datum.control[datum.control$species == "lmb",]
# 
# ### keep only control
# datum.control = datum.control[datum.control$type == "control",]
# 
# ### remove empty levels 
# datum.control = droplevels(datum.control)
# 
# ### only keep 2018
# # datum.control = datum.control[datum.control$year == "2018",]
# ### add a date column
# datum.control$date = paste(datum.control$month, datum.control$day, sep = "/")
# 
# ## select only columns lake, age, tl
# dat.control = select(datum.control, "lake", "age", "tl", "type")
# head(dat.control)
# ## separate observed data into age- and length-samples
# rb.len = filter(dat.control, is.na(dat.control$age))
# str(rb.len)
# rb.age = filter(dat.control, !is.na(dat.control$age))
# str(rb.age)
# 
# 
# ## add a variable to the age-sample that contains the 25mm TL bins
# rb.age.mod = mutate(rb.age, lcat = lencat(tl, w = 25))
# headtail(rb.age.mod)
# head(rb.age.mod)
# summary(rb.age.mod)
# raw = xtabs(~lcat + age, data = rb.age.mod)
# 
# ## construct a table of the # of fish in each age and 25mm TL category in the age-sample
# ALK.obs = prop.table(raw, margin = 1)
# lblTL = "Total Length (mm)"
# alkPlot(ALK.obs, showLegend = T, xlab = lblTL)
# 
# ## construct an observed ALK from the table above
# mlr = multinom(age ~ lcat, data = rb.age.mod, maxit = 500)
# lens = seq(50, 600, 25)
# ALK.sm = predict(mlr, data.frame(lcat = lens), type = "probs")
# row.names(ALK.sm) = lens
# round(ALK.sm, 3)
# alkPlot(ALK.sm, showLegend = T, xlab = lblTL)
# 
# ## use the semi random assignemnt from Isermann and Knight and the observed ALK to assign ages to the unaged fish 
# rb.len.mod = alkIndivAge(ALK.obs, age ~ tl, data = rb.len)
# headtail(rb.len.mod)
# rb.comb = rbind(rb.age, rb.len.mod)
# str(rb.comb)
# 
# ## how many fish are estimated to be each age
# agefreq = xtabs(~age, data = rb.comb)
# 
# ## plot age distribution for all fish
# hist(~age, data = rb.comb, w = 1, xlab = "Age (yrs)")
# 
# ## how many fish in each 25mm interval
# rb.comb = mutate(rb.comb, lcat = lencat(tl, w = 25))
# xtabs(~lcat, data = rb.comb)
# 
# ## mean TL of aged fish
# rb.sum = Summarize(tl ~ age, data = rb.comb, digits = 2)
# 
# ## plot l-a-a with the mean l-a-a superimposed for all fish
# plot(tl ~ age, data = rb.comb, ylab = lblTL, xlab = "Age (yrs)", pch = 19, col = col2rgbt("black", 0.05))
# lines(mean ~ age, data = rb.sum, col = "blue", lwd = 2)
# 
# 
# ##### treatment ALK 
# datum.treat <- read.csv("1.21.19data.csv")
# head(datum.treat)
# tail(datum.treat)
# summary(datum.treat)
# # datum.treat[is.na(datum.treat$lake),]
# # datum.treat <- datum.treat[!is.na(datum.treat$lake),]
# # unique(datum.treat$lake)
# 
# ### remove seine data
# datum.treat = datum.treat[datum.treat$gear != "seine",]
# 
# ### remove griggs1 data
# datum.treat = datum.treat[datum.treat$lake != "griggs1",]
# 
# ### remove harris data
# datum.treat = datum.treat[datum.treat$lake != "harris",]
# 
# ### remove larry data
# datum.treat = datum.treat[datum.treat$lake != "larry",]
# 
# ## remove s6 data
# datum.treat = datum.treat[datum.treat$lake != "s6",]
# 
# ## remove anderson county lake 2017 bc of efing time of year
# datum.treat = datum.treat[!(datum.treat$lake == "anderson county lake" & datum.treat$year == "2017"),]
# 
# ### keep only lmb
# datum.treat = datum.treat[datum.treat$species == "lmb",]
# 
# ### keep only treat
# datum.treat = datum.treat[datum.treat$type == "treat",]
# 
# ### remove empty levels 
# datum.treat = droplevels(datum.treat)
# 
# ### only keep 2018
# # datum.treat = datum.treat[datum.treat$year == "2018",]
# ### add a date column
# datum.treat$date = paste(datum.treat$month, datum.treat$day, sep = "/")
# 
# ## select only columns lake, age, tl
# dat.treat = select(datum.treat, "lake", "age", "tl", "type")
# head(dat.treat)
# ## separate observed data into age- and length-samples
# rb.len = filter(dat.treat, is.na(dat.treat$age))
# str(rb.len)
# rb.age = filter(dat.treat, !is.na(dat.treat$age))
# str(rb.age)
# 
# ## add a variable to the age-sample that contains the 25mm TL bins
# rb.age.mod = mutate(rb.age, lcat = lencat(tl, w = 25))
# headtail(rb.age.mod)
# head(rb.age.mod)
# summary(rb.age.mod)
# raw = xtabs(~lcat + age, data = rb.age.mod)
# 
# ## construct a table of the # of fish in each age and 25mm TL category in the age-sample
# ALK.obs = prop.table(raw, margin = 1)
# lblTL = "Total Length (mm)"
# alkPlot(ALK.obs, showLegend = T, xlab = lblTL)
# 
# ## construct an observed ALK from the table above
# mlr = multinom(age ~ lcat, data = rb.age.mod, maxit = 500)
# lens = seq(50, 600, 25)
# ALK.sm = predict(mlr, data.frame(lcat = lens), type = "probs")
# row.names(ALK.sm) = lens
# round(ALK.sm, 3)
# alkPlot(ALK.sm, showLegend = T, xlab = lblTL)
# 
# ## use the semi random assignemnt from Isermann and Knight and the observed ALK to assign ages to the unaged fish 
# rb.len.mod = alkIndivAge(ALK.obs, age ~ tl, data = rb.len)
# headtail(rb.len.mod)
# rb.comb = rbind(rb.age, rb.len.mod)
# str(rb.comb)
# 
# ## how many fish are estimated to be each age
# agefreq = xtabs(~age, data = rb.comb)
# 
# ## plot age distribution for all fish
# hist(~age, data = rb.comb, w = 1, xlab = "Age (yrs)")
# 
# ## how many fish in each 25mm interval
# rb.comb = mutate(rb.comb, lcat = lencat(tl, w = 25))
# xtabs(~lcat, data = rb.comb)
# 
# ## mean TL of aged fish
# rb.sum = Summarize(tl ~ age, data = rb.comb, digits = 2)
# 
# ## plot l-a-a with the mean l-a-a superimposed for all fish
# plot(tl ~ age, data = rb.comb, ylab = lblTL, xlab = "Age (yrs)", pch = 19, col = col2rgbt("black", 0.05))
# lines(mean ~ age, data = rb.sum, col = "blue", lwd = 2)
# 
# 
# ##### MATTS DATA COMPARISON 
# setwd("~/Documents/Classes/Fish Pop Dynamics/Labs")
# 
# 
# a=read.csv('lmb_data.csv',na.strings='.')
# a
# a = a[a$length != 84,]
# a = a[a$length != 86,]
# ##
# junk.len = filter(a, is.na(a$age))
# str(junk.len)
# junk.age = filter(a, !is.na(a$age))
# str(junk.age)
# ### TAPPLY ###
# tapply(junk.age$length, junk.age$age, mean)
# ##
# junk.age.mod = mutate(junk.age, lcat = lencat(length, w = 10))
# headtail(junk.age.mod)
# raw = xtabs(~lcat + age, data = junk.age.mod)
# ##
# ALK.obs.junk = prop.table(raw, margin = 1)
# lblTL = "TL (mm)"
# alkPlot(ALK.obs.junk, showLegend = T, xlab = lblTL)
# ##
# mlr = multinom(age ~ lcat, data = junk.age.mod, maxit = 500)
# lens = seq(50, 600, 10)
# ALK.sm = predict(mlr, data.frame(lcat = lens), type = "probs")
# row.names(ALK.sm) = lens
# round(ALK.sm, 3)
# alkPlot(ALK.sm, showLegend = T, xlab = lblTL)
# ##
# junk.len.mod = alkIndivAge(ALK.obs, age ~ length, data = junk.len)
# headtail(junk.len.mod)
# junk.comb = rbind(junk.age, junk.len.mod)
# str(junk.comb)
# ##
# agefreq.junk = xtabs(~age, data = junk.comb)
# ##
# hist(~age, data = junk.comb, w = 1, xlab = "Age")
# ##
# junk.comb = mutate(junk.comb, lcat = lencat(length, w = 10))
# xtabs(~lcat, data = junk.comb)
# ##
# junk.sum = Summarize(length ~ age, data = junk.comb, digits = 2)
# ##
# plot(length ~ age, data = junk.comb, ylab = lblTL, xlab = "Age", pch = 19, col = col2rgbt("black", 0.05))
# lines(mean ~ age, data = junk.sum, col = "blue", lwd = 2)