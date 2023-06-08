###LARGEMOUTH BASS###
rm(list = ls(all = T))

library(lme4)
library(nlme)
library(MASS)
library(plyr)
library(dplyr)
library(MuMIn)
library(lmerTest)
# library(pbkrtest)
# library(r2glmm)
# source("AICc_table_func.R")
ppi = 600 


# datum = read.csv(file.choose())
# setwd("~/Documents/Projects/Rotenone/Data")
datum = read.csv("data/3.27.19data.csv")
colnames(datum)[1] = "lake" # this is important for non-Mac users (for some reason)
head(datum)
tail(datum)
summary(datum)
datum[is.na(datum$lake), ]
levels(datum$lake)

# remove this line if you fixed the error found in the data -- this is fixed -- not erasing for reference 
# datum[datum$lake == "lm" & datum$event == 3 & datum$year == 2018 & datum$period == "before",]$event = 2

datum = datum[datum$gear != "efishing",]  # remove efishing data
datum = datum[datum$lake != "griggs1",]   # remove griggs1 data
datum = datum[datum$lake != "harris",]    # remove harris data
datum = datum[datum$lake != "s6",]        # remove s6 data
datum = datum[datum$species == "lmb",]    # keep only lmb
datum = droplevels(datum)                 # remove empty cells 
datum = datum[datum$tlgroup != "FALSE" | is.na(datum$tlgroup),]      # remove fish > 137.5mm
datum$date = paste(datum$month, datum$day, datum$year, sep = "/")    # add a date column
remove.type = "type"            # remove type because it is duplicated when we combine below
datum = datum[, ! names(datum) %in% remove.type, drop = F]           # remove type from datum
unique(datum$date)

# combine lake id data with fish data
datum = datum[order(datum$lake, datum$year),]                  # orders datum by lake and yr       
ids = read.csv("data/lake_year_ids.csv", stringsAsFactors = F) # read in id file
datum = merge(ids, datum, by = c("lake", "year"))              # merge datum and id files by lake and year
datum = datum[datum$year != 2016,]                             # take out 2016 data
datum$year = as.character(datum$year)                    
datum$times_treat = as.factor(datum$times_treat)         
datum$size_class = ifelse(datum$size > 30, "big", "small")    # make size categorical 
datum$period = relevel(factor(datum$period), ref = "before")  # make "before" ref group
datum$type = relevel(factor(datum$type), ref = "control")     # make "control" ref group
datum$size_class = relevel(factor(datum$size_class), ref = "small")   # this might not work?
datum$type_treat = ifelse(datum$times_treat == "0", "control", "treat")

# datum = subset(datum, select = -c(age1, age2, age, NOTES, X, wt, 
#                                   month, day, hour, min, gear, 
#                                   effort, status, species, id, structure))
# write.csv(datum, "lmb_seine.csv", row.names = FALSE)        # write dataset into a .csv to send


## Analyses
##### BACI ANALYSIS: DAY 1 VS DAY 2 LMB -------------------------------------------------------------
datsub = datum[datum$period %in% c("before", "after") & datum$event %in% c(1,2), ]
datsub = datsub[datsub$size_class %in% "small",]   # for small sized impoundment analysis
# datsub = datsub[datsub$size_class %in% "big",]      # for large sized impoundment analysis
unique(datsub$event)
unique(datsub$period)
unique(datsub$size_class)

df = aggregate(datsub['count'], list(lake = datsub$lake, period = datsub$period, 
                                     type = datsub$type, event = datsub$event, 
                                     year = datsub$year), sum)
df$period = relevel(df$period, ref = 'before')
df$type = relevel(df$type, ref = 'control')
df$event = as.factor(df$event)
df$period = droplevels(df$period)
df$lakeyr = paste(df$lake, df$year, sep='')   # only needed for glmmPQL - used to get df
hist(df$count, breaks = 50)
hist(df$count)
hist(log(df$count))

## Models used for manuscript
fit = glmer.nb(count ~ period * type * event + (1 | lake:year), data = df,
               glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
fit = glmer.nb(count ~ period * type * event + (1 | lake:year), data = df,
               glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 100000000)))
fit = glmmPQL(count ~ period * type * event, random =  ~1 | lakeyr,
              data = df, family = "quasipoisson") # use this to get df

fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df,
               glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
fit = glmmPQL(count ~ period * type, random =  ~1 | lakeyr, 
              data = df, family = "quasipoisson")         # use this to get df

###
##### Old Models #####
# fit = glmer.nb(count ~ period * type * event * size + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))

# fit = glmmPQL(count ~ period * type * event * size, 
#               random =  ~1 | lakeyr, data = df, family = "quasipoisson")  # use this to get df
# fit = glmmPQL(count ~ period * type * event, random =  ~1 | lakeyr, 
#               data = df, family = "quasipoisson")         # use this to get df
# fit = glmmPQL(count ~ period * type, random =  ~1 | lakeyr, 
#               data = df, family = "quasipoisson")                 # use this to get df

# fit = lmer(count ~ period * type * event * size + (1 | lake) + (1|year), data = df, REML = F)
# fit = lmer(count ~ period * type * event + (1 | lake) + (1|year), data = df, REML = F)
# fit = lmer(count ~ period * type + (1 | lake) + (1|year), data = df, REML = F)
# 
# fit = glmer.nb(count ~ period * type * event + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df, nAGQ = 0,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit2 = glmer.nb(count ~ period + type + event + period:type + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit3 = glmer.nb(count ~ period + type + period:type + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# aic_d1_d2 = aic_table(AICc(fit, fit2))
# fit = glmer.nb(count ~ period * type * relevel(event, ref = 2) + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * relevel(type, ref = "treat") * relevel(event, ref = 2) + (1 | lake:year), 
#                data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * relevel(type, ref = "treat") * event + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
#####

summary(fit)
anova(fit)
# q=r2beta(model = fit, method = "sgv")
# KRmodcomp.lmerMod(fit)

coefs = summary(fit)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

exp(-3.1675678 + 0.2347063 + 0.2138647)

plot(exp(coefs[2:4,"Estimate"]), ylim = range(lwrs[2:4], uprs[2:4]))
segments(1:7, lwrs[2:4], 1:7, uprs[2:4])
abline(h = 1)

summary(fit)
anova(fit)

df$pred = exp(predict(fit, re.form = NA))

means = tapply(df$pred, list(df$period, df$type, df$event), mean)

means["after","control",1]/means["before","control",1]  # period effects by treat level
means["after","treat",1]/means["before","treat",1]      
log(0.0532/1.2645)

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

## this function is for model without event
# myfunc = function(.) {
#   beta = unname(fixef(.))
#   
#   cb1 = exp(beta[1])             # mean count control before event 1
#   ca1 = exp(beta[1] + beta[2])
#   tb1 = exp(beta[1] + beta[3])   # mean count treatment before event 1
#   ta1 = exp(sum(beta[c(1:4)]))
#   
#   c(cb1 = cb1, tb1 = tb1, ca1 = ca1, ta1 = ta1)
# }

## this function is to separate events 
myfunc = function(.) {
  beta = unname(fixef(.))
  
  cb1 = exp(beta[1])             # mean count control before event 1
  ca1 = exp(beta[1] + beta[2])   # mean count control after in event 1
  tb1 = exp(beta[1] + beta[3])   # mean count treatment before in event 1
  ta1 = exp(sum(beta[c(1:3,5)])) # mean count 
  
  
  cb2 = exp(beta[1]+beta[4])             # mean count control before event 2
  ca2 = exp(beta[1] + beta[2]+beta[4]+beta[6])
  tb2 = exp(beta[1] + beta[3]+beta[4]+beta[7])   # mean count treatment before event 2
  ta2 = exp(sum(beta))
  
  c(cb1 = cb1, tb1 = tb1, ca1 = ca1, ta1 = ta1, cb2 = cb2, tb2 = tb2, ca2 = ca2, ta2 = ta2)
}

myfunc(fit)


start = Sys.time()
rand = bootMer(x = fit, FUN = myfunc, nsim = 1000)$t
Sys.time() - start

# ## remove rows with missing data
# complete.cases(rand)
# rand = rand[complete.cases(rand), ]

summ = apply(rand, 2, function(x) c(mean = mean(na.omit(x)), quantile(na.omit(x), c(0.025, 0.975))))
# summ = log(summ)  # log() data for plotting 
## percent change event 1; c = control, t = treatment
percentc1 = ((summ[1, 3] - summ[1, 1]) / summ[1, 1]) * 100
percentt1 = ((summ[1, 4] - summ[1, 2]) / summ[1, 2]) * 100
## percent change event 2; c = control, t = treatment
percentc2 = ((summ[1, 7] - summ[1, 5]) / summ[1, 5]) * 100
percentt2 = ((summ[1, 8] - summ[1, 6]) / summ[1, 6]) * 100

## line plot ##

# ## applicaiton 1 and 2
# x = c(0.9,2.1)
# xc = x + 0.03
# xt = x - 0.03
# xc2 = x + 0.06
# xt2 = x - 0.06
# png("lmb_day1_day2.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.axis = 1.2, cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "",
#      xlim = c(0.75,2.25), ylim = c(0, 500),
#      las = 1, xaxt = "n", yaxt = "n")     # change ylim for log()
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
# arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], col = "blue", 
#        length = 0, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], col = "red",
#        length = 0, cex = 1.2, lwd = 3, pch = 16)
# points(summ[1,c(5,7)] ~ xc2, type = "l", pch = 16, lty = 2, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(6,8)] ~ xt2, type = "l", pch = 2, lty = 2, lwd = 3, cex = 1.2, col = "red")
# arrows(xc2, summ[2,c(5,7)], xc2, summ[3,c(5,7)], col = "blue",
#        length = 0, lty = 2, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt2, summ[2,c(6,8)], xt2, summ[3,c(6,8)], col = "red", 
#        length = 0, lty = 2, cex = 1.2, lwd = 3, pch = 16)
# mtext(side = 2, "Total LMB Catch Per Impoundment", line = 2.8, cex = 1.2)           # change for log()
# axis(side = 2, at = seq(0, 450, 50), labels = seq(0, 450, 50), las = 2)             # change for log()
# axis(side = 1, at = x, labels = rep("", 2))
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 1 & 21\nPre-Treatment", "Day 2 & 22\nPost-Treatment"), xpd = T)
# legend("top", legend = c("1", "2"), lty = c(1, 2), 
#        bty = "n", lwd = 2, title = "Application", seg.len = 3)
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.20, pos = 4,
#      label = paste("App. 1 Control Increase: ", 
#                    round(percentc1), "%", sep = ""), col = "blue")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.25, pos = 4,
#      label = paste("App. 1 Treatment Decrease: ",
#                    abs(round(percentt1)), "%", sep = ""), col = "red")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.30, pos = 4,
#      label = paste("App. 2 Control Increase: ", 
#                    round(percentc2), "%", sep = ""), col = "blue")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.35, pos = 4,
#      label = paste("App. 2 Treatment Decrease: ", 
#                    abs(round(percentt2)), "%", sep = ""), col = "red")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.40, pos = 4,
#      label = paste("Treatment x Time Interaction: p < 0.001"), col = "black")
# text(x = usr[1] + xdiff * 0.1, y = usr[4] - ydiff * 0.75, pos = 4,
#      label = paste("App. 1 Control Increase: ",
#                    round(percentc1), "%", sep = ""), col = "blue")          # log()
# text(x = usr[1] + xdiff * 0.1, y = usr[4] - ydiff * 0.80, pos = 4,
#      label = paste("App. 1 Treatment Decrease: ", 
#                    abs(round(percentt1)), "%", sep = ""), col = "red")    # log()
# text(x = usr[1] + xdiff * 0.1, y = usr[4] - ydiff * 0.85, pos = 4,
#      label = paste("App. 2 Control Increase: ",
#                    round(percentc2), "%", sep = ""), col = "blue")          # log()
# text(x = usr[1] + xdiff * 0.1, y = usr[4] - ydiff * 0.90, pos = 4,
#      label = paste("App. 2 Treatment Decrease: ",
#                    abs(round(percentt2)), "%", sep = ""), col = "red")    # log()
# text(x = usr[1] + xdiff * 0.1, y = usr[4] - ydiff * 0.80, pos = 4,
#      label = paste("Treatment x Time Interaction: p < 0.001"), col = "black")  # log()
# dev.off()

# ## applicaiton 1
# x = c(0.9,2.1)
# xc = x + 0.03
# xt = x - 0.03
# xc2 = x + 0.06
# xt2 = x - 0.06
# png("lmb_day1_day2_app1.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.axis = 1.2, cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), 
#      ylim = c(0, 450),las = 1, xaxt = "n", yaxt = "n", main = "Applicaiton 1")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
# arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], col = "blue", length = 0, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], col = "red", length = 0, cex = 1.2, lwd = 3, pch = 16)
# mtext(side = 2, "Total LMB Catch Per Impoundment", line = 2.8, cex = 1.2)
# axis(side = 2, at = seq(0, 450, 50), labels = seq(0, 450, 50), las = 2)
# axis(side = 1, at = x, labels = rep("", 2))
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 1\nPre-Treatment", "Day 2\nPost-Treatment"), xpd = T)  # only event 1
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.1, pos = 4,
#      label = paste("App. 1 Control Increase: ", round(percentc1), "%", sep = ""),
#      col = "blue")   # only event 1
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.15, pos = 4,
#      label = paste("App. 1 Treatment Decrease: ", abs(round(percentt1)), "%", sep = ""), 
#      col = "red")  # only event 1
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.20, pos = 4,
#      label = paste("Treatment x Time Interaction: p < 0.001"), col = "black")   # only event 1
# dev.off()

# ## applicaiton 2
# x = c(0.9,2.1)
# xc = x + 0.03
# xt = x - 0.03
# xc2 = x + 0.06
# xt2 = x - 0.06
# png("lmb_day1_day2_app2.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.axis = 1.2, cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", 
#      xlim = c(0.75,2.25), ylim = c(0, 180),las = 1, 
#      xaxt = "n", yaxt = "n", main = "Applicaiton 2")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# axis(side = 2, at = seq(0, 180, 25), labels = seq(0, 180, 25), las = 2)
# axis(side = 1, at = x, labels = rep("", 2))
# mtext(side = 2, "Total LMB Catch Per Impoundment", line = 2.8, cex = 1.2)
# points(summ[1,c(5,7)] ~ xc2, type = "l", pch = 16, lty = 2, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(6,8)] ~ xt2, type = "l", pch = 2, lty = 2, lwd = 3, cex = 1.2, col = "red")
# arrows(xc2, summ[2,c(5,7)], xc2, summ[3,c(5,7)], col = "blue", length = 0,
#        lty = 2, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt2, summ[2,c(6,8)], xt2, summ[3,c(6,8)], col = "red", length = 0,
#        lty = 2, cex = 1.2, lwd = 3, pch = 16)
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.10, pos = 4,
#      label = paste("App. 2 Control Increase: ", round(percentc2), "%", sep = ""),
#      col = "blue")  # only event 2
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.15, pos = 4,
#      label = paste("App. 2 Treatment Decrease: ", abs(round(percentt2)), "%", sep = ""),
#      col = "red") # only event 2
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 21\nPre-Treatment", "Day 22\nPost-Treatment"), xpd = T)   # only event 2
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.20, pos = 4, 
#      label = paste("Treatment x Time Interaction: p < 0.001"), col = "black")    # only event 2
# dev.off()










##### BACI ANALYSIS: Day one VS Follow up GLMER.NB -------------------------------------------------
datsub = datum[datum$period %in% c("before", "followup") & datum$event %in% c(1,3),]
datsub = datsub[datsub$size_class %in% "small",]   # for small sized impoundment analysis
datsub = datsub[datsub$size_class %in% "big",]      # for large sized impoundment analysis
unique(datsub$event)
unique(datsub$size_class)
# df = aggregate(datsub['count'], list(lake = datsub$lake, period = datsub$period), sum)

### df = data.frame(count = unname(counts), lake = names(counts))
# levels(df$lake)

# df = aggregate(datsub['count'], list(lake = datsub$lake, period = datsub$period,
#                                      type = datsub$type, event = datsub$event,
#                                      year = datsub$year, size = datsub$size_class), sum)
df = aggregate(datsub['count'], list(lake = datsub$lake, period = datsub$period, 
                                     type = datsub$type, event = datsub$event, 
                                     year = datsub$year), sum)
df = droplevels(df)
df$lakeyr = paste(df$lake, df$year, sep='')

df$period = relevel(df$period, ref = 'before')
df$type = relevel(df$type, ref = 'control')
table(df$period, df$lake, df$year)
df$lakeyr = paste(df$lake, df$year, sep='')   # only needed for glmmPQL - used to get df

hist(df$count)
hist(log(df$count))

## Models used for manuscript
fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df, nAGQ = 1L, start = 0.21,
               control=glmerControl(optimizer="nloptwrap",
                                    boundary.tol=1e-2,
                                    check.conv.singular =.makeCC(action="ignore",tol=1e-2),
                                    tolPwrss=1e-2))
summary(fit)
anova(fit)
fit = glmer.nb(count ~ period * relevel(type, ref = "treat") + (1 | lake:year), 
               data = df, nAGQ = 1L, start = 0.21,
               control=glmerControl(optimizer="nloptwrap",
                                    boundary.tol=1e-2,
                                    check.conv.singular =.makeCC(action="ignore",tol=1e-2),
                                    tolPwrss=1e-2))
summary(fit)

# run PQL to get DF for reporting stats
fit = glmmPQL(count ~ period * type, random = ~1|lakeyr, data = df, family = "quasipoisson")
summary(fit)
fit = glmmPQL(count ~ period * relevel(type, ref = "treat"), random = ~1|lakeyr, 
              data = df, family = "quasipoisson")

## Old Models (Older listed at bottom of script); size_class interaction not sig, so take it out
# fit = glmer.nb(count ~ period * type * size + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period + type + type:period + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period + relevel(type, ref = "treat") +  
#                  relevel(type, ref = "treat"):period + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ relevel(period, ref = "followup") + type +  
#                  type:relevel(period, ref = "followup") + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))

# summary(glmmPQL(count ~ period * type * size, random = ~1|lakeyr, data = df, family = "quasipoisson"))
# summary(glmmPQL(count ~ period * type, random = ~1|lakeyr, data = df, family = "quasipoisson"))
# summary(glmmPQL(count ~ relevel(period, ref = "followup") * type, random = ~1|lakeyr,
#                 data = df, family = "quasipoisson"))

summary(fit)
anova(fit)
coefs = summary(fit)$coef
coefs_est = exp(coefs[,"Estimate"])
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

plot(exp(coefs[2:4,"Estimate"]), ylim = range(lwrs[2:4], uprs[2:4]))
segments(1:7, lwrs[2:4], 1:7, uprs[2:4])
abline(h = 1)

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

# coefs = fixef(fit)
# exp(coefs[1]) * exp(coefs[2])          # average count in the followup within controls
# exp(coefs[1]) * exp(coefs[3])          # ave count in the treat and controls before
# exp(coefs[1]) * exp(coefs[4])          # mean diff btw treat and control after


myfunc = function(.) {
  beta = unname(fixef(.))
  
  cb = exp(beta[1])             # mean count control before
  cf = exp(beta[1] + beta[2])
  tb = exp(beta[1] + beta[3])   # mean count treatment before
  tf = exp(sum(beta[1:4]))
  
  c(cb = cb, tb = tb, cf = cf, tf = tf)
}

start = Sys.time()
rand = bootMer(x = fit, FUN = myfunc, nsim = 1000)$t
Sys.time() - start

summ = apply(rand, 2, function(x) c(mean = mean(x, na.rm = TRUE), 
                                    quantile(x, c(0.025, 0.975),na.rm = TRUE)))
# summ = log(summ)   # log() for plot
## percent change; c = control, t = treatment
percent_c = ((summ[1, 3] - summ[1, 1]) / summ[1, 1]) * 100
percent_t = ((summ[1, 4] - summ[1, 2]) / summ[1, 2]) * 100



# barplot
# png("glmer_lmb_day1_follwup.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2))
# mp = barplot(summ[1,], ylim = c(0, max(summ) + 50), las = 1, names.arg = rep("", 4))
# usr = par("usr"); ydiff = usr[4] - usr[3]; xdiff = usr[2] - usr[1]
# arrows(mp, summ[2,], mp, summ[3,], length = 0.05, angle = 90, code = 3)
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# axis(side = 1, at = mp, labels = rep("", 4))
# text(x = mp, y = usr[3] - 0.07 * ydiff, labels = c("Control", "Treatment", "Control", "Treatment"), 
#      xpd = T)
# text(x = c(sum(mp[1:2])/2, sum(mp[3:4])/2), y = usr[3] - 0.15 * ydiff,
#      labels = c("Before", "After"), xpd = T, cex = 1.2, font = 1)
# mtext(side = 2, "Total LMB Catch Per Impoundment", line = 3, cex = 1.2)
# text(x = sum(mp[1:2])/2, y = 300, label = "p = 0.37", font = 3)
# text(x = sum(mp[3:4])/2, y = 75, label = "p < 0.001", font = 3)
# dev.off()

# # line plot
# x = c(1,2)
# xc = x + 0.03
# xt = x - 0.03
# ### Change pvalue and % changes when running updated dataset ###
# png("lmb_day1_follwup.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.axis = 1.2, cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "",
#      xlim = c(0.75,2.25), ylim = c(0, 380),las = 1, xaxt = "n", yaxt = "n")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
# arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], col = "blue", length = 0)
# arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], col = "red", length = 0)
# axis(side = 1, at = x, labels = rep("", 2))
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 1\nPre-Treatment", "Day 42\nFollow-up"), xpd = T, cex = 1.3)
# text(x = usr[2], y = usr[4] - ydiff * 0.1, pos = 2, cex = 1.2,
#      label = paste("Control Decrease: ", abs(round(percent_c)), "%", sep = ""), col = "blue")
# text(x = usr[2], y = usr[4] - ydiff * 0.17, pos = 2, cex = 1.2,
#      label = paste("Treatment Decrease: ", abs(round(percent_t)), "%", sep = ""), col = "red")
# text(x = usr[2], y = usr[4] - ydiff * 0.24,
#      labels = "Treatment x Time Interaction: p = 0.006", col = "black", pos = 2, cex = 1.2)
# text(x = usr[2], y = usr[4] - ydiff * 0.31, 
#      labels = "Additional 26% Decrease", col = "black", pos = 2, cex = 1.2)
# mtext(side = 2, "Total LMB Catch Per Impoundment", line = 2.8, cex = 1.3)
# axis(side = 2, at = seq(0, 380, 50), labels = seq(0, 380, 50), las = 2)
# dev.off()

# # log() line plot
# x = c(1,2)
# xc = x + 0.03
# xt = x - 0.03
# ### Change pvalue and % changes when running updated dataset ###
# png("glmer_lmb_day1_follwup_log.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.axis = 1.2, cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", 
#      xlim = c(0.75,2.25), ylim = c(0, 6),las = 1, xaxt = "n", yaxt = "n")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
# arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], col = "blue", length = 0)
# arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], col = "red", length = 0)
# axis(side = 1, at = x, labels = rep("", 2))
# text(x = x, y = usr[3] - ydiff * 0.1,
#      labels = c("Day 1\nPre-Treatment", "Day 42\nFollow-up"), xpd = T)
# text(x = usr[2], y = usr[4] - ydiff * 0.81, pos = 2, cex = 1.2,
#      label = paste("Control Decrease: ", abs(round(percent_c)), "%", sep = ""), col = "blue")
# text(x = usr[2], y = usr[4] - ydiff * 0.88, pos = 2, cex = 1.2, 
#      label = paste("Treatment Decrease: ", abs(round(percent_t)), "%", sep = ""), col = "red")
# text(x = usr[2], y = usr[4] - ydiff * 0.95,
#      labels = "Treatment x Time Interaction: p = 0.006", col = "black", pos = 2, cex = 1.2)
# text(x = usr[2], y = usr[4] - ydiff * 0.31, 
#      labels = "Additional 26% Decrease", col = "black", pos = 2, cex = 1.2)
# mtext(side = 2, "log(Total LMB Catch Per Impoundment)", line = 2.8, cex = 1.2)
# axis(side = 2, at = seq(0, 6, 1), labels = seq(0, 6, 1), las = 2)
# dev.off()









##### BACI ANALYSIS: MLA-0 DAY-1 TO FOLLOW-UP ------------------------------------------------------
datsub = datum[datum$period %in% c("before", "followup") & datum$event %in% c(1,3),]
datsub = droplevels(datsub)
datsub = datsub[!(is.na(datsub$tl)),]
unique(datsub$event)
unique(datsub$period)

datsub = datsub[datsub$size_class %in% "small",]    # for small sized impoundment analysis
datsub = datsub[datsub$size_class %in% "big",]      # for large sized impoundment analysis

df = aggregate(datsub['tl'], list(lake = datsub$lake, period = datsub$period, 
                                  type = datsub$type, event = datsub$event, 
                                  year = datsub$year), mean)
# df = df[df$lake != "promise",]
df$lakeyr = paste(df$lake, df$year, sep='')   # only needed for glmmPQL - used to get df


hist(df$tl)
hist(log(df$tl))

# fit_mla0 = lmer(log(tl) ~ period * type * size + (1 | lake) + (1 | year), data = df, REML = F)
fit_mla0 = lmer(log(tl) ~ period * type + (1 | lake) + (1 | year), data = df, REML = F)
# fit_mla0 = lmer(log(tl) ~ relevel(period, ref = "followup") * type + (1 | lake) + (1 | year), data = df, REML = F)
# fit_mla0 = lmer(log(tl) ~ period + type + (1 | lake) + (1 | year), data = df, REML = F)

summary(fit_mla0)
anova(fit_mla0)
coefs = summary(fit_mla0)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

fit = glmmPQL(log(tl) ~ period * type, random = ~1|lakeyr, data = df, family = "quasipoisson")
summary(fit)

x = fitted(fit_mla0)
y = residuals(fit_mla0)
plot(y ~ x)

## plot ##
dat = datsub

yvar = "tl"

datsub = dat[,c("year", "type", "period", yvar)]
datsub = na.omit(datsub)


library(dplyr)

plot_dat = datsub %>% group_by(year, type, period) %>%
  summarize(
    mn = mean(tl, na.rm = T),
    lwr = quantile(tl, 0.025, na.rm = T),
    upr = quantile(tl, 0.975, na.rm = T)
  ) %>% ungroup

plot_dat$year = as.numeric(plot_dat$year)
plot_dat

plot_dat = data.frame(plot_dat)
m <- matrix(NA, 2, 4)
m[1,1] = plot_dat[1, 4]

m[2,1] = plot_dat[2, 4]

m[1,2] = plot_dat[3, 4]

m[2,2] = plot_dat[4, 4]

m[1,3] = plot_dat[5, 4]

m[2,3] = plot_dat[6, 4]

m[1,4] = plot_dat[7, 4]

m[2,4] = plot_dat[8, 4]


ppi = 600
# png("mla0.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(4,4,1,1))
mp <- barplot(m, beside = T, ylim = c(0, max(plot_dat$upr)), las = 1, cex.axis = 1.2,
              main = "", cex.main = 2, 
              col = c("blue", "blue", "red", "red", "blue", "blue", "red", "red"),
              density = c(1000, 20, 1000, 20, 1000, 20, 1000, 20),
              space = c(0.1, 0, 0.3, 0, 1, 0, 0.3, 0))
mtext(side = 2, "LMB Age-0 Length (mm)", line = 2.5, cex = 1.75)
mp <- as.numeric(mp) 
mp <- mp[1:8]

arrows(mp[1], plot_dat[1, 5], mp[1], plot_dat[1, 6], length = 0)
arrows(mp[2], plot_dat[2, 5], mp[2], plot_dat[2, 6], length = 0)
arrows(mp[3], plot_dat[3, 5], mp[3], plot_dat[3, 6], length = 0)
arrows(mp[4], plot_dat[4, 5], mp[4], plot_dat[4, 6], length = 0)
arrows(mp[5], plot_dat[5, 5], mp[5], plot_dat[5, 6], length = 0)
arrows(mp[6], plot_dat[6, 5], mp[6], plot_dat[6, 6], length = 0)
arrows(mp[7], plot_dat[7, 5], mp[7], plot_dat[7, 6], length = 0)
arrows(mp[8], plot_dat[8, 5], mp[8], plot_dat[8, 6], length = 0)

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

# axis(side = 1, at = mp, labels = c("1", "42", "1", "42", "1", "42", "1", "42"))
mtext(side = 1, c("2017"), line = 1.5, at = 2.25, cex = 1.5)
mtext(side = 1, c("2018"), line = 1.5, at = 7.55, cex = 1.5)
abline(lty = 2, lwd = 2, v = 4.9)

segments(mp, plot_dat$lwr, mp, plot_dat$upr, lwd = 1.5)

legend("top", col = c("blue", "red"), legend = c("Day-1", "Day-42"), density = c(1000, 30), 
       cex = 1.2)
# dev.off()
# legend("topleft", legend = c("Control", "Treated Once", "Treated Twice"),
#        col = c("blue", "red", "red"), lty = c(1,1,2), seg.len = 3, lwd = 2, bty = "n", cex = 1.3)




##### T-TEST ANALYSIS: FOLLOW-UP MEAN TL IN CONTROLS VS TREATS -------------------------------------
# follow_tl = datum
# follow_tl = follow_tl[follow_tl$period == "followup",]    # keep only followup data
# follow_tl = follow_tl[!is.na(follow_tl$tl),]              # remove NA's; NA = no fish at that site
# follow_tl = follow_tl[,c("lake", "year", "type",
#                          "period", "tl", "size_class")]  # keep only these columns for ttest
# control = follow_tl[which(follow_tl$type == "control"),] # call out controls
# treat = follow_tl[which(follow_tl$type == "treat"),]     # call out treatments
# 
# t.test(control$tl, treat$tl)                             # ttest

# split out big and small lakes, get the same results 
# bigc = control[which(control$size_class == "big"),]
# smallc = control[which(control$size_class == "small"),]
# bigt = treat[which(treat$size_class == "big"),]
# smallt = treat[which(treat$size_class == "small"),]
# 
# t.test(bigc$tl, bigt$tl)
# boxplot(bigc$tl, bigt$tl)
# t.test(smallc$tl, smallt$tl)
# boxplot(smallc$tl, smallt$tl)

# ## plot ##
# ppi = 600
# png("lmb_mla_followup.png", h = 6.5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.3)
# boxplot(control$tl, treat$tl, ylab = "",
#         col = c("blue", "red"), whiskcol = c("blue", "red"), staplecol = c("blue", "red"),
#         names = c("Control", "Treatment"), las = 1, ylim = c(20,110), yaxt = "n", outline = F)
# mtext(side = 2, "LMB Total Length (mm)", line = 2.8, cex = 1.3)
# axis(2, seq(20, 100, 20), las = 1, cex = 1.2)
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# text(x = usr[2] - xdiff * .75, y = usr[4] - ydiff * 0.075,
#      labels = "p < 0.001", col = "black", pos = 1, cex = 1.35)
# dev.off()







##### OLD STUFF; EVERYTHING BELOW IS # OUT ----------------------------------------------------
# fit = glm(count ~ lake + period + treat+ treat*period, data = df, family = quasipoisson)
# fit.plot = glmer.nb(count ~ period + type + type:period + (1|lake), data = df)
# fit = glmer.nb(count ~ period + treat + treat:period + (1|lake), data = df)
# fit = glmer.nb(count ~ type + period + treat + treat:period + type:treat + type:period + (1|lake),
#                data = df)
# df2 = df
# df2$count = df2$count + 1
# fit = lmer(log(count) ~ period + type + type:period + (1 | lake) + (1 | year),
#            data = df2, REML = F)
# summary(fit)
### Matt model
## figure out what PQL is. Matt could not explain 
# fit = glmmPQL(count ~ treat + period  + treat:period, random=~1|lakeyr,data = df,
#               family = quasipoisson)
# fit = glmmPQL(count ~ relevel(type, ref = 'control') + relevel(period, ref = 'before')  +
#                 relevel(type, ref = 'control'):relevel(period, ref = 'before'), random=~1|lakeyr,
#               data = df,
#               family = quasipoisson)
# summary(fit.plot)
# summary(fit)

### this is removed with the addtion of type to dataset ###
### df = data.frame(count = unname(counts), lake = names(counts))
# df$treat = rep(c("control", "control", "treatment", "control", "control", "control", "treatment",
#                  "control", "treatment", "treatment", "treatment", "treatment"),2)
# df$type = rep(c("pond", "pond", "lake", "pond", "lake", 
#                 "pond", "pond", "lake", "pond", "lake", "pond", "lake"),2)

## remove this if leaving event out of the model for day 1 to day 2 analysis
### remove fp3, 2018, event 1
# datum = datum[!(datum$lake == "fp3" & datum$year == '2018'),]

### Matt model for day1 - day 2 analysis
# fit = glmmPQL(count ~ type + period  + type:period, random=~1|lakeyrevent,data = df,
#               family = quasipoisson)
# summary(fit)
# fit = glmer(count ~ period + type + type:period + (1 | lake:year:event), data = df,
#             family=MASS::negative.binomial(theta=1.75),
#             glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 500000000)))
# ## lake, year, and event treated as Fixed effects
# fit = glm.nb(count ~ period + type + lake + year + event + type:period + lake:year:event, data = df)
# summary(fit)
# hist(log(df$count))
# df2 = df
# df2$count = df2$count + 1
# fit = lmer(log(count) ~ period * type * event + (1 | lake) + (1 | year), data = df2, REML = F)


##### ae1
datsub <- datum[datum$lake == "ae1",]
unique(datum$lake)
unique(datsub$year)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(3,4,5,6,10,1,2,8,9,7)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
summary(counts)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18ae1.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "AE1 (Control; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

##### s3 
datsub <- datum[datum$lake == "s3",]
unique(datsub$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,4,5,3)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
summary(counts)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]


ppi = 600
png("18s3.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "S3 (Treatment; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

### length freq

datsub$tlgroup = as.numeric(as.character(datsub$tlgroup))
tab <- table(datsub$tlgroup, datsub$period)
m <- matrix(0, 6, 3)
rownames(m) = c(0, 25, 50, 75, 100, 125)
m[rownames(tab),] = tab
m <- apply(m, 2, function(x) x/sum(x))
m <- m[, 1:2]
m <- t(m)
m <- m[c(2,1),]
barplot(m, beside = T)

##### dead 
datsub <- datum[datum$lake == "dead",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,4,5,3)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
summary(counts)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18dead.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Dead (Treatment; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

###length frequency

unique(datsub$date)
datsub$tlgroup = as.numeric(as.character(datsub$tlgroup))
# tab <- table(datsub$tlgroup[datsub$date %in% c("5/15", "5/16")], 
#              datsub$period[datsub$date %in% c("5/15", "5/16")])
tab <- table(datsub$tlgroup[datsub$date %in% c("6/5", "6/6")], 
             datsub$period[datsub$date %in% c("6/5", "6/6")])

m <- matrix(0, 6, 3)
rownames(m) = c(0, 25, 50, 75, 100, 125)
m[rownames(tab),] = tab
m <- apply(m, 2, function(x) x/sum(x))
m <- m[, 1:2]
m <- t(m)
m <- m[c(2,1),]
barplot(m, beside = T)

##### promise 

datsub <- datum[datum$lake == "promise",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,4,5,3)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
summary(counts)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18promise.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Promise (Control; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

### length frequency

datsub$tlgroup = as.numeric(as.character(datsub$tlgroup))
tab <- table(datsub$tlgroup, datsub$period)
m <- matrix(0, 6, 3)
rownames(m) = c(0, 25, 50, 75, 100, 125)
m[rownames(tab),] = tab
m <- apply(m, 2, function(x) x/sum(x))
m <- m[, 1:2]
m <- t(m)
m <- m[c(2,1),]
barplot(m, beside = T)

##### drummond1 

datsub <- datum[datum$lake == "drummond1",]
unique(datsub$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,4,5,3)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
summary(counts)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]


ppi = 600
png("18drummond1.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Drummond 1 (Treatment; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

##### drummond3 

datsub <- datum[datum$lake == "drummond3",]
unique(datsub$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,4,5,3)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
summary(counts)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]


ppi = 600
png("18drummond3.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Drummond 3 (Control; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

##### monroe
datsub <- datum[datum$lake == "monroe county lake",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18monroe.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Monroe (Treatment; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100


##### dale 

datsub <- datum[datum$lake == "dale county lake",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18dale.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Dale (Control; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100




##### little pit 

datsub <- datum[datum$lake == "little pit",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18littlepit.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Little Pit (Treatment; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100


##### big pit

datsub <- datum[datum$lake == "big pit",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18bigpit.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Big Pit (Control; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100


##### horseshoe pond 

datsub <- datum[datum$lake == "horseshoe pond",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18horseshoe.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Horseshoe Pond (Treatment; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100



##### fp3 

datsub <- datum[datum$lake == "fp3",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18fp3.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "FP3 (Control; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2.2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

##### britton 

datsub <- datum[datum$lake == "britton",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
summary(counts)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18britton.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Britton (Treatment; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

##### lm 

datsub <- datum[datum$lake == "lm",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18lm.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "LM (Control; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2.2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

##### griggs 

datsub <- datum[datum$lake == "griggs2",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18griggs.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Griggs (Treatment; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2.2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100

##### mark

datsub <- datum[datum$lake == "mark",]
unique(datum$lake)

fdates = factor(datsub$date)
levels(fdates)
fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
levels(fdates2)
datsub$date = fdates2

counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
means <- counts/15
datsub$date
unique(datum$date)

m <- matrix(NA, 2, 3)
m[1,1] = means["before", 1]

m[2,1] = means["after", 2]

m[1,2] = means["before", 3]

m[2,2] = means["after", 4]

m[1,3] = means["followup", 5]

ppi = 600
png("18mark.png", h = 3*ppi, w = 7*ppi, res = ppi)
par(mar = c(2,4,3,1))
mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
              main = "Mark (Control; 2018)", cex.main = 2, 
              col = c("black", "grey", "black", "grey", "white"))
mtext(side = 2, "LMB Catch/Haul", line = 2.2, cex = 1.5, font = 2)
mp <- as.numeric(mp) 
mp <- mp[1:5]

usr <- par("usr")
segments(usr[1], usr[3], usr[2], usr[3], xpd = T)

axis(side = 1, at = mp, labels = colnames(means))
dev.off()

((m[2,] - m[1,]) / m[1,]) * 100







##### PERCENT CHANGE DAY 1 VS DAY 2 LMB ------------------------------------------------------------
# counts = with(datum, tapply(count, list(lake, period, event), sum))   # among years
# 
# e1.perc = ((counts[,"after",1] - counts[,"before",1])/counts[,"before",1]) * 100
# e2.perc = ((counts[,"after",2] - counts[,"before",2])/counts[,"before",2]) * 100
# 
# perc = (e1.perc + e2.perc)/2
# 
# df = data.frame(perc = unname(perc), lake = names(perc))
# 
# # na = 0
# df[is.na(df$perc)==T,1]<-0
# 
# levels(datum$lake)
# treat = c("control", "control", "treatment", "control", "treatment", "treatment",
#           "control", "control", "treatment", "treatment", "treatment", "control",
#           "control", "treatment", "control",
#           "treatment")
# type = c("pond", "pond", "pond", "lake", "pond", "pond", "pond", "pond",
#          "pond", "pond", "pond", "pond", "pond", "lake", "pond", "pond")
# 
# df$type = type
# df$treatment = treat
# 
# mn = as.numeric(tapply(df$perc, list(df$treatment, df$type), mean))
# high = as.numeric(tapply(df$perc, list(df$treatment, df$type), min))
# low = as.numeric(tapply(df$perc, list(df$treatment, df$type), max))
# 
# x <- 1:4
# png("18percentchange_lmb.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(3, 4, 1, 1))
# plot(mn~x, ylim = c(-100, 150), las = 1, ylab = "", xlab = "",
#      pch = 16, xaxt = "n", cex = 2,
#      col = c("blue", "red", "blue", "red"))
# mtext(side = 2, line = 2.25, cex = 1.5, font = 2, "% Change Catch/Haul")
# axis(side = 1, at = x, labels = rep("", 4))
# text(x = x, y = rep(-130, 4), c("Control", "Treatment", "Control",
#                                 "Treatment"), xpd = T)
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# text(usr[1], usr[4] - ydiff * 0.09, "STATE\nLAKES", cex = 1.5, font = 2, pos = 4)
# text(usr[2], usr[4] - ydiff * 0.05, "PONDS", cex = 1.5, font = 2, pos = 2)
# abline(v = 2.5)
# arrows(x, low, x, high, code = 3, angle = 90, length = .1,
#        col = c("blue", "red", "blue", "red"))
# abline(h = 0, lty = 2, col = "grey")
# dev.off()