###SUNFISH###
rm(list = ls(all = T))

# datum <- read.csv(file.choose())
# setwd("~/Documents/Projects/Rotenone/Data")
datum <- read.csv("data/3.27.19data.csv")
unique(datum$lake)
head(datum)
tail(datum)
summary(datum)
datum[is.na(datum$lake),]
datum <- datum[!is.na(datum$lake),]
unique(datum$lake)
unique(datum$species)
ppi = 600

datum <- datum[datum$gear != "efishing",]               # remove efishing data
datum = datum[datum$species == "blue",]                 # keep only blue
datum <- droplevels(datum)                              # remove empty cells
datum$date <- paste(datum$month, datum$day, sep = "/")  # add a date column
unique(datum$date)
remove.type = "type"            # remove type because it is duplicated when we combine below
datum = datum[, ! names(datum) %in% remove.type,
              drop = FALSE]     # remove type from datum
unique(datum$date)
# counts = with(datum, tapply(count, list(lake, period, event), sum))

# combine lake id data with fish data
datum = datum[order(datum$lake, datum$year),]                 # orders datum by lake and yr       
ids = read.csv("data/lake_year_ids.csv", stringsAsFactors = F)     # read in id file
datum = merge(ids, datum, by = c("lake", "year"))             # merge datum and id files by lake and year
datum = datum[datum$year != 2016,]                            # take out 2016 data
datum$year = as.character(datum$year)                    
datum$times_treat = as.factor(datum$times_treat)         
datum$size_class = ifelse(datum$size >30, "big", "small")             # make size categorical 
datum$period = relevel(factor(datum$period), ref = "before")          # make "before" ref group
datum$type = relevel(factor(datum$type), ref = "control")             # make "control" ref group
datum$size_class = relevel(factor(datum$size_class), ref = "small")   # this might not work?

# datum = subset(datum, select = -c(age1, age2, age, NOTES, X, wt, month, day,
#                                   hour, min, gear, effort, status, species, id, structure))
# write.csv(datum, "blue_seine.csv", row.names = FALSE)        # write dataset into a .csv to send

# Analyses # 

##### BACI ANALYSIS: DAY 1 VS DAY 2 blue --------------------------------------------------------------------
datsub = datum[datum$period %in% c("before", "after") & datum$event %in% c(1,2), ]
# datsub = datsub[datsub$size_class %in% "small",]   # for small sized impoundment analysis
datsub = datsub[datsub$size_class %in% "big",]     # for large sized impoundment analysis
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

## Models used for manuscript
fit = glmer.nb(count ~ period * type * event + (1 | lake:year), data = df,
               glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 100000000)))
summary(fit)
anova(fit)
fit = glmmPQL(count ~ period * type * event, random =  ~1 | lakeyr,
              data = df, family = "quasipoisson")   # use this to get df
summary(fit)

fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df,
               glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 100000000)))
summary(fit)
anova(fit)
fit = glmmPQL(count ~ period * type, random =  ~1 | lakeyr,
              data = df, family = "quasipoisson")   # use this to get df
summary(fit)
#

##### Old Models ##### -----------------------------------------------------------------------
# df$count = log(df$count + 1)
# hist(df$count)
# fit = glmer.nb(count ~ period * type * event + (1 | lake:year), data = df,                
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# # use for large SIs
# fit = glmer.nb(count ~ period * type * event + (1 | lake:year),data=df, nAGQ = 1L, start = 0.21,
#                control=glmerControl(optimizer="bobyqa",
#                                     boundary.tol=1e-2,
#                                     check.conv.singular =.makeCC(action="ignore",tol=1e-2),
#                                     tolPwrss=1e-2))
# fit = glmmPQL(count ~ period * type * event, random =  ~1 | lakeyr, 
#               data = df, family = "quasipoisson") # use this to get df
# 
# fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# # use for large SIs
# fit = glmer.nb(count ~ period * type + (1 | lake:year),data=df, nAGQ = 1L, start = 0.21,
#                control=glmerControl(optimizer="bobyqa",
#                                     boundary.tol=1e-2,
#                                     check.conv.singular =.makeCC(action="ignore",tol=1e-2),
#                                     tolPwrss=1e-2))
# fit = glmmPQL(count ~ period * type, random =  ~1 | lakeyr,
#               data = df, family = "quasipoisson")  # use this to get df

## lake, year treated as Random effects
### Matt model
# fit = glmmPQL(count ~ period * type * event * size, random=~1|lakeyr,data = df,
#               family = quasipoisson)
# fit = glmmPQL(count ~ period * type * event, random=~1|lakeyr,data = df,
#               family = quasipoisson)
# fit = glmmPQL(count ~ period * type, random=~1|lakeyr,data = df,
#               family = quasipoisson)
# summary(fit)

# fit = glmer(count ~ period + type + type:period + (1 | lake:year:event), data = df,
#             family=MASS::negative.binomial(theta=1.75), glmerControl(optimizer = "bobyqa",
#                                                                      optCtrl = list(maxfun = 500000000)))
## lake, year, and event treated as Fixed effects
# fit = glm.nb(count ~ period + type + lake + year + event + type:period + lake:year:event, data = df)
# summary(fit)

## Models used
# fit = glmer.nb(count ~ period * type * event * size + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * type * event + (1 | lake:year), data = df,  # use this to plot by applicaiton
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * type * relevel(event, ref = 2) + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ relevel(period, ref = 'after') + relevel(type, ref = 'treat') + event +
#                  relevel(period, ref = 'after'):relevel(type, ref = 'treat') + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * relevel(type, ref = "treat") * relevel(event, ref = 2) + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * relevel(type, ref = "treat") * event + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * relevel(type, ref = "treat") * relevel(event, ref = "2") + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * type * relevel(event, ref = "2") + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glm(count ~ period * type, data = df)
##### ##### ---------------------------------------------------------------------------------

coefs = summary(fit)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s

plot(exp(coefs[2:4,"Estimate"]), ylim = range(lwrs[2:4], uprs[2:4]))
segments(1:7, lwrs[2:4], 1:7, uprs[2:4])
abline(h = 1)
# plot(exp(coefs[2:8,"Estimate"]), ylim = range(lwrs[2:8], uprs[2:8]))
# segments(1:7, lwrs[2:8], 1:7, uprs[2:8])
# abline(h = 1)

df$pred = exp(predict(fit, re.form = NA))

means = tapply(df$pred, list(df$period, df$type, df$event), mean)

means["after","control",1]/means["before","control",1]  # period effects by treat level
means["after","treat",1]/means["before","treat",1]      

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

## this function is to seperate events 
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

start = Sys.time()
rand = bootMer(x = fit, FUN = myfunc, nsim = 1000)$t
Sys.time() - start

# ## remove rows with missing data
# complete.cases(rand)
# rand = rand[complete.cases(rand), ]

summ = apply(rand, 2, function(x) c(mean = mean(na.omit(x)), quantile(na.omit(x), c(0.025, 0.975))))
# summ = log(summ)   # log() plot
## percent change event 1; c = control, t = treatment
percentc1 = ((summ[1, 3] - summ[1, 1]) / summ[1, 1]) * 100
percentt1 = ((summ[1, 4] - summ[1, 2]) / summ[1, 2]) * 100
## percent change event 2; c = control, t = treatment
percentc2 = ((summ[1, 7] - summ[1, 5]) / summ[1, 5]) * 100
percentt2 = ((summ[1, 8] - summ[1, 6]) / summ[1, 6]) * 100

# ## line plot ##
# ## event 1 and 2
# x = c(0.9,2.1)
# xc = x + 0.03
# xt = x - 0.03
# xc2 = x + 0.06
# xt2 = x - 0.06
# png("blue_day1_day2.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", 
#      xlim = c(0.75,2.25), ylim = c(0, 2750),las = 1, xaxt = "n", yaxt = "n")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
# arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], 
#        col = "blue", length = 0, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], 
#        col = "red", length = 0, cex = 1.2, lwd = 3, pch = 16)
# points(summ[1,c(5,7)] ~ xc2, type = "l", pch = 16, lty = 2, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(6,8)] ~ xt2, type = "l", pch = 2, lty = 2, lwd = 3, cex = 1.2, col = "red")
# arrows(xc2, summ[2,c(5,7)], xc2, summ[3,c(5,7)],
#        col = "blue", length = 0, lty = 2, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt2, summ[2,c(6,8)], xt2, summ[3,c(6,8)],
#        col = "red", length = 0, lty = 2, cex = 1.2, lwd = 3, pch = 16)
# mtext(side = 2, "Total Bluegill Catch Per Impoundment", line = 2.95, cex = 1.2)
# axis(side = 2, at = seq(0, 2800, 500), labels = seq(0, 2800, 500), las = 1)
# axis(side = 1, at = x, labels = rep("", 2))
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 1 & 21\nPre-Treatment", "Day 2 & 22\nPost-Treatment"), xpd = T)
# legend("top", legend = c("1", "2"), lty = c(1, 2), 
#        bty = "n", lwd = 2, title = "Application", seg.len = 3)
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.20, pos = 4, 
#      label = paste("App. 1 Control Decrease: ", abs(round(percentc1)), "%", sep = ""), col = "blue")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.25, pos = 4, 
#      label = paste("App. 1 Treatment Decrease: ", abs(round(percentt1)), "%", sep = ""), col = "red")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.30, pos = 4, 
#      label = paste("App. 2 Control Decrease: ", abs(round(percentc2)), "%", sep = ""), col = "blue")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.35, pos = 4, 
#      label = paste("App. 2 Treatment Decrease: ", abs(round(percentt2)), "%", sep = ""), col = "red")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.40, pos = 4, 
#      label = paste("Treatment x Time Interaction: p = 0.0018"), col = "black")
# dev.off()
# 
# # log() line plot #
# # event 1 and 2
# x = c(0.9,2.1)
# xc = x + 0.03
# xt = x - 0.03
# xc2 = x + 0.06
# xt2 = x - 0.06
# png("glmer_blue_day1_day2_log.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", 
#      xlim = c(0.75,2.25), ylim = c(0, 8.2),las = 1, xaxt = "n", yaxt = "n")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
# arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], col = "blue", length = 0, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], col = "red", length = 0, cex = 1.2, lwd = 3, pch = 16)
# # points(summ[1,c(5,7)] ~ xc2, type = "l", pch = 16, lty = 2, lwd = 3, cex = 1.2, col = "blue")
# # points(summ[1,c(6,8)] ~ xt2, type = "l", pch = 2, lty = 2, lwd = 3, cex = 1.2, col = "red")
# arrows(xc2, summ[2,c(5,7)], xc2, summ[3,c(5,7)], 
#        col = "blue", length = 0, lty = 2, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt2, summ[2,c(6,8)], xt2, summ[3,c(6,8)], 
#        col = "red", length = 0, lty = 2, cex = 1.2, lwd = 3, pch = 16)
# mtext(side = 2, "log(Total Bluegill Catch Per Impoundment)", line = 2.8, cex = 1.2)
# axis(side = 2, at = seq(0, 8.2, 1), labels = seq(0, 8.2, 1), las = 1)
# axis(side = 1, at = x, labels = rep("", 2))
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 1 & 21\nPre-Treatment", "Day 2 & 22\nPost-Treatment"), xpd = T)
# legend("top", legend = c("1", "2"), lty = c(1, 2), bty = "n", 
#        lwd = 2, title = "Application", seg.len = 3)
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.20, pos = 4, 
#      label = paste("App. 1 Control Decrease: ", abs(round(percentc1)), "%", sep = ""), col = "blue")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.25, pos = 4, 
#      label = paste("App. 1 Treatment Decrease: ", abs(round(percentt1)), "%", sep = ""), col = "red")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.30, pos = 4, 
#      label = paste("App. 2 Control Decrease: ", abs(round(percentc2)), "%", sep = ""), col = "blue")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.35, pos = 4, 
#      label = paste("App. 2 Treatment Decrease: ", abs(round(percentt2)), "%", sep = ""), col = "red")
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.40, pos = 4, 
#      label = paste("Treatment x Time Interaction: p = 0.0018"), col = "black")
# dev.off()
# 
# ## event 1
# x = c(0.9,2.1)
# xc = x + 0.03
# xt = x - 0.03
# xc2 = x + 0.06
# xt2 = x - 0.06
# png("blue_day1_day2_app1.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", 
#      xlim = c(0.75,2.25), ylim = c(0, 2750),las = 1, xaxt = "n", yaxt = "n", main = "Application 1")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
# arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], col = "blue", length = 0, cex = 1.2, lwd = 3, pch = 16)
# arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], col = "red", length = 0, cex = 1.2, lwd = 3, pch = 16)
# mtext(side = 2, "Total Bluegill Catch Per Impoundment", line = 2.8, cex = 1.2)
# axis(side = 2, at = seq(0, 2800, 500), labels = seq(0, 2800, 500), las = 2)
# axis(side = 1, at = x, labels = rep("", 2))
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 1\nPre-Treatment", "Day 2\nPost-Treatment"), xpd = T)  # only event 1
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.1, pos = 4, 
#      label = paste("App. 1 Control Decrease: ", abs(round(percentc1)), "%", sep = ""), 
#      col = "blue")   # only event 1
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.15, pos = 4, 
#      label = paste("App. 1 Treatment Decrease: ", abs(round(percentt1)), "%", sep = ""), 
#      col = "red")  # only event 1
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.20, pos = 4,
#      label = paste("Treatment x Time Interaction: p = 0.0018"), col = "black")   # only event 1
# dev.off()
# 
# 
# ## event 2
# x = c(0.9,2.1)
# xc = x + 0.03
# xt = x - 0.03
# xc2 = x + 0.06
# xt2 = x - 0.06
# png("blue_day1_day2_app2.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.lab = 1.2)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", 
#      xlim = c(0.75,2.25), ylim = c(0, 2750),las = 1, 
#      xaxt = "n", yaxt = "n", main = "Application 2")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# axis(side = 2, at = seq(0, 2800, 500), labels = seq(0, 2800, 500), las = 2)
# axis(side = 1, at = x, labels = rep("", 2))
# mtext(side = 2, "Total Bluegill Catch Per Impoundment", line = 2.8, cex = 1.2)
# points(summ[1,c(5,7)] ~ xc2, type = "l", pch = 16, lty = 2, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(6,8)] ~ xt2, type = "l", pch = 2, lty = 2, lwd = 3, cex = 1.2, col = "red")
# arrows(xc2, summ[2,c(5,7)], xc2, summ[3,c(5,7)], 
#        col = "blue", length = 0, lty = 2, cex = 1.2, 
#        lwd = 3, pch = 16)
# arrows(xt2, summ[2,c(6,8)], xt2, summ[3,c(6,8)], 
#        col = "red", length = 0, lty = 2, cex = 1.2, 
#        lwd = 3, pch = 16)
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.10, pos = 4, 
#      label = paste("App. 2 Control Decrease: ", abs(round(percentc2)), "%", sep = ""), 
#      col = "blue")  # only event 2
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.15, pos = 4,
#      label = paste("App. 2 Treatment Decrease: ", abs(round(percentt2)), "%", sep = ""),
#      col = "red") # only event 2
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 21\nPre-Treatment", "Day 22\nPost-Treatment"), xpd = T)   # only event 2
# text(x = usr[1] + xdiff * 0.28, y = usr[4] - ydiff * 0.20, pos = 4, 
#      label = paste("Treatment x Time Interaction: p = 0.0018"), 
#      col = "black")    # only event 2
# dev.off()










##### BACI ANALYSIS: Day one Follow up GLMER.NB ---------------------------------------------------------------------------
datsub = datum[datum$period %in% c("before", "followup") & datum$event %in% c(1,3), ]
datsub = datsub[datsub$size_class %in% "small",]    # for small sized impoundment analysis
datsub = datsub[datsub$size_class %in% "big",]      # for large sized impoundment analysis
unique(datsub$event)
unique(datsub$size_class)

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
df$lakeyr = paste(df$lake, df$year, sep='')   # only needed for glmmPQL - used to get df
# table(df$period, df$lake, df$year)

### New Models for Manuscript ###
# small 
fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df,
               glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * relevel(type, ref = "treat") + (1 | lake:year), data = df, 
#                glmerControl(optimizer = "Nelder_Mead", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
summary(fit)
anova(fit)
fit = glmmPQL(count ~ period * type, random = ~1|lakeyr, data = df, family = "quasipoisson")
summary(fit)

# fit = glmer.nb(count ~ period * relevel(type, ref = "treat") + (1 | lake:year), data = df,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmmPQL(count ~ period * relevel(type, ref = "treat"), 
#               random = ~1|lakeyr, data = df, family = "quasipoisson")

# large
# fit = glm.nb(count ~ period * type, data = df)
# fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df, nAGQ = 1L, start = 0.21,
#                control=glmerControl(optimizer="nloptwrap",
#                                     boundary.tol=1e-2,
#                                     check.conv.singular =.makeCC(action="ignore",tol=1e-2),
#                                     tolPwrss=1e-2))
fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df, nAGQ = 1L, start = 0.21,
               control=glmerControl(optimizer="nloptwrap",
                                    boundary.tol=1e-2,
                                    check.conv.singular =.makeCC(action="ignore",tol=1e-2),
                                    tolPwrss=1e-2))
summary(fit)
anova(fit)
# fit = glmer.nb(count ~ period * relevel(type, ref = "treat") + (1 | lake:year), 
#                data = df, nAGQ = 1L, start = 0.21,
#                control=glmerControl(optimizer="nloptwrap",
#                                     boundary.tol=1e-2,
#                                     check.conv.singular =.makeCC(action="ignore",tol=1e-2),
#                                     tolPwrss=1e-2))
summary(fit)

fit = glmmPQL(count ~ period * type, random = ~1|lakeyr, data = df, family = "quasipoisson")
summary(fit)
# fit = glmmPQL(count ~ period * relevel(type, ref = "treat"), random = ~1|lakeyr, 
#               data = df, family = "quasipoisson")


## Old Models 
# summary(glmmPQL(count ~ period * type * size, random = ~1|lakeyr, data = df, family = "quasipoisson"))
# summary(glmmPQL(count ~ period * type, random = ~1|lakeyr, data = df, family = "quasipoisson"))
# summary(glmmPQL(count ~ period + type, random = ~1|lakeyr, data = df, family = "quasipoisson"))

## fit data to model
# fit = glmer.nb(count ~ period * type * size + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period + type + type:period + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period + type + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ period + relevel(type, ref = "treat") +  relevel(type, ref = "treat"):period + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))
# fit = glmer.nb(count ~ relevel(period, ref = "followup") + type +  type:relevel(period, ref = "followup") + (1 | lake:year),
#                data = df, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000000)))

summary(fit)
anova(fit)
coefs = summary(fit)$coef
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


# run this fit for large before below; same results as with glm.nb
fit = glmer.nb(count ~ period * type + (1 | lake:year), data = df, nAGQ = 1L, start = 0.21,
               control=glmerControl(optimizer="nloptwrap",
                                    boundary.tol=1e-2,
                                    check.conv.singular =.makeCC(action="ignore",tol=1e-2),
                                    tolPwrss=1e-2))

# bootstrap func for CIs
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


summ = apply(rand, 2, function(x) c(mean = mean(x, na.rm = TRUE), quantile(x, c(0.025, 0.975),na.rm = TRUE)))
summ = log(summ)   # log() plot
## percent change; c = control, t = treatment
percent_c = ((summ[1, 3] - summ[1, 1]) / summ[1, 1]) * 100
percent_t = ((summ[1, 4] - summ[1, 2]) / summ[1, 2]) * 100

# # line plot
# x = c(1,2)
# xc = x + 0.03
# xt = x - 0.03
# ### Change pvalue ###
# png("blue_day1_follwup.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(4,4,2,2), cex.lab = 1.3)
# plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", xlim = c(0.75,2.25), 
#      ylim = c(0, 3600),las = 1, xaxt = "n", yaxt = "n")
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
# points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
# arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], col = "blue", length = 0)
# arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], col = "red", length = 0)
# axis(side = 1, at = x, labels = rep("", 2))
# text(x = x, y = usr[3] - ydiff * 0.1, 
#      labels = c("Day 1\nPre-Treatment", "Day 42\nFollow-up"), xpd = T, cex = 1.3)
# text(x = usr[1] + xdiff * 0.05, y = usr[4] - ydiff * 0.05, 
#      pos = 4, label = paste("Control Increase: ", 
#                             abs(round(percent_c)), "%", sep = ""), col = "blue", cex = 1.2)
# text(x = usr[1] + xdiff * 0.05, y = usr[4] - ydiff * 0.12, 
#      pos = 4, label = paste("Treatment Increase: ", 
#                             abs(round(percent_t)), "%", sep = ""), col = "red", cex = 1.2)
# text(x = usr[1] + xdiff * 0.05, y = usr[4] - ydiff * 0.19, 
#      pos = 4, label = "Treatment x Time Interaction: p = 0.75", col = "black", cex = 1.2)
# mtext(side = 2, "Total Bluegill Catch Per Impoundment", line = 2.8, cex = 1.3)
# axis(side = 2, at = seq(0, 3500, 500), labels = seq(0, 3500, 500), las = 2)
# dev.off()

# log() line plot
x = c(1,2)
xc = x + 0.03
xt = x - 0.03
### Change pvalue ###
png("glmer_blue_day1_follwup_log.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
par(mar = c(4,4,2,2), cex.lab = 1.2)
plot(x = 1, y = 1, type = "n", xlab = "", ylab = "", 
     xlim = c(0.75,2.25), ylim = c(0, 8.3),las = 1, xaxt = "n", yaxt = "n")
usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
points(summ[1,c(1,3)] ~ xc, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "blue")
points(summ[1,c(2,4)] ~ xt, type = "l", pch = 16, lwd = 3, cex = 1.2, col = "red")
arrows(xc, summ[2,c(1,3)], xc, summ[3,c(1,3)], col = "blue", length = 0)
arrows(xt, summ[2,c(2,4)], xt, summ[3,c(2,4)], col = "red", length = 0)
axis(side = 1, at = x, labels = rep("", 2))
text(x = x, y = usr[3] - ydiff * 0.1, labels = c("Day 1\nPre-Treatment", "Day 42\nFollow-up"), xpd = T)
text(x = usr[1] + xdiff * 0.05, y = usr[4] - ydiff * 0.05, 
     pos = 4, label = paste("Control Increase: ", abs(round(percent_c)), "%", sep = ""), 
     col = "blue", cex = 1.2)
text(x = usr[1] + xdiff * 0.05, y = usr[4] - ydiff * 0.12, 
     pos = 4, label = paste("Treatment Increase: ", abs(round(percent_t)), "%", sep = ""), 
     col = "red", cex = 1.2)
text(x = usr[1] + xdiff * 0.05, y = usr[4] - ydiff * 0.9, pos = 4, 
     label = "Treatment x Time Interaction: p = 0.75", col = "black", cex = 1.2)
mtext(side = 2, "log(Total Bluegill Catch Per Impoundment)", line = 2.8, cex = 1.2)
axis(side = 2, at = seq(0, 8, 1), labels = seq(0, 8, 1), las = 2)
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








##### OLD; everything below is commented out ---------------------------------------
##### ae1 
# datsub <- datum[datum$lake == "ae1",]
# unique(datsub$lake)
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum)
# summary(counts)
# means <- counts/15
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17ae1_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "AE1 (Control; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ##### s3
# datsub <- datum[datum$lake == "s3",]
# unique(datsub$lake)
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# summary(counts)
# datsub$date
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# m
# ppi = 600
# png("17s3_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "S3 (Treatment; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ### length freq
# 
# datsub$tlgroup = as.numeric(as.character(datsub$tlgroup))
# tab <- table(datsub$tlgroup, datsub$period)
# m <- matrix(0, 6, 3)
# rownames(m) = c(0, 25, 50, 75, 100, 125)
# m[rownames(tab),] = tab
# m <- apply(m, 2, function(x) x/sum(x))
# m <- m[, 1:2]
# m <- t(m)
# m <- m[c(2,1),]
# barplot(m, beside = T)
# 
# ##### washington
# datsub <- datum[datum$lake == "washington county lake",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,2,4,5,3)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# summary(counts)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17washington_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Washington (Treatment; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ###length frequency
# 
# unique(datsub$date)
# datsub$tlgroup = as.numeric(as.character(datsub$tlgroup))
# # tab <- table(datsub$tlgroup[datsub$date %in% c("5/15", "5/16")], 
# #              datsub$period[datsub$date %in% c("5/15", "5/16")])
# tab <- table(datsub$tlgroup[datsub$date %in% c("6/5", "6/6")], 
#              datsub$period[datsub$date %in% c("6/5", "6/6")])
# 
# m <- matrix(0, 6, 3)
# rownames(m) = c(0, 25, 50, 75, 100, 125)
# m[rownames(tab),] = tab
# m <- apply(m, 2, function(x) x/sum(x))
# m <- m[, 1:2]
# m <- t(m)
# m <- m[c(2,1),]
# barplot(m, beside = T)
# 
# ##### anderson
# 
# datsub <- datum[datum$lake == "anderson county lake",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,2,4,5,3)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# summary(counts)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17anderson_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Anderson (Control; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ### length frequency
# 
# datsub$tlgroup = as.numeric(as.character(datsub$tlgroup))
# tab <- table(datsub$tlgroup, datsub$period)
# m <- matrix(0, 6, 3)
# rownames(m) = c(0, 25, 50, 75, 100, 125)
# m[rownames(tab),] = tab
# m <- apply(m, 2, function(x) x/sum(x))
# m <- m[, 1:2]
# m <- t(m)
# m <- m[c(2,1),]
# barplot(m, beside = T)
# 
# ##### barbour
# 
# datsub <- datum[datum$lake == "barbour county lake",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17barbour_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Barbour (Treatment; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ##### lee
# 
# datsub <- datum[datum$lake == "lee county lake",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17lee_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Lee (Control; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ##### monroe
# 
# datsub <- datum[datum$lake == "monroe county lake",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17monroe_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Monroe (Treatment; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ##### dale
# 
# datsub <- datum[datum$lake == "dale county lake",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17dale_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Dale (Control; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ##### littlepit
# 
# datsub <- datum[datum$lake == "little pit",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17littlepit_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Little Pit (Treatment; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ##### bigpit
# 
# datsub <- datum[datum$lake == "big pit",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,2,3,4,5)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17bigpit_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Big Pit (Control; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ##### horseshoe 
# 
# datsub <- datum[datum$lake == "horseshoe pond",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,4,2,3,5)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17horseshoe_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "Horseshoe (Treatment; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
# 
# ##### fp3 
# 
# datsub <- datum[datum$lake == "fp3",]
# unique(datum$lake)
# 
# fdates = factor(datsub$date)
# levels(fdates)
# fdates2=factor(fdates, levels = levels(fdates)[c(1,4,2,3,5)])
# levels(fdates2)
# datsub$date = fdates2
# 
# counts <- tapply(datsub$count, list(datsub$period, datsub$date), sum, na.rm = T)
# means <- counts/15
# datsub$date
# unique(datum$date)
# 
# m <- matrix(NA, 2, 3)
# m[1,1] = means["before", 1]
# 
# m[2,1] = means["after", 2]
# 
# m[1,2] = means["before", 3]
# 
# m[2,2] = means["after", 4]
# 
# m[1,3] = means["followup", 5]
# 
# ppi = 600
# png("17fp3_blue.png", h = 3*ppi, w = 7*ppi, res = ppi)
# par(mar = c(2,4,3,1))
# mp <- barplot(m, beside = T, ylim = c(0, max(m, na.rm = T) + 1), las = 1, 
#               main = "FP3 (Control; 2017)", cex.main = 2, 
#               col = c("black", "grey", "black", "grey", "white"))
# mtext(side = 2, "Blue Catch/Haul", line = 2, cex = 1.5, font = 2)
# mp <- as.numeric(mp) 
# mp <- mp[1:5]
# 
# usr <- par("usr")
# segments(usr[1], usr[3], usr[2], usr[3], xpd = T)
# 
# axis(side = 1, at = mp, labels = colnames(means))
# dev.off()
# 
# ((m[2,] - m[1,]) / m[1,]) * 100
##### PERCENT CHANGE DAY 1 VS DAY 2 BLUEGILL #####
# 
# ###all 12 lake bar graphs above
# 
# ### county lake control, treatment, pond control, treatment
# # mn <- c(11.456493, -86.644325, -6.748446833, -95.87959667)
# # high <- c(-18.41, -96.67, -17.31, -100.0)
# # low <- c(53.125, -69.57, 2.59, -81.6)
# 
# e1.perc = ((counts[,"after",1] - counts[,"before",1])/counts[,"before",1]) * 100
# e2.perc = ((counts[,"after",2] - counts[,"before",2])/counts[,"before",2]) * 100
# 
# perc = (e1.perc + e2.perc)/2
# 
# # df = data.frame(perc = unname(perc), lake = names(perc))
# df = data.frame(perc = unname(perc), lake = names(perc))
# 
# # na = 0
# df[is.na(df$perc)==T,1]<-0
# 
# levels(datum$lake)
# treat = c("control", "control", "treatment", "control", "treatment", "treatment",
#           "control", "control", "treatment", "treatment", "treatment", "control", "control", "treatment", "control",
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
# 
# png("18percentchange_blue.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(3, 4, 1, 1))
# plot(mn~x, ylim = c(-100, 100), las = 1, ylab = "", xlab = "",
#      pch = 16, xaxt = "n", cex = 2,
#      col = c("blue", "red", "blue", "red"))
# mtext(side = 2, line = 2.25, cex = 1.5, font = 2, "% Change Catch/Haul")
# axis(side = 1, at = x, labels = rep("", 4))
# text(x = x, y = rep(-125, 4), c("Control", "Treatment", "Control",
#                                 "Treatment"), xpd = T)
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# text(usr[1], usr[4] - ydiff * 0.09, "STATE\nLAKES", cex = 1.5, font = 2, pos = 4)
# text(usr[2], usr[4] - ydiff * 0.05, "PONDS", cex = 1.5, font = 2, pos = 2)
# abline(v = 2.5)
# arrows(x, low, x, high, code = 3, angle = 90, length = .1,
#        col = c("blue", "red", "blue", "red"))
# abline(h = 0, lty = 2, col = "grey")
# dev.off()
# 



##### FOLLOW UP ANALYSIS (ON PERCENT DECREASE FROM D1 TO FOLLOWUP) #####

# perc = ((counts[,"followup",3] - counts[,"before",1])/counts[,"before",1]) * 100
# 
# 
# df = data.frame(perc = unname(perc), lake = names(perc))
# treat = c("control", "control", "treatment", "control", "control", "control", "treatment",
#           "control", "treatment", "treatment", "treatment", "treatment")
# type = c("pond", "pond", "lake", "pond", "lake", "pond", "pond", "lake", "pond", "lake", "pond", "lake")
# 
# df$type = type
# df$treatment = treat
# 
# # fit = lm(perc ~ treatment + type, data = df)
# # summary(fit)
# # 
# # pred = predict(fit, newdata = data.frame(treatment = c("control", "treatment", "control", "treatment"),
# #                                          type = c("lake", "lake", "pond", "pond")), se.fit = T)
# # means = pred$fit
# # lwr = pred$fit - 1.96 * pred$se.fit
# # upr = pred$fit + 1.96 * pred$se.fit
# 
# df[df$treatment == "treatment",]
# 
# means = as.numeric(tapply(df$perc, list(df$treatment, df$type), mean))
# lwr = as.numeric(tapply(df$perc, list(df$treatment, df$type), min))
# upr = as.numeric(tapply(df$perc, list(df$treatment, df$type), max))
# 
# x = c(1:4)
# # plot(means ~ x, ylim = range(c(lwr, upr)))
# # arrows(x, lwr, x, upr, length = 0)
# 
# ppi = 600
# png("17followup_mean_percent_blue.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
# par(mar = c(3, 4, 1, 1))
# plot(means~x, type = "n", ylim = c(-100, 620), las = 1, ylab = "", xlab = "",
#      pch = 16, xaxt = "n", cex = 2)
# arrows(x, lwr, x, upr, code = 3, angle = 90, length = .1)
# usr = par("usr"); xdiff = usr[2] - usr[1]; ydiff = usr[4] - usr[3]
# mtext(side = 2, line = 2.5, cex = 1.5, font = 2, "% Change")
# axis(side = 1, at = x, labels = rep("", 4))
# text(x = x, y = usr[3] - ydiff * 0.06, c("Control", "Treatment", "Control",
#                                          "Treatment"), xpd = T)
# text(usr[1], usr[4] - ydiff * 0.09, "STATE\nLAKES", cex = 1.5, font = 2, pos = 4)
# text(usr[2], usr[4] - ydiff * 0.05, "PONDS", cex = 1.5, font = 2, pos = 2)
# abline(v = 2.5)
# abline(h = 0, lty = 2, col = "grey")
# points(means ~ x, pch = 16, cex = 2)
# # text(x = c(1.5, 3.5), y = c(80, 80), c("*", "*"), cex = 2.5, font = 2)
# dev.off()
