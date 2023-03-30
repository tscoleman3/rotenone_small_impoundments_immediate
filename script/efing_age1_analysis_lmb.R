rm(list = ls(all = T))

##### LARGEMOUTH BASS ELECTROFISHING #####
#' age-1 manuscript script
#' figures are created in separate R scripts 


##### ----------------------------------------------------------------------------------------------
##### PACKAGES NEEDED ##### ------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
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

# highlight and run all code in funcs.R
source("script/funcs_lmb.R")
# highlight and run all code for AICc table
source("script/AICc_table_func.R")




##### ----------------------------------------------------------------------------------------------
##### LETS LOOP EFISHING DATA TO CALCULATE MLA, CPUE, SURV, RECRUIT ##### --------------------------
##### ----------------------------------------------------------------------------------------------
mla_dat = read.csv("data/3.27.19data.csv")          # read in dataset
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





##### ----------------------------------------------------------------------------------------------
##### BACI ANALYSIS: MLA-1 ##### -------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
output_mla_1 = output   # rename output to manipulate for this analysis only
output_mla_1$mla_1[is.na(output_mla_1$mla_1)] = 0       # add a 0 in place of NA for removal
output_mla_1 = output_mla_1[output_mla_1$mla_1 != 0,]   # remove 0 in mla_1
unique(output_mla_1$mla_1)                              # 0's and NA's absent 

hist(output_mla_1$mla_1)
hist(log(output_mla_1$mla_1))

# call out small and large impoundments for separate analyses
datsub_small <- output_mla_1[output_mla_1$size_class %in% "small",]  # for small sized analysis
datsub_big <- output_mla_1[output_mla_1$size_class %in% "big",]    # for large sized analysis

# can only use a random effect of lake;
# adding a year random effect causes singular fit due to sample size
lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
                    (1 | lake), data = datsub_small, REML = F)
lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "1") + 
                    (1 | lake), data = datsub_small, REML = F)
# pql fit is to get degrees of freedom for each group
fit <- glmmPQL(log(mla_1) ~ times_treat, random = ~1|lake, 
               data = datsub_small, family = "gaussian")
summary(fit)

# can only use a random effect of year;
# adding a lake random effect causes singular fit due to sample size
# lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
#                     (1 | lake), data = datsub_big, REML = F)
lmer_mla_1 = lmer(log(mla_1) ~ times_treat + 
                    (1 | year), data = datsub_big, REML = F)
lmer_mla_1 = lmer(log(mla_1) ~ relevel(times_treat, ref = "1") + 
                    (1 | year), data = datsub_big, REML = F)
# pql fit is to get degrees of freedom for each group
fit <- glmmPQL(log(mla_1) ~ times_treat, random = ~1|year, 
               data = datsub_big, family = "gaussian")
summary(fit)


# after each model above is run, come here to interpret results
summary(lmer_mla_1)
anova(lmer_mla_1)
coefs = summary(lmer_mla_1)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s



# plot residuals from prediction to test for assumptions
resfit = resid(lmer_mla_1)
hist(resfit)
plot(log(datsub_small$mla_1), resfit, ylab="Residuals", xlab="Count") 
plot(log(datsub_big$mla_1), resfit, ylab="Residuals", xlab="Count") 
abline(0, 0)              
# look at predictions from model
predict(lmer_mla_1)
datsub_small$pred = exp(predict(lmer_mla_1))
datsub_small$predicted = predict(lmer_mla_1)    # Save the predicted values
datsub_small$residuals = residuals(lmer_mla_1)  # Save the residual values

# quick look at the actual, predicted, and residual values
pred_df = datsub_small %>% select(mla_1, predicted, residuals) %>% head()
pred_df$predicted = exp(pred_df$predicted)
pred_df$residuals = exp(pred_df$residuals)
plot(exp(fitted(lmer_mla_1)) ~ datsub_small$mla_1); abline(0,1)





##### ----------------------------------------------------------------------------------------------
##### BACI ANALYSIS: CPUE AGE-1 (RECRUITMENT) ##### ------------------------------------------------
##### ----------------------------------------------------------------------------------------------
hist(output$cpue_1)              # non-normal distribution
hist(log(output$cpue_1))         # normal distribution

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
               data = datsub_small, family = "gaussian")
summary(fit)


fit_cpue_1 = lmer(log(cpue_1) ~ times_treat + 
                    (1 | year), data = datsub_big, REML = F)
fit_cpue_1 = lmer(log(cpue_1) ~ relevel(times_treat, ref = "1") + 
                    (1 | year), data = datsub_big, REML = F)
fit <- glmmPQL(log(cpue_1) ~ times_treat, random = ~1|year, 
               data = datsub_big, family = "gaussian")
summary(fit)

# interpret results from models above
summary(fit_cpue_1)
anova(fit_cpue_1)
coefs = summary(fit_cpue_1)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s





##### ----------------------------------------------------------------------------------------------
##### SURVIVAL; COMPENSATORY RESPONSE ##### --------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
hist(output$surv)
output$surv

# sample size will be even smaller because we need data from seine hauls the year before
# and then from electrofishing that year; thus, every first year electrofishing data will not
# have a surv index
# see below
output$surv
output$mla_1
output$cpue_1

# remove NAs from data
output_surv <- output
unique(output_surv$surv)
length(output_surv$surv)   # N = 48
output_surv$surv[is.na(output_surv$surv)] = 0        # add a 0 in place of NA for removal
output_surv = output_surv[output_surv$surv != 0, ]   # remove 0 in surv
unique(output_surv$surv)                             # 0's and NA's absent 
length(output_surv$surv)   # N went from 48 to 26

hist(output_surv$surv)
hist(log(output_surv$surv))

# separate out large and small 
datsub_small <- output_surv[output_surv$size_class %in% "small", ]  # for small sized analysis
datsub_big <- output_surv[output_surv$size_class %in% "big", ]      # for large sized analysis

### ###
# sample size went from 12 (large) and 36 (small) in analyses above to
# 7 (large) and (19) small for survival calculation 
### ###

hist(log(datsub_small$surv))
hist(log(datsub_big$surv))

# different models as above due to change in sample size 
fit_surv_1 = lmer(log(surv) ~ times_treat + 
                    (1 | year), data = datsub_small, REML = F)
fit_surv_1 = lmer(log(surv) ~ relevel(times_treat, ref = "1") + 
                    (1 | year), data = datsub_small, REML = F)
fit <- glmmPQL(log(surv) ~ times_treat, random = ~1|year, 
               data = datsub_small, family = "gaussian")
summary(fit)

fit_surv_1 <- lm(log(surv) ~ times_treat,
                 data = datsub_big)
fit_surv_1 <- lm(log(surv) ~ relevel(times_treat, ref = "1"),
                 data = datsub_big)

# interpret results from models above
summary(fit_surv_1)
anova(fit_surv_1)
coefs = summary(fit_surv_1)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s
