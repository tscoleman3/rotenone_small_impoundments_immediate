rm(list = ls(all = T))

##### BLUEGILL ELECTROFISHING #####
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
source("script/funcs_blue.R")
# highlight and run all code for AICc table
source("script/AICc_table_func.R")





##### ----------------------------------------------------------------------------------------------
##### READ IN DATA #####
##### ----------------------------------------------------------------------------------------------
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





##### ----------------------------------------------------------------------------------------------
##### LETS LOOP EFISHING DATA TO CALCULATE MLA, CPUE, SURV, RECRUIT ##### --------------------------
##### ----------------------------------------------------------------------------------------------
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
# write.csv(output, "blue_output.csv", row.names = FALSE)       # write dataset into a .csv to send





##### ----------------------------------------------------------------------------------------------
##### BACI ANALYSIS: CPUE > 80 mm ##### ------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------
hist(output$cpue)
hist(log(output$cpue))     # normal

output_cpue <- output    # create new dataset to manipulate
min(output_cpue$cpue)   # no 0's

# call out small and large impoundments for separate analyses
datsub_small <- output_cpue[output_cpue$size_class %in% "small",]  # for small sized analysis
datsub_big <- output_cpue[output_cpue$size_class %in% "big",]    # for large sized analysis

# can only use a random effect of lake;
# adding a year random effect causes singular fit due to sample size
lmer_cpue_1 = lmer(log(cpue) ~ times_treat + 
                    (1 | lake), data = datsub_small, REML = F)
lmer_cpue_1 = lmer(log(cpue) ~ relevel(times_treat, ref = "1") + 
                    (1 | lake), data = datsub_small, REML = F)
# pql fit is to get degrees of freedom for each group
fit <- glmmPQL(log(cpue) ~ times_treat, random = ~1|lake, 
               data = datsub_small, family = "gaussian")
summary(fit)

# can only use a random effect of year;
# adding a lake random effect causes singular fit due to sample size
# lmer_cpue_1 = lmer(log(cpue) ~ times_treat + 
#                     (1 | lake), data = datsub_big, REML = F)
lmer_cpue_1 = lmer(log(cpue) ~ times_treat + 
                    (1 | year), data = datsub_big, REML = F)
lmer_cpue_1 = lmer(log(cpue) ~ relevel(times_treat, ref = "1") + 
                    (1 | year), data = datsub_big, REML = F)
# pql fit is to get degrees of freedom for each group
fit <- glmmPQL(log(cpue) ~ times_treat, random = ~1|year, 
               data = datsub_big, family = "gaussian")
fit <- glmmPQL(log(cpue) ~ relevel(times_treat, ref = "1"), random = ~1|year, 
               data = datsub_big, family = "gaussian")
summary(fit)


# after each model above is run, come here to interpret results
summary(lmer_cpue_1)
anova(lmer_cpue_1)
coefs = summary(lmer_cpue_1)$coef
coefs_est = exp(coefs[,"Estimate"])
uprs = exp(coefs[,"Estimate"] + 1.96 * coefs[,"Std. Error"])
lwrs = exp(coefs[,"Estimate"] - 1.96 * coefs[,"Std. Error"])
(uprs - 1) * 100       # upr CI %'s
(coefs_est - 1) * 100  # coeficent estimates CI %'s
(lwrs - 1) * 100       # lwr CI %'s
