### LMB DIETS ###

# this will allow the %!in% function
# devtools::install_github("bstaton1/StatonMisc")
# library(StatonMisc)
# library(MuMIn)

rm(list = ls(all = T))

# highlight and run all code for AICc table
# source("AICc_table_func.R")

# read in diet and lake identifier data sets, keep relevant columns from each
dat = read.csv("lmbdiets.csv", stringsAsFactors = F)
ids = read.csv("lake_year_ids.csv", stringsAsFactors = F)
ids = ids[,c("lake", "year", "ba", "type", "size", "times_treat")]
colnames(dat)[1] = "lake"
dat = dat[,c("lake", "year", "season", "tl", "wt", "newid", "prey")]

# throw out harris and larry diet data
# dat = dat[dat$lake %!in% c("harris", "larry"),]
# or, without StatonMisc::%!in%
dat = dat[!(dat$lake %in% c("harris", "larry")),]
unique(dat$lake)

# keep a specific size range of predators
dat = dat[dat$tl > 250 & dat$tl < 354,]

# discard empty diet fish
# dat = dat[dat$prey != "empt",]

# rename the lakes in the diet data to be consistent with the lake names in the id data
dat$lake = ifelse(dat$lake == "lee county", "lee county lake", dat$lake)
dat$lake = ifelse(dat$lake == "monroe county", "monroe county lake", dat$lake)
dat$lake = ifelse(dat$lake == "washington county", "washington county lake", dat$lake)
dat$lake = ifelse(dat$lake == "barbour county", "barbour county lake", dat$lake)
dat$lake = ifelse(dat$lake == "anderson pond", "anderson county lake", dat$lake)
dat$lake = ifelse(dat$lake == "dale", "dale county lake", dat$lake)

# merge ids with dat
dat = merge(ids, dat, by = c("lake", "year"))

# determine whether each predator ate fish
fish_codes = c("blgl", "shin", "unpr", "unfu", "bwdr", "fspi",
               "gamb", "lgmb", "rear", "sunf", "thsh", "unfi")

fish_present = NULL
for (i in unique(dat$newid)) {
  # subset out data for this fish
  dat_sub = dat[dat$newid == i,]
  
  # determine if it ate fish
  tmp = data.frame(newid = i, ate_fish = as.numeric(any(dat_sub$prey %in% fish_codes)))
  
  # store the result for that fish
  fish_present = rbind(fish_present, tmp)
}

# or the same thing with sapply()
# t(sapply(unique(dat$newid), function(i) {
#   # subset out data for this fish
#   dat_sub = dat[dat$newid == i,]
#   
#   # determine if it ate fish
#   data.frame(newid = i, ate_fish = as.numeric(any(dat_sub$prey %in% fish_codes)))
# }))

# merge in the fish present data with the fish/lake identifier data
dat = merge(dat, fish_present, by = "newid")

# remove duplicate rows for each predator and remove the "prey" column
dat = dat[!duplicated(dat$newid),-which(colnames(dat) == "prey")]

#### fit models ####
dat$times_treat = as.factor(dat$times_treat)

dat = dat[dat$season == "spring",]

datsub = dat[,c("year", "type", "times_treat", "ate_fish")]

library(dplyr)

calc_p = function(x) {
  sum(x)/length(x)
}
calc_lwr_p = function(x) {
  p = calc_p(x)
  p - 1.96 * sqrt((p * (1 - p))/length(x))
}
calc_upr_p = function(x) {
  p = calc_p(x)
  p + 1.96 * sqrt((p * (1 - p))/length(x))
}

plot_dat = datsub %>% group_by(year, type, times_treat) %>%
  summarize(
    mn = calc_p(ate_fish),
    lwr = calc_lwr_p(ate_fish),
    upr = calc_upr_p(ate_fish)
  ) %>% ungroup

plot_dat$year = as.numeric(plot_dat$year)

off_ctrl = -0.05
off_trt1 = 0.05

ppi = 600
png("ate_fish.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
par(mar = c(2,4,1,1), cex.axis = 1.2, cex.lab = 1.2)
plot(1,1, type = "n", xlim = range(plot_dat$year) + c(off_ctrl, off_trt1) * 2, ylim = range(plot_dat[,c("lwr", "upr")]),
     xaxt = "n", las = 1, ylab = "Proportion of LMB Diets Containing Fish")

# controls
lines(mn ~ I(year + off_ctrl), data = plot_dat[c(1,4),], lty = 2, lwd = 2)
with(plot_dat[c(1,4),], segments(I(year + off_ctrl), lwr, I(year + off_ctrl), upr, lwd = 2, lty = 2))
points(mn ~ I(year + off_ctrl), data = plot_dat[c(1,4),], pch = 21, bg = "white", cex = 1.2)

# treated once
lines(mn ~ I(year), data = plot_dat[c(2,5),], lwd = 2)
with(plot_dat[c(2,5),], segments(I(year), lwr, I(year), upr, lwd = 2))
points(mn ~ I(year), data = plot_dat[c(2,5),], pch = 21, bg = c("white", "black"), cex = 1.2)

# treated twice
lines(mn ~ I(year + off_trt1), data = plot_dat[c(3,6),], lwd = 2)
with(plot_dat[c(3,6),], segments(I(year + off_trt1), lwr, I(year + off_trt1), upr, lwd = 2))
points(mn ~ I(year + off_trt1), data = plot_dat[c(3,6),], pch = c(21,15), bg = c("black", "black"), cex = 1.2)

axis(side = 1, at = 2017:2019, labels = 2017:2019)
legend("top", legend = c("Control", "Treatment", "Untreated", "Treated Once", "Treated Twice"),
       pch = c(NA, NA, 21, 21, 15), pt.cex = c(NA, NA, 1.2, 1.2, 1.2), pt.bg = c(NA, NA, "white", "black", "black"),
       bty = "n", lty = c(2,1,NA,NA,NA), lwd = c(2,2,NA,NA,NA), x.intersp = c(0.5, 0.5, 0.25, 0.25, 0.25))
mtext("Already Treated", line = 1)

dev.off()
