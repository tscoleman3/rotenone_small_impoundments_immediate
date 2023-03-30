### LARGEMOUTH BASS FUNCTIONS ###

# rm(list = ls(all = T))

### MLA FUNCTION ### 
get_mla = function(datsub) {
  cmgrp = floor(datsub$tl / 25) * 25
  count_cmgrp = table(cmgrp)
  count_cmgrp_age = table(cmgrp, datsub$age)
  prob_cmgrp_age = prop.table(count_cmgrp_age, margin = 1)
  n_cmgrp_age = as.matrix(prob_cmgrp_age) * as.numeric(count_cmgrp)
  n_age = apply(n_cmgrp_age, 2, sum, na.rm = T)
  lengths = as.numeric(row.names(n_cmgrp_age)) + 12.5
  products = n_cmgrp_age * lengths
  mean_length = apply(products, 2, sum, na.rm = T) / n_age
  meantl = rep(NA, maxage)
  la = as.numeric(names(mean_length))
  meantl[la] = mean_length
  
  names(meantl) = paste("mla", 1:maxage, sep = "_")
  meantl = t(as.data.frame(meantl)); rownames(meantl) = NULL
  meantl
}

### CPUE AGE-1 and AGE-2+ FUNCTION ###
get_cpue = function(datsub, age = 1) {
  cmgrp = floor(datsub$tl / 25) * 25
  count_cmgrp = table(cmgrp)
  count_cmgrp_age = table(cmgrp, datsub$age)
  prob_cmgrp_age = prop.table(count_cmgrp_age, margin = 1)
  n_cmgrp_age = as.matrix(prob_cmgrp_age) * as.numeric(count_cmgrp)
  n_age = apply(n_cmgrp_age, 2, sum, na.rm = T)
  
  
  if (age == 1) {
    unname(n_age["1"])
  } else {
    sum(n_age[names(n_age) != "1"])
  }
  
  
}

### Wr FUNCTION ###
# std_a and std_b are the length-weight coefs for the standard population
# min_length: don't want to calculate it for fish smaller than some size
get_wr = function(datsub, min_length = 150, std_a = -5.528, std_b = 3.273) {
  
  obs = datsub[datsub$tl >= min_length, c("tl", "wt")]
  
  log10_std_wt = std_a + std_b * log(obs$tl, base = 10)
  std_wt = 10^log10_std_wt
  
  wr = (obs$wt/std_wt) * 100
  
  mean(wr, na.rm = T)
}

### PSDs FUNCITON ###
# stock length: length of "stock" fish
# other length: length of "quality" (if doing psd), length of "preferred" (if doing psd-p), etc.
get_psd = function(datsub, stock_length, other_length) {
  n_stock = nrow(datsub[datsub$tl >= stock_length,])
  n_other = nrow(datsub[datsub$tl >= other_length,])
  
  (n_other/n_stock) * 100
}

### SURVIVAL FUNCTION ###
get_surv  = function(datsub_ef, datsub_sn) {
  n_sn = nrow(datsub_sn[datsub_sn$event == 3,])
  n_ef = get_cpue(datsub_ef, age = 1)
  
  n_ef/n_sn
}