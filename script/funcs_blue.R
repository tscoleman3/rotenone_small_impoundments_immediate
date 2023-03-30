### BLUEGILL FUNCTIONS ###

### CPUE < 80mm and >= 80mm FUNCTION ###
get_cpue = function(datsub, min_length = 80) {
  
  count = datsub[datsub$tl >= min_length, c("tl", "count")]
  cpue = sum(count$count, na.rm = T)
}


### Wr FUNCTION ###
# std_a and std_b are the length-weight coefs for the standard population
# min_length: if you don't want to calculate it for fish smaller than some size
get_wr = function(datsub, min_length = 80, std_a = -5.374, std_b = 3.316) {
  
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


