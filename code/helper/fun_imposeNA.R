### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-28
### Modified: 2021-07-08

imposeNA <- function(dat_in, parms){
  ## Description
  # Adds Matrix Design missingness to the MAR
  ## Example Inputs
  # cond <- conds[1, ]
  # dat_in <- genData(parms = parms, cond = cond)
  # plot = TRUE
  
# MAR ---------------------------------------------------------------------
  
  dat_out <- dat_in$dat_ob
  
  # Number of variables receiving MAR
  MAR_ta_n <- length(parms$varMap$ta) * parms$J
  
  # Impose MAR
  for (i in 1:MAR_ta_n) {
    MAR_type <- sample(c("high", "low", "center", "tails"), 1)
    nR <- simMissingness(pm    = parms$pm,
                         data  = dat_in$dat_lv,
                         preds = parms$varMap$mp,
                         type  = MAR_type,
                         beta  = rep(1, length(parms$varMap$mp)))
    
    # Fill in NAs
    dat_out[nR, i] <- NA
  }

  # Result
  return( dat_out )
}