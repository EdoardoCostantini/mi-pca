### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-28
### Modified: 2021-07-23

amputePerVar <- function(targets, preds, pm = .5, type = "high"){
  ## Description
  # Adds Matrix Design missingness to the MAR
  ## Example Inputs
  # cond <- conds[1, ]
  # dat_list <- genData(parms = parms, cond = cond)
  # targets = dat_list$dat_ob[, parms$varMap_items$ta]
  # preds = dat_list$dat_ob[, parms$varMap_items$mp]
  # pm = parms$pm
  # type = "high"

  for (i in parms$varMap_items$ta) {
    nR <- simMissingness(pm    = pm,
                         data  = model.matrix(~ ., preds)[, -1],
                         type  = type)
    
    # Fill in NAs
    targets[nR, i] <- NA
  }

  # Result
  return( target )
}