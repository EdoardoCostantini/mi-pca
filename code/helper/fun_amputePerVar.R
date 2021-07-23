### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-28
### Modified: 2021-07-23

amputePerVar <- function(targets, preds, pm = .5, type = "high"){
  ## Description
  # Uses the simMissingness() function to impose missing values on
  # targets set of variables based on preds.
  # It returns the variables in targets with missing values imposed.
  ## Example Inputs
  # cond <- conds[1, ]
  # dat_list <- genData(parms = parms, cond = cond)
  # targets = dat_list$dat_ob[, parms$varMap_items$ta]
  # preds = dat_list$dat_ob[, parms$varMap_items$mp]
  # preds = dat_list$dat_lv[, parms$varMap$mp,
  #                           drop = FALSE]
  # pm = parms$pm
  # type = "high"

  ## Body
  preds_object <- model.matrix(~ ., as.data.frame(preds))[, -1, drop = FALSE]

  for (i in parms$varMap_items$ta) {
    nR <- simMissingness(pm    = pm,
                         data  = preds_object,
                         type  = type)
    
    # Fill in NAs
    targets[nR, i] <- NA
  }

  # Result
  return( targets )
}