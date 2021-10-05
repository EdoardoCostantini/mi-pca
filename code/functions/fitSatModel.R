# Project:   mipca_compare
# Objective: Fit saturated model to mids data
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-09-21

fitSatModel <- function(mids, model){
  # Given a list of complete datasets it fits a model described
  # in mod
  ## Example input ##
  # mids = mids_out$mids
  # model = genLavaanMod(dat_miss, targets = parms$vmap$ta)
  # target_coef <- "mean"

  ## Body ##
  # Obtain full datasets
  dats <- complete(mids, "all")

  # Fit models
  models <- lapply(X = dats,
                   FUN = function(x) {
                     # Obtain MLE estimates
                     sem(model, data = x)
                   }
  )

  return(models)
}