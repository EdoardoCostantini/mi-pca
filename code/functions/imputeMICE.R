# Project:   mipca_compare
# Objective: function for imputation under true imputation model
# Author:    Edoardo Costantini
# Created:   2021-09-20
# Modified:  2021-09-20

imputeMICE <- function(Z, imp_target, preds, parms){

  ## Example inputs
  # Z = dat_miss
  # imp_target = parms$vmap$ta
  # preds = parms$vmap$mp
  # preds = parms$vmap$ta

  ## output:
  # - mids object
  # - vector of type
  # - imputation run time

  ## body:
  start_time <- Sys.time()

  # Make predictor matrix
  pred_mat <- make.predictorMatrix(Z)
  pred_mat[imp_target, -preds] <- 0

  # Impute
  imp_mids <- mice::mice(Z,
                         predictorMatrix = pred_mat,
                         m = parms$mice_ndt,
                         maxit = parms$mice_iters,
                         method = "norm.boot")
  end_time <- Sys.time()
  imp_time <- difftime(end_time, start_time, units = "mins")

  # Store results
  return(list(mids = imp_mids,
              time = as.vector(imp_time)))

}
