# Project:   mipca_compare
# Objective: function to define the lavaan model desired
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-09-21

genLavaanMod <- function (dt, targets){

  ## Inputs
  dt = dat_miss
  targets = parms$vmap$ta

  ## Body
  var_names <- colnames(dt[, targets])

  # Means
  all_means <- paste0(var_names, " ~ ", "1")

  # Variances
  all_vars <- paste0(var_names, " ~~ ", var_names)

  # Coivariances
  all_covs <- combn(var_names, 2)
  all_covs <- apply(all_covs, 2, paste0, collapse = " ~~ ")

  # Join
  lavaan_mod <- paste(c(all_means, all_vars, all_covs), collapse = "\n")

  return(lavaan_mod)
}