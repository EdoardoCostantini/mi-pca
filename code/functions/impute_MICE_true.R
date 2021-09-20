# Project:   mipca_compare
# Objective: function for imputation under true imputation model
# Author:    Edoardo Costantini
# Created:   2021-09-20
# Modified:  2021-09-20

impute_MICE <- function(Z, imp_target, preds, parms){

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
  tryCatch({

    start_time <- Sys.time()

    # Make predictor matrix
    pred_mat <- make.predictorMatrix(Z)
    pred_mat[imp_target, -preds] <- 0

    # Impute
    imp_MITR_mids <- mice::mice(Z,
                                predictorMatrix = pred_mat,
                                m = parms$mice_ndt,
                                maxit = parms$mice_iters,
                                method = "norm.boot")
    end_time <- Sys.time()
    imp_MITR_time <- difftime(end_time, start_time, units = "mins")

    # Store results
    return(list(mids = imp_MITR_mids,
                time = as.vector(imp_MITR_time)))

    ### END TRYCATCH EXPRESSION
  }, error = function(e){

    err <- paste0("Original Error: ", e)
    print(err)
    return(list(mids = NULL,
                time = NULL))

  }
  )

}
