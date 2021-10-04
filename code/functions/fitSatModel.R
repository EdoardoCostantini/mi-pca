# Project:   mipca_compare
# Objective: Fit saturated model to mids data
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-10-04

fitSatModel <- function(mids, model){
  # Given a list of complete datasets it fits a model described
  # in mod
  ## Example input ##
  # mids = imp_out$mids
  # model = genLavaanMod(dat_miss, targets = parms$vmap$ta)

  ## Body ##
  # Obtain full datasets
  tryCatch({
    dats <- complete(mids, "all")

    # Fit models
    models <- lapply(X = dats,
                     FUN = function(x) {
                       # Obtain MLE estimates
                       sem(model, data = x)
                     }
    )

    return(models)

    ### END TRYCATCH EXPRESSION
  }, error = function(e){
    err <- paste0("Original Error: ", e)
    print(err)
    return(NULL)
  }
  )
}