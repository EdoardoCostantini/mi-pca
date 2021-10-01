# Project:   mipca_compare
# Objective: function for imputation using PCA at every iteration
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-09-29

imputePCAvbv <- function(Z, ncfs = 1, parms){

  ## Example inputs
  # Z = dat_miss
  # ncfs = 1

  ## output:
  # - mids object
  # - average explained variance
  # - imputation run time

  ## body:
  tryCatch({

    ## Process the number of predictors
    if(grepl("max", ncfs)){
      ncfs_int <- ncol(Z) - 1
    } else {
      ncfs_int <- as.numeric(ncfs)
    }

    # TIME STAMP: Start!
    start_time <- Sys.time()

    # Prepare object for mice
    Z_mice <- apply(Z, 2, as.numeric)

    # Impute
    mids_pcr_sim <- mice.pcr.sim(Z_mice,
                                 m      = parms$mice_ndt,
                                 maxit  = parms$mice_iters,
                                 method = "pcr.mixed",
                                 npcs = ncfs_int)
    # Extract CPVE
    ## Drop empty PCs
    keep <- sapply(mids_pcr_sim$pcs, function (x) {
      !all(is.na(x))}
    )
    CPVE_dfs <- mids_pcr_sim$pcs[keep]
    CPVE_dfs <- lapply(1:length(CPVE_dfs),
                         function (x) {
                           # Fix Column names to number of imputation
                           colnames(CPVE_dfs[[x]]) <- paste0("m",
                                                               1:ncol(CPVE_dfs[[x]]))
                           cbind(
                           # Add variable name column
                             var = names(CPVE_dfs)[x],
                           # Add iteration counter column
                             iter = 1:nrow(CPVE_dfs[[x]]),
                             CPVE_dfs[[x]])
                         })
    CPVE_mat <- do.call(rbind, CPVE_dfs)

    # Compute mean for the last iteration
    CPVE <-
      mean(
        unlist(
          lapply(CPVE_dfs,
                 function (x) x[nrow(x), -c(1, 2)]
          )
        )
      )

    # TIME STAMP: End!
    end_time <- Sys.time()
    imp_time <- difftime(end_time, start_time, units = "mins")

    # Store results
    return(list(mids = mids_pcr_sim,
                CPVE = CPVE,
                CPVE_mat = CPVE_mat,
                time = as.vector(imp_time)))

    ### END TRYCATCH EXPRESSION
  }, error = function(e){

    err <- paste0("Original Error: ", e)
    print(err)
    return(list(mids = NULL,
                CPVE = NULL,
                CPVE_mat = NULL,
                time = NULL))
  }
  )

}