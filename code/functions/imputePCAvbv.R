# Project:   mipca_compare
# Objective: function for imputation using PCA at every iteration
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-09-29

imputePCAvbv <- function(Z, ncfs = 1, parms){

  ## Example inputs
  # Z = sapply(dat_miss, as.numeric)
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

    # Impute
    mids_pcr_sim <- mice.pcr.sim(Z,
                                 m      = parms$mice_ndt,
                                 maxit  = parms$mice_iters,
                                 method = "pcr.mixed",
                                 npcs = ncfs_int)
    # Drop empty PCs
    is.na(mids_pcr_sim$pcs)
    keep <- sapply(mids_pcr_sim$pcs, function (x) {
      !all(is.na(x))}
    )
    pc_exp_dfs <- mids_pcr_sim$pcs[keep]
    pc_exp_dfs <- lapply(1:length(pc_exp_dfs),
                         function (x) {
                           # Fix Column names to number of imputation
                           colnames(pc_exp_dfs[[x]]) <- paste0("m",
                                                               1:ncol(pc_exp_dfs[[x]]))
                           cbind(
                           # Add variable name column
                             var = names(pc_exp_dfs)[x],
                           # Add iteration counter column
                             iter = 1:nrow(pc_exp_dfs[[x]]),
                             pc_exp_dfs[[x]])
                         })
    pc_var_mat <- do.call(rbind, pc_exp_dfs)

    # Compute mean for the last iteration
    pc_var_exp <-
      mean(
        unlist(
          lapply(pc_exp_df,
                 function (x) x[nrow(x), ])
        )
      )

    # TIME STAMP: End!
    end_time <- Sys.time()
    imp_time <- difftime(end_time, start_time, units = "mins")

    # Store results
    return(list(mids = mids_pcr_sim,
                pc_var_exp = pc_var_exp,
                pc_var_mat = pc_var_mat,
                time = as.vector(imp_time)))

    ### END TRYCATCH EXPRESSION
  }, error = function(e){

    err <- paste0("Original Error: ", e)
    print(err)
    return(list(mids = NULL,
                pc_var_exp = NULL,
                time = NULL))
  }
  )

}