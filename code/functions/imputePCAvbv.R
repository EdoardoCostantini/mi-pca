# Project:   mipca_compare
# Objective: function for imputation using PCA at every iteration
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-09-29

imputePCAvbv <- function(Z, ncfs = 1){

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
                                 method = "pcr.mixed",
                                 npcs = ncfs_int)
    # pc_var_exp_df <- do.call(rbind, imp_out$pcs)
    pc_var_exp <- mean(unlist(mids_pcr_sim$pcs), na.rm = TRUE)

    # TIME STAMP: End!
    end_time <- Sys.time()
    imp_time <- difftime(end_time, start_time, units = "mins")

    # Store results
    return(list(mids = mids_pcr_sim,
                pc_var_exp = pc_var_exp,
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