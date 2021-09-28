### Title:    Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-09-28

imputePCA <- function(Z, imp_target, pcs_target, ncfs = 1, parms){
  
  ## Input: 
  # @Z: dataset w/ missing values,
  # @target: integer vector indexing important variables to impute
  # @cond: single line dataframe describing current condition
  # @parms: the initialization object parms
  
  ## output: 
  # - a list of chains imputed datasets at iteration iters
  # - per variable list of imputed values
  # - imputation run time
  
  # For internals
  ## Data
  # Z = dat_miss
  # imp_target = parms$vmap$ta
  # pcs_target = c(parms$vmap$mp, parms$vmap$ax)
  # ncfs <- 1

  ## body:
    tryCatch({

      start_time <- Sys.time()

      ## Single Imputation Run
      if(any(imp_target %in% pcs_target)){
        pMat     <- quickpred(Z, mincor = .3)
        ZDA_mids <- mice(Z,
                         m               = 1,
                         maxit           = 100,
                         predictorMatrix = pMat,
                         printFlag       = FALSE,
                         method          = "pmm")

        Z_tpca <- complete(ZDA_mids)
      } else {
        Z_tpca <- Z[, pcs_target]
      }

      # Extract PCs
      prcomp_out <- prcomp(sapply(Z_tpca, as.numeric),
                           center = TRUE,
                           scale = TRUE)
      prop_var_exp <- prop.table(prcomp_out$sdev^2)

      # Keep desired number of factors (based on how it was specified)
      if(ncfs >= 1){
        # ncfs as NOT a proportion
        pcs_keep <- 1:ncfs
        prcomp_dat <- prcomp_out$x[, pcs_keep, drop = FALSE]
      } else {
        # ncfs as a proportion
        pcs_keep <- which(cumsum(prop_var_exp) <= ncfs)
        # Check is not empty
        if(length(pcs_keep) == 0){
          pcs_keep <- 1
        }
        prcomp_dat <- prcomp_out$x[, pcs_keep, drop = FALSE]
      }
      pc_var_exp <- sum(prop_var_exp[pcs_keep])

      ## Define input data for imputation
      Z_input <- cbind(prcomp_dat, Z[, imp_target])

      ## Define predictor matrix
      pred_mat <- make.predictorMatrix(Z_input)
      if(any(imp_target %in% pcs_target)){
        # If we have included the imp_target variables that are pcs_target,
        # then we do not want to use them again in the imputaion
        pred_mat[, colnames(Z[, imp_target])] <- 0
      }

      ## Impute
      imp_PCA_mids <- mice::mice(Z_input,
                                 m      = parms$mice_ndt,
                                 maxit  = parms$mice_iters,
                                 predictorMatrix = pred_mat)

      # Track time it took
      end_time <- Sys.time()
      imp_PCA_time <- difftime(end_time, start_time, units = "mins")

      return(list(mids = imp_PCA_mids,
                  pc_var_exp = pc_var_exp,
                  time = as.vector(imp_PCA_time)))

      ### END TRYCATCH EXPRESSION
    }, error = function(e){
      err <- paste0("Original Error: ", e)
      print(err)
      return(list(mids = NULL,
                  pc_var_exp = NULL,
                  time = NULL)
      )
    }
    )
}
