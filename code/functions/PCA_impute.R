### Title:    Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-08-25

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
  # imp_target = parms$vmap_it$ta
  # pcs_target = c(parms$vmap_it$mp, parms$vmap_it$ax)
  # criterion = c("first", "half")[2]
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
      pcaout <- principal(sapply(Z_tpca, as.numeric),
                          nfactors = ncfs,
                          cor = "mixed")

      ## Define input data for imputation
      Z_input <- cbind(pcaout$scores, Z[, imp_target])

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
                  time = imp_PCA_time))

      ### END TRYCATCH EXPRESSION
    }, error = function(e){
      err <- paste0("Original Error: ", e)
      print(err)
      return(list(mids = NULL,
                  time = NULL)
      )
    }
    )
}
