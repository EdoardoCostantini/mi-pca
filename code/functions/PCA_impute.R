### Title:    Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-08-25

imputePCA <- function(Z, imp_target, pcs_target, parms){
  
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
        Z_tpca <- Z
      }

      # Data
      O <-  as.data.frame(!is.na(Z)) # matrix index of observed values
      usable_min <- min(colSums(O)) # smallest observed sample size
      Z_PC <- model.matrix(~ ., Z_tpca[, pcs_target])[, -1]

      # Clean model matrix
      emptyvars <- names(which(apply(Z_PC, 2, var) == 0))
      Z_PC_clean <- Z_PC[, !colnames(Z_PC) %in% emptyvars]

      # Extract PCs
      pcaOut <- prcomp(Z_PC_clean, scale = TRUE, retx = TRUE)

      ## Compute and store the cumulative proportion of variance explained by
      ## the component scores:
      rSquared <- cumsum(pcaOut$sdev^2) / sum(pcaOut$sdev^2)

      ## Extract the principal component scores:
      if(criterion == "first"){
        Z_pca <- pcaOut$x[, 1, drop = FALSE]
      }
      if(criterion == "half"){
        Z_pca <- pcaOut$x[, rSquared <= .5]
      }

      ## Define input data for imputation
      Z_input <- cbind(Z[, imp_target], Z_pca)

      ## Define predictor matrix
      pred_mat <- make.predictorMatrix(Z_input)
      if(any(imp_target %in% pcs_target)){
        # If we have included the imp_target variables that are pcs_target,
        # then we do not want to use them again in the imputaion
        pred_mat[, imp_target] <- 0
      }

      ## Impute
      imp_PCA_mids <- mice::mice(Z_input,
                                 m      = parms$mice_ndt,
                                 maxit  = parms$mice_iters,
                                 predictorMatrix = pred_mat)

      # Store results
      print("PCA Impute: Storing Results")
      imp_PCA_PC_dats <- mice::complete(imp_PCA_mids, "all")

      # Fill into orignal datasets the imputations (get rid of PCs)
      imp_PCA_dats <- lapply(imp_PCA_PC_dats, function(x){
        df_temp <- Z
        df_temp[, imp_target] <- x[, imp_target]
        return(df_temp)
      })

      # Track time it took
      end_time <- Sys.time()
      imp_PCA_time <- difftime(end_time, start_time, units = "mins")

      return(list(dats = imp_PCA_dats,
                  mids = imp_PCA_mids,
                  time = imp_PCA_time,
                  usable_min = usable_min))

      ### END TRYCATCH EXPRESSION
    }, error = function(e){
      err <- paste0("Original Error: ", e)
      print(err)
      return(list(dats = NULL,
                  mids = NULL,
                  time = NULL,
                  usable_min = NULL)
      )
    }
    )
}
