### Title:    Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-07-19

imputePCA <- function(Z, target, cond, parms){
  
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
  # Z <- amputePerVar(genData(parms = parms, cond = cond), parms = parms)
  # target = parms$vmap_it$ta

  ## body:
    tryCatch({

      start_time <- Sys.time()

      # Data
      O = as.data.frame(!is.na(Z)) # matrix index of observed values
      usable_min <- min(colSums(O)) # smallest observed sample size

      if(sum(is.na(Z[, -target])) > 0){
        # Fill in cases when ZDA has missing values
        print("PCA Impute: Filling in auxiliary NAs")

        pMat     <- quickpred(Z, mincor = .3)
        ZDA_mids <- mice(Z,
                         m               = 1,
                         maxit           = 100,
                         predictorMatrix = pMat,
                         printFlag       = TRUE,
                         ridge           = cond$ridge,
                         method          = "pmm")

        Z_out <- complete(ZDA_mids)

        # Define Single Imputed data as the auxiliary set
        Z[, -target] <- Z_out[, -target]

        # Define dataset for output
        Z_out <- Z

      } else {
        Z_out <- Z # Need it for output consistency
      }

      # If there are categorical variables, then use FAMD
      if("factor" %in% sapply(Z, class)){

        res.famd <- FAMD(Z[, -target], graph = FALSE, ncp = ncol(Z))
        Z_pca <- res.famd$ind$coord[, 1:cond$npcs, drop = FALSE]

      } else {

        Z_PC <- model.matrix(~ ., Z[, -target])[, -1]

        # Clean model matrix
        emptyvars <- names(which(apply(Z_PC, 2, var) == 0))
        Z_PC_clean <- Z_PC[, !colnames(Z_PC) %in% emptyvars]

        # Extract PCs
        pcaOut <- prcomp(Z_PC_clean, scale = TRUE, retx = TRUE)

        ## Compute and store the cumulative proportion of variance explained by
        ## the component scores:
        rSquared <- cumsum(pcaOut$sdev^2) / sum(pcaOut$sdev^2)

        ## Extract the principal component scores:
        Z_pca <- pcaOut$x[, rSquared <= parms$PCA_pcthresh]

      }

      ## Define Imputation methods
      Z_input <- cbind(Z[, target], Z_pca)

      ## Impute
      print("PCA Impute: Performing Multiple Imputation")
      imp_PCA_mids <- mice::mice(Z_input,
                                 m      = parms$mice_ndt,
                                 maxit  = parms$mice_iters)

      # Store results
      print("PCA Impute: Storing Results")
      imp_PCA_PC_dats <- mice::complete(imp_PCA_mids, "all")

      # Fill into orignal datasets the imputations (get rid of PCs)
      imp_PCA_dats <- lapply(imp_PCA_PC_dats, function(x){
        df_temp <- Z
        df_temp[, target] <- x[, target]
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
