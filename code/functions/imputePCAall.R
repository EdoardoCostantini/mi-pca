### Title:    Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-11-24

imputePCAall <- function(Z, imp_target, pcs_target, ncfs = 1,
                         Z_ori = NULL, parms){

  ## Input: 
  # @Z: dataset w/ missing values,
  # @target: integer vector indexing important variables to impute
  # @cond: single line dataframe describing current condition
  # @parms: the initialization object parms
  # @Z_ori: the original dataset

  ## output: 
  # - a list of chains imputed datasets at iteration iters
  # - per variable list of imputed values
  # - imputation run time

  # For internals
  ## Data
  # Z = dat_miss
  # imp_target = parms$vmap$ta
  # pcs_target = c(parms$vmap$mp, parms$vmap$ax)
  # ncfs = 1
  # Z_ori = dat_ordi

  ## body:
  ## Process the number of predictors
  if(grepl("max", ncfs)){
    ncfs_int <- length(pcs_target)
  } else {
    ncfs_int <- as.numeric(ncfs)
  }

  start_time <- Sys.time()
  ## Check if we are running oracle version or not
  if(is.null(Z_ori)){
    # Single Imputation to allow PCA
    pMat     <- quickpred(Z, mincor = .3)
    Z_SI_mids <- mice(Z,
                      m               = 1,
                      maxit           = 20,
                      predictorMatrix = pMat,
                      printFlag       = FALSE,
                      method          = "norm")
    Z_SI <- complete(Z_SI_mids)

    # Prepare object for prcomp
    Z_pca <- apply(Z_SI, 2, as.numeric)
  } else {
    # Use original data for PCA
    Z_pca <- apply(Z_ori, 2, as.numeric)
  }

  # Extract PCs
  prcomp_out <- prcomp(Z_pca,
                       center = TRUE,
                       scale = TRUE)
  PVE <- prop.table(prcomp_out$sdev^2)

  # Define which pcs to keep (flexible to proportion of v explained)
  if(ncfs_int >= 1){
    # ncfs_int is a number
    pcs_keep <- 1:ncfs_int
  } else {
    # ncfs_int is a proportion
    pcs_keep <- which(cumsum(PVE) <= ncfs_int)
    # Check is not empty
    if(length(pcs_keep) == 0){
      pcs_keep <- 1
    }
  }

  # Keep dataset of PC predictors
  prcomp_dat <- prcomp_out$x[, pcs_keep, drop = FALSE]

  # Keep CPVE value
  CPVE <- sum(PVE[pcs_keep])

  ## Define input data for imputation
  Z_mice <- cbind(Z[, imp_target], prcomp_dat)

  ## Define predictor matrix
  pred_mat <- make.predictorMatrix(Z_mice)
  pred_mat[, colnames(Z[, imp_target])] <- 0

  ## Impute
  imp_PCA_mids <- mice::mice(Z_mice,
                             m      = parms$mice_ndt,
                             method = "norm.boot",
                             maxit  = 1,
                             printFlag = FALSE,
                             predictorMatrix = pred_mat,
                             eps = 0)

  # Track time it took
  end_time <- Sys.time()
  imp_PCA_time <- difftime(end_time, start_time, units = "mins")

  return(list(mids = imp_PCA_mids,
              CPVE = CPVE,
              time = as.vector(imp_PCA_time)))

}
