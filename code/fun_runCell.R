### Title:    Subroutine runCell
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-12
### Modified: 2020-07-19
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs
###           imputations for every condition in the set.

runCell <- function(cond, parms, 
                    rep_status,
                    cluster = FALSE) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[9, ]
  # cluster = FALSE 
  # if you are running on lisa you want to store differently

# Data Generation ---------------------------------------------------------

  ## Data
  dat_list <- genData(parms = parms, cond = cond)

  ## Impose Missingness
  dat_miss <- imposeNA(dat_list, parms = parms)

# Imputation --------------------------------------------------------------

  imp_out <- imputePCA(dat_miss, target = parms$varMap_items$ta,
                       cond = cond, parms = parms)
  
# Analyze and pool --------------------------------------------------------

  ## Factor Loadings ##

  cfa(parms$CFA_model,
      data = imp_out$dats$`1`,
      std.lv = TRUE)

  # Pool paramters
  CFA_est <- sapply(semR_fit_mi[lapply(semR_fit_mi, length) != 0],
                    sem_pool_EST_f)
  CFA_ci  <- sapply(semR_fit_mi[lapply(semR_fit_mi, length) != 0],
                    sem_pool_CI_f)
  CFA_fmi <- sapply(semR_fit_mi[lapply(semR_fit_mi, length) != 0],
                    .fmi_compute)

  ## Analysis on original and CC
    imp_no_dats <- list(orig = dat_list$dat_ob,
                        omit = na.omit(dat_miss))

    # Fit CFA model on Categorical (scaled) data
    fits <- lapply(imp_no_dats, function(x) {
      x <- imp_no_dats[[1]]
      head(x)
      # Treat as categorical data?
      if(cond$K >= 5){
        order_status <- NULL
      } else {
        factor_names <- names(which(sapply(x, is.factor))) # identify facotrs
        factor_index <- which(names(x) %in% factor_names) # their index
        order_status <- factor_names[factor_index %in% parms$varMap_items$ta] # only in ta
      }
      cfa(parms$CFA_model,
          data = x,
          ordered = order_status,
          std.lv = TRUE)
    })
    ests <- lapply(fits, function(x){
      parameterEstimates(x,
                         se = TRUE, ci = TRUE,
                         zstat = FALSE, pvalue = FALSE,
                         standardized = FALSE)
    })

# Store Output ------------------------------------------------------------

  ## Define storing object
  output <- list(cond         = cond,
                 dat_full     = Xy,
                 dat_miss     = Xy_mis,
                 # SEM raw
                 semR_EST     = semR_est_all,
                 semR_CI      = semR_ci_all,
                 # CFA raw
                 CFA_EST      = CFA_est_all,
                 CFA_CI       = CFA_ci_all,
                 # SEM Scored
                 semS_EST     = semS_est_all,
                 semS_CI      = semS_ci_all,
                 # Other
                 fmi          = list(semR = semR_fmi,
                                     CFA  = CFA_fmi,
                                     semS = semS_fmi),
                 miss_descrps = list(miss_descrps = miss_descrps,
                                     PCA_diff_comp = imp_PCA$diff_comp,
                                     IURR_AS_size = imp_IURR_ls$Xy_mis$succ_ratio),
                 run_time_min = imp_time,
                 run_time_prep = prepro_time_end - prepro_time_start,
                 imp_values   = imp_values)[parms$store]
  
  ## Store Results
  if(cluster == TRUE){
    saveRDS(output,
            file = paste0(parms$outDir,
                          "exp", parms$exp,
                          "_rep", rp,
                          "_cond", cond$id, "_",
                          cond_tag,
                          ".rds")
    )
  } else {
    return(output)
  }
}

