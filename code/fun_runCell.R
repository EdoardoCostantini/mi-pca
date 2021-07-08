### Title:    Subroutine runCell
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-12
### Modified: 2020-07-08
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs
###           imputations for every condition in the set.

runCell <- function(cond, parms, 
                    rep_status,
                    cluster = FALSE) {

# Example Internals -------------------------------------------------------
  
  # set.seed(1234)
  # cond    = conds[6, ]
  # cluster = FALSE 
  # if you are running on lisa you want to store differently

# Data Generation ---------------------------------------------------------

  ## Data
  dat_list <- genData(parms = parms, cond = cond)

  ## Impose Missingness
  dat_miss <- imposeNA(dat_list, parms = parms)

# Imputation --------------------------------------------------------------

  ## Method 1
  
  ## Method 2
  
  ## Convergence
  
# Analyze and pool --------------------------------------------------------

  ## Analysis 1: Means, variances, covariances
  lm(z1 ~ z2 + z3 + z4 + z5 + z6, data = dat_miss)
  lm(z1 ~ z2 + z3 + z4 + z5 + z6, data = dat_list$dat_ob)

  ## Analysis 2: CFA parms
  CFA()
  
  ## Analysis 3: Time

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

