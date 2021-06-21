### Title:    Subroutine runCell
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-12
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

  ## Condition Tag
  cond_tag <- paste0(names(cond), 
                     sub("\\.", "", as.character(round(cond, 2))),
                     collapse = "-")
  
  ## Data
  # Generate Continuous Data
  dat_list <- genData(parms = parms, cond = cond)
  dat_cont <- dat_list$dat_ob

  # Discretise
  index_keep_continuous <- 1:(max(parms$varMap$ta)*parms$J)
  dat_disc <- apply(dat_cont[, -index_keep_continuous],
                    2,
                    function(j){
                      as.numeric(cut(j, breaks = cond$K))
                    })

  dat_disc <- cbind(dat_cont[, index_keep_continuous], dat_disc)

  # Generate Continuous Data w/ attenuated relationships
  Sigma <- cor(dat_disc)
  mu <- rep(0, ncol(dat_disc))
  dat_disc_cont <- MASS::mvrnorm(parms$N, mu, Sigma)
  head(dat_disc_cont)

  ## Impose Missingness
  
# Imputation --------------------------------------------------------------

  ## Method 1
  
  ## Method 2
  
  ## Convergence
  
# Analyze and pool --------------------------------------------------------

  ## Analysis 1: Means, variances, covariances
  
  ## Analysis 2: CFA parms
  
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

