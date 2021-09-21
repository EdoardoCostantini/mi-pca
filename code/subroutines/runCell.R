# Project:   mipca_compare
# Objective: subroutine runCell to run a single condition for a single rep
# Author:    Edoardo Costantini
# Created:   2021-05-12
# Modified:  2021-09-21
# Note:      A "cell" is a cycle through the set of conditions.
#            The function in this script generates 1 data set, performs
#            imputations for every condition in the set.

runCell <- function(cond, parms, rp) {

# Example Internals -------------------------------------------------------

  # cond = conds[9, ]
  # rp   = 1

# Data Generation ---------------------------------------------------------

  dat <- genData(parms = parms, cond = cond)

  ## Impose Missingness
  preds   <- dat$cont[, parms$vmap$mp, drop = FALSE]
  targets <- dat$ordi[, parms$vmap$ta, drop = FALSE]
  target_miss <- amputePerVar(targets = targets,
                              preds = preds,
                              pm = parms$pm,
                              type = "high")
  dat_miss <- cbind(target_miss, dat$ordi[, -parms$vmap$ta])

# Imputation --------------------------------------------------------------

  if(cond$fpc == "all"){
    pca_out <- imputePCA(dat_miss,
                         imp_target = parms$vmap$ta,
                         pcs_target = unlist(parms$vmap, use.names = FALSE),
                         ncfs = cond$npc,
                         parms = parms)
    mids_out <- pca_out$mids
  }
  if(cond$fpc == "imp") {
    pca_out <- imputePCA(dat_miss,
                         imp_target = parms$vmap$ta,
                         pcs_target = c(parms$vmap$mp, parms$vmap$ax),
                         ncfs = cond$npc,
                         parms = parms)
    mids_out <- pca_out$mids
  }
  if(cond$fpc == "uni") {
    mids_out <- mice(sapply(dat_miss, as.numeric),
                     method = "pcr.mixed",
                     npcs = 1)
  }

  # MICE w/ true missing data imposition model (optimal)
  mids_out <- impute_MICE(Z = dat_miss,
                          imp_target = parms$vmap$ta,
                          preds = c(parms$vmap$ta, parms$vmap$mp),
                          parms = parms)

  # MICE w/ minimal missing data models (minimal)
  mids_out <- impute_MICE(Z = dat_miss,
                          imp_target = parms$vmap$ta,
                          preds = parms$vmap$ta,
                          parms = parms)

# Analyze and pool --------------------------------------------------------

  ## Estimate Mean, variance, covariance
  fits <- fitSatModel(mids = mids_out$mids,
                      model = genLavaanMod(dat_miss,
                                           targets = parms$vmap$ta)
  )

  ## Pool mean, variance, covariance
  pooled_sat <- poolSatMod(fits)

  ## Estimate and pool regression coefficients
  pooled_cor <- poolCor(mids_out$mids, targets = parms$vmap$ta)

  ## Join outputs
  pooled_est <- rbind(pooled_sat, pooled_cor)

  ## Attach condition tags
  res <- cbind(cond, pooled_est)

# Store Output ------------------------------------------------------------
  
  ## Store Results
  saveRDS(res,
          file = paste0(fs$outDir,
                        "rep_", rp, "_", cond$tag,
                        ".rds")
  )

}

