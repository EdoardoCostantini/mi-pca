# Project:   mipca_compare
# Objective: subroutine runCell to run a single condition for a single rep
# Author:    Edoardo Costantini
# Created:   2021-05-12
# Modified:  2021-08-26
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
  dat_miss <- cbind(dat$ordi[, -parms$vmap$ta], target_miss)

# Imputation --------------------------------------------------------------

  if(cond$fpc == "all"){
    imputePCA <- imputePCA(dat_miss,
                           imp_target = parms$vmap$ta,
                           pcs_target = unlist(parms$vmap, use.names = FALSE),
                           ncfs = cond$npc,
                           parms = parms)
    mids_out <- imputePCA$mids
  }
  if(cond$fpc == "imp") {
    imputePCA <- imputePCA(dat_miss,
                           imp_target = parms$vmap$ta,
                           pcs_target = c(parms$vmap$mp, parms$vmap$ax),
                           ncfs = cond$npc,
                           parms = parms)
    mids_out <- imputePCA$mids
  }
  if(cond$fpc == "uni") {
    mids_out <- mice(sapply(dat_miss, as.numeric),
                     method = "pcr.mixed",
                     npcs = 1)
  }

# Analyze and pool --------------------------------------------------------

  ## Mean, variance, covariance
  # MI data
  mi_sat_fits <- miFitSat(mi_data = imp_out$dats,
                          model = satModWrite(names(imp_out$dats[[1]][,
                                                      parms$vmap_it$ta]))
  )
  mi_sat_pool <- miPool(mi_fits = mi_sat_fits,
                        m = parms$mice_ndt,
                        N = parms$N)

  # Original data and complete case analysis
  sd_data <- list(orig = dat$dat_ob,
                  omit = na.omit(dat_miss))
  sd_sat_fits <- miFitSat(mi_data = sd_data,
                          model = satModWrite(names(imp_out$dats[[1]][,
                                                      parms$vmap_it$ta]))
  )
  sd_sat_ests <- lapply(sd_sat_fits, function(x) {
    est_all <- parameterEstimates(x, standardized = TRUE)
    est <- est_all[, c("std.all", "est", "ci.lower", "ci.upper")]
    return(
      cbind(par = apply(est_all[, 1:3], 1, paste0, collapse = ""),
            est)
    )
  })
  sat_ests <- c(sd_sat_ests, mi = list(mi_sat_pool))

  ## Factor Loadings ##
  # MI data
  mi_cfa_fits <- miFitCFA(mi_data = imp_out$dats,
                       model = parms$CFA_model
  )
  mi_cfa_pool <- miPool(mi_fits = mi_cfa_fits,
                     m = parms$mice_ndt,
                     N = parms$N)

  # Original data and complete case analysis
  sd_cfa_fits <- miFitCFA(mi_data = sd_data, model = parms$CFA_model)
  sd_cfa_ests <- lapply(sd_cfa_fits, function(x) {
    est_all <- parameterEstimates(x, standardized = TRUE)
    est <- est_all[, c("std.all", "est", "ci.lower", "ci.upper")]
    return(
      cbind(par = apply(est_all[, 1:3], 1, paste0, collapse = ""),
            est)
    )
  })

  # Join into a list
  cfa_ests <- c(sd_cfa_ests, mi = list(mi_cfa_pool))

  # Combine all results into a single object
  res_sat_list <- lapply(sat_ests, reshape2::melt, id = "par")
  res_cfa_list <- lapply(cfa_ests, reshape2::melt, id = "par")
  res_sat_df <- do.call(rbind, res_sat_list)
  res_cfa_df <- do.call(rbind, res_cfa_list)
  res <- cbind(cond, rbind(res_sat_df, res_cfa_df))

# Store Output ------------------------------------------------------------
  
  ## Store Results
  saveRDS(res,
          file = paste0(fs$outDir,
                        "rep_", rp, "_", cond$tag,
                        ".rds")
  )

}

