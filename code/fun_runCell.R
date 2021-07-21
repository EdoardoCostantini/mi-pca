### Title:    Subroutine runCell
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-12
### Modified: 2021-07-21
### Note:     A "cell" is a cycle through the set of conditions.
###           The function in this script generates 1 data set, performs
###           imputations for every condition in the set.

runCell <- function(cond, parms, rp) {

# Example Internals -------------------------------------------------------

  # cond = conds[9, ]
  # rp   = 1

# Data Generation ---------------------------------------------------------

  ## Data
  dat_list <- genData(parms = parms, cond = cond)

  ## Impose Missingness
  dat_miss <- imposeNA(dat_list, parms = parms)

# Imputation --------------------------------------------------------------

  imp_out <- imputePCA(dat_miss, target = parms$varMap_items$ta,
                       cond = cond, parms = parms)

# Analyze and pool --------------------------------------------------------

  ## Mean, variance, covariance
  # MI data
  mi_sat_fits <- miFitSat(mi_data = imp_out$dats,
                       model = satModWrite(names(imp_out$dats[[1]][,
                                                   parms$varMap_items$ta]))
  )
  mi_sat_pool <- miPool(mi_fits = mi_sat_fits,
                     m = parms$mice_ndt,
                     N = parms$N)

  # Original data and complete case analysis
  sd_data <- list(orig = dat_list$dat_ob,
                  omit = na.omit(dat_miss))
  sd_sat_fits <- miFitSat(mi_data = sd_data,
                          model = satModWrite(names(imp_out$dats[[1]][,
                                                      parms$varMap_items$ta]))
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
  saveRDS(output,
          file = paste0(parms$outDir, "id_"
                        rp, "_",
                        cond_tag,
                        ".rds")
  )

}

