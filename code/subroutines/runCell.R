# Project:   mipca_compare
# Objective: subroutine runCell to run a single condition for a single rep
# Author:    Edoardo Costantini
# Created:   2021-05-12
# Modified:  2021-09-29
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

  if(cond$method == "all"){
    imp_out <- imputePCA(dat_miss,
                         imp_target = parms$vmap$ta,
                         pcs_target = unlist(parms$vmap, use.names = FALSE),
                         ncfs = cond$npc,
                         parms = parms)
  }
  if(cond$method == "imp") {
    imp_out <- imputePCA(dat_miss,
                         imp_target = parms$vmap$ta,
                         pcs_target = c(parms$vmap$mp, parms$vmap$ax),
                         ncfs = cond$npc,
                         parms = parms)
  }
  if(cond$method == "vbv") {
    imp_out <- imputePCAvbv(Z = sapply(dat_miss, as.numeric),
                            ncfs = cond$npc)
  }

  # MICE w/ true missing data imposition model (optimal)
  if(cond$method == "MITR") {
    imp_out <- imputeMICE(Z = dat_miss,
                           imp_target = parms$vmap$ta,
                           preds = c(parms$vmap$ta, parms$vmap$mp),
                           parms = parms)
  }

  # MICE w/ minimal missing data models (minimal)
  if(cond$method == "MIMI") {
    imp_out <- imputeMICE(Z = dat_miss,
                           imp_target = parms$vmap$ta,
                           preds = parms$vmap$ta,
                           parms = parms)
  }

# Analyze and pool --------------------------------------------------------

  if(cond$method %in% c("all", "imp", "vbv", "MITR", "MIMI")){
    ## Estimate Mean, variance, covariance
    fits <- fitSatModel(mids = imp_out$mids,
                        model = genLavaanMod(dat_miss,
                                             targets = parms$vmap$ta)
    )

    ## Pool mean, variance, covariance
    pooled_sat <- poolSatMod(fits)

    ## Estimate and pool regression coefficients
    pooled_cor <- poolCor(imp_out$mids, targets = parms$vmap$ta)

    ## Join outputs
    res <- rbind(pooled_sat, pooled_cor)
  }

  ## Complete Case analysis
  if(cond$method == "CC"){
    res <- fitSingleData(na.omit(dat_miss), targets = parms$vmap$ta)
  }

  ## Original data
  if(cond$method == "OG"){
    res <- fitSingleData(dat$cont, targets = parms$vmap$ta)
  }

  ## Attach condition tags
  res <- cbind(cond, res)

# Store Output ------------------------------------------------------------
  
  ## Store Results
  saveRDS(res,
          file = paste0(fs$outDir,
                        "rep_", rp, "_", cond$tag,
                        ".rds")
  )

}

