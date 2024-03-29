# Project:   mipca_compare
# Objective: subroutine runCell to run a single condition for a single rep
# Author:    Edoardo Costantini
# Created:   2021-05-12
# Modified:  2022-09-12
# Note:      A "cell" is a cycle through the set of conditions.
#            The function in this script generates 1 data set, performs
#            imputations for every condition in the set.

runCell <- function(rp, cond, fs, parms) {

  # Example Internals -------------------------------------------------------

  # cond = conds[2, ]
  # rp   = 1
  tryCatch({
    ### START TRYCATCH EXPRESSION
    # Data Generation ---------------------------------------------------------

    if(cond$lv == TRUE){
      dat <- genDataLatent(parms = parms, cond = cond)
    }
    if(cond$lv == FALSE){
      dat <- genData(parms = parms, cond = cond)
    }
    dat_ordi <- disData(x = dat$x, K = cond$K, parms = parms)

    ## Impose Missingness
    preds   <- dat$x[, parms$vmap$mp, drop = FALSE]
    targets <- dat_ordi[, parms$vmap$ta, drop = FALSE]
    target_miss <- amputePerVar(targets = targets,
                                preds = preds,
                                pm = parms$pm,
                                type = "high")
    dat_miss <- cbind(target_miss, dat_ordi[, -parms$vmap$ta])

    # Imputation --------------------------------------------------------------

    if(cond$method == "all"){
      imp_out <- imputePCAall(dat_miss,
                              imp_target = parms$vmap$ta,
                              pcs_target = 1:ncol(dat_miss),
                              ncfs = cond$npc,
                              parms = parms)
    }
    if(cond$method == "all_oracle"){
      imp_out <- imputePCAall(Z = dat_miss,
                              imp_target = parms$vmap$ta,
                              pcs_target = 1:ncol(dat_miss),
                              ncfs = cond$npc,
                              Z_ori = dat_ordi,
                              parms = parms)
    }
    if(cond$method == "aux") {
      imp_out <- imputePCAaux(dat_miss,
                              imp_target = parms$vmap$ta,
                              pcs_target = (tail(parms$vmap$ta, 1)+1):ncol(dat_miss),
                              ncfs = cond$npc,
                              parms = parms)
    }
    if(cond$method == "vbv") {
      imp_out <- imputePCAvbv(dat_miss,
                              ncfs = cond$npc,
                              parms = parms)
    }

    # MICE w/ optimal choices
    if(cond$method == "MIOP") {
      imp_out <- imputeMICE(Z = dat_miss,
                            imp_target = parms$vmap$ta,
                            preds = c(parms$vmap$ta,
                                      parms$vmap$mp,
                                      # auxiliaries that are not junk
                                      dat$index_good_aux
                            ),
                            parms = parms)
    }

    # MICE w/ oracle knowledge
    if(cond$method == "MIOR") {
      imp_out <- imputeMICE(Z = dat_miss,
                            imp_target = parms$vmap$ta,
                            preds = c(parms$vmap$ta,
                                      parms$vmap$mp),
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

    if(exists("imp_out")){
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
      res <- rbind(pooled_sat, pooled_cor, make.row.names = FALSE)
    }

    ## Complete Case analysis
    if(cond$method == "CC"){
      res <- fitSingleData(na.omit(dat_miss), targets = parms$vmap$ta)
    }

    ## Original data
    if(cond$method == "OG"){
      res <- fitSingleData(dat$x, targets = parms$vmap$ta)
    }

    ## Define explained variance information
    if(exists("imp_out")){         # If imputation
      if("CPVE" %in% names(imp_out)){   # - PCA used
        PC_exp <- imp_out$CPVE
      } else {                          # - PCA not used
        PC_exp <- NA
      }
    } else {                          # If no Imputation
      PC_exp <- NA
    }

    ## Attach condition tags
    row.names(cond) <- NULL # to avoid warning
    res <- cbind(rp = rp, cond, PC_exp = PC_exp, res)

    # Store Output ------------------------------------------------------------

    ## Store Main Results
    saveRDS(res,
            file = paste0(fs$outDir,
                          "rep_", rp, "_", cond$tag,
                          "_main",
                          ".rds")
    )

    ## Store Time Results
    if(exists("imp_out")){
      saveRDS(cbind(cond, time = imp_out$time),
              file = paste0(fs$outDir,
                            "rep_", rp, "_", cond$tag,
                            "_time",
                            ".rds")
      )
    }

    ## Store Cumulative Explained Variance in vbv case
    if(cond$method == "FREEZE"){ # usually equal to "vbv"
      pc_res <- base::cbind(cond, imp_out$CPVE_mat)
      saveRDS(pc_res,
              file = paste0(fs$outDir,
                            "rep_", rp, "_", cond$tag,
                            "_CPVE",
                            ".rds")
      )
    }

    ## Store mids results if run requires it
    if(parms$run_type == 2){
      saveRDS(imp_out$mids,
              file = paste0(fs$outDir,
                            "rep_", rp, "_", cond$tag,
                            "_mids",
                            ".rds")
      )
    }

    ### END TRYCATCH EXPRESSION
  }, error = function(e){
    err <- paste0("Original Error: ", e)
    err_res <- cbind(rp = rp, cond, Error = err)
    saveRDS(err_res,
            file = paste0(fs$outDir,
                          "rep_", rp, "_", cond$tag,
                          "_ERROR",
                          ".rds")
    )
    return(NULL)
  }
  )

}

