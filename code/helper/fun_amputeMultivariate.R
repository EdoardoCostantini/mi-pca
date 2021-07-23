### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-07-22

amputeMultivariate <- function(miss_target, miss_preds, parms, seed = NULL){
  ## Description
  # Uses the mice::ampute() function to impose missing values on
  # miss_target based on miss_preds.
  # It returns the variables in miss_target with missing values imposed.
  ## Example Inputs
  # cond <- conds[1, ]
  # dat_start <- genData(parms = parms, cond = cond)
  # miss_target <- dat_start$dat_ob[, parms$varMap_items$ta]
  # miss_preds <- dat_start$dat_ob[, parms$varMap_items$mp]
  
# MAR ---------------------------------------------------------------------

  # Define dataset
  dat_init <- cbind(miss_target,
                    miss_preds)

  # Define possible patterns
  space <- matrix(rep(c(0, 1), length(parms$varMap_items$ta)),
                  ncol = 2, byrow = TRUE,
                  dimnames = list(NULL, c("obs", "mis")))
  patts_all <- do.call(expand.grid,
                       split(space,
                             rep(1:nrow(space), ncol(space)))
  )
  patts_mis <- patts_all[-c(nrow(patts_all)), ]
  if(!is.null(seed)){
    set.seed(seed)
  }
  patts_sel <- patts_mis[sample(1:nrow(patts_mis), 5), ]
  patts_ful <- cbind(patts_sel, pred = matrix(
    1,
    nrow = nrow(patts_sel),
    ncol = ncol(miss_preds)
  )
  )

  pred_weights_v <- c(rep(0, ncol(miss_target)), rep(1, ncol(miss_preds)))
  pred_weights <- matrix(rep(pred_weights_v, nrow(patts_ful)),
                        ncol = ncol(patts_ful), byrow = TRUE)

  # Impose miss
  ampute_out <- ampute(dat_init,
                       prop = parms$pm,
                       patterns = patts_ful,
                       weights = pred_weights,
                       mech = "MAR")
  dat_miss <- ampute_out$amp

  # Result
  return( dat_miss )
}