### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-07-20

ampute_step <- function(dat_in, parms){
  ## Description
  # Adds Matrix Design missingness to the MAR
  ## Example Inputs
  # cond <- conds[1, ]
  # dat_in <- genData(parms = parms, cond = cond)
  # plot = TRUE
  
# MAR ---------------------------------------------------------------------

  # Define dataset
  dat_init <- cbind(dat_in$dat_ob[, parms$varMap_items$ta],
                    lv = dat_in$dat_lv[, parms$varMap$ta])

  patts <- cbind(parms$patts,
                 lv = matrix(1,
                             nrow = nrow(parms$patts),
                             ncol = length(parms$varMap$ta)))
  patts <- patts[sample(1:nrow(patts), 5), ]

  mar_weights_v <- c(rep(0, (ncol(patts)-length(parms$varMap$ta))),
                     rep(1, length(parms$varMap$ta)))
  mar_weights <- matrix(rep(mar_weights_v, nrow(patts)),
                        ncol = ncol(patts), byrow = TRUE)

  # Impose miss
  ampute_out <- ampute(dat_init,
                       prop = .5,
                       patterns = patts,
                       weights = mar_weights,
                       mech = "MAR")
  dat_miss <- ampute_out$amp

  # Restructure original data
  dat_out <- cbind(dat_miss[, parms$varMap_items$ta],
                   dat_in$dat_ob[, -parms$varMap_items$ta])

  # Result
  return( dat_out )
}