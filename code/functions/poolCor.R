# Project:   mipca_compare
# Objective: Estimate, pool and sturcture correlation output
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-09-28

poolCor <- function (mids, targets, alphaCI = .95){

  ## Internals
  # mids = mids_out$mids
  # targets = parms$vmap$ta
  # alphaCI = .95

  ## Body
  cor_out <- miceadds::micombine.cor(mids,
                                     variables = targets,
                                     conf.level = alphaCI,
                                     method = "pearson",
                                     nested = FALSE,
                                     partial = NULL)

  ## Identify unique correlations
  cor_unique_index <- !duplicated(t(apply(cor_out, 1, sort)))
  cor_unique <- cor_out[cor_unique_index, ]

  ## Store name of parameter
  cor_unique <- cbind(par = paste0(cor_unique$variable1,
                                   "r",
                                   cor_unique$variable2),
                      cor_unique)

  ## Drop useless column
  cor_select <- cor_unique[, c("par", "r", "lower95", "upper95")]

  ## Change names
  colnames(cor_select) <- c("par", "Q_bar", "lwr", "upr")

  return(cor_select)
}

