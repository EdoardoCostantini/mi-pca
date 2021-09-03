### Title:    Data Generation Checks
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-09-03

## Make sure we have a clean environment:
rm(list = ls())

## Initialize the environment:
source("./init.R")

# Correlations as expected ------------------------------------------------
store_cors <- matrix(nrow = nrow(conds), ncol = 3)

for (i in 1:nrow(conds)){
  print(i)
  dat_list <- genData(parms = parms, cond = conds[i, ])
  cormat <- round(cor(dat_list), 3)
  ta_cor <- cormat[parms$vmap$ta, parms$vmap$ta][upper.tri(diag(length(parms$vmap$ta)))]
  mp_cor <- cormat[parms$vmap$mp, parms$vmap$mp][upper.tri(diag(length(parms$vmap$mp)))]
  ax_cor <- cormat[parms$vmap$ax, parms$vmap$ax][upper.tri(diag(length(parms$vmap$ax)))]
  store_cors[i, ] <- sapply(list(ta_cor, mp_cor, ax_cor), mean)
}

round(store_cors, 1)

# Categorical distributions ------------------------------------------------
dat_list <- genData(parms = parms, cond = conds[13, ])
par(mfrow = c(4,3))
apply(dat_list, 2, function (x) plot(density(x)))

# MAR effect ---------------------------------------------------------------

  library(lattice)

  ## Data
  plots_nomar <- list()
  plots_mar <- list()

  for(i in 6:10){
    dat_list <- genData(parms = parms, cond = conds[i, ])
    cor(dat_list)[, parms$vmap$mp, drop = FALSE]

    ## Impose Missingness
    preds   <- dat_list[, parms$vmap$mp, drop = FALSE]
    targets <- dat_list[, parms$vmap$ta, drop = FALSE]
    target_miss <- amputePerVar(targets = targets,
                                preds = preds,
                                pm = parms$pm,
                                type = "high")
    dat_miss <- cbind(dat_list[, -parms$vmap$ta], target_miss)

    # Imputation --------------------------------------------------------------
    plots_nomar[[i-5]] <- densityplot(~ z1, data = data.frame(dat_miss),
                                      groups =  is.na(z10),
                                      par.settings = list(superpose.line = list(col = c("blue","red"))),
                                      auto.key = TRUE)
    plots_mar[[i-5]] <- densityplot(~ z5, data = data.frame(dat_miss),
                                    groups =  is.na(z10),
                                    par.settings = list(superpose.line = list(col = c("blue","red"))),
                                    auto.key = TRUE)
  }
  plots_nomar
  plots_mar
