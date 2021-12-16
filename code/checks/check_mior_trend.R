# Project:   mipca_compare
# Objective: Check the discrete trend for MI-OR
# Author:    Edoardo Costantini
# Created:   2021-12-02
# Modified:  2021-12-16

source("init.R")

# Define a condition
cond <- conds[1, ]
cond$pj <- 0
cond$lv <- FALSE

set.seed(1234)
reps <- 25

logged <- matrix(NA, nrow = reps, ncol = length(K))
avg_corrs <- matrix(NA, nrow = reps, ncol = length(K))
max_corrs <- matrix(NA, nrow = reps, ncol = length(K))

pb <- txtProgressBar(min = 0, max = reps, style = 3)
for(i in 1:reps){
  for (k in K){
    # K condition definition
    cond$K <- k

    # Generate data
    dat <- genData(parms = parms, cond = cond)
    dat_ordi <- disData(x = dat$x, K = cond$K, parms = parms)
    dat_ordi <- apply(as.matrix(dat_ordi), 2, as.numeric)

    cor_mat <- cor(apply(as.matrix(dat_ordi[, 5:8]), 2, as.numeric))
    avg_corrs[i, (K %in% k)] <- mean(cor_mat[upper.tri(cor_mat)])
    max_corrs[i, (K %in% k)] <- max(cor_mat[upper.tri(cor_mat)])

    ## Impose Missingness
    preds   <- dat$x[, parms$vmap$mp, drop = FALSE]
    targets <- dat_ordi[, parms$vmap$ta, drop = FALSE]
    target_miss <- amputePerVar(targets = targets,
                                preds = preds,
                                pm = parms$pm,
                                type = "high")
    dat_miss <- cbind(target_miss, dat_ordi[, -parms$vmap$ta])

    ## Make predictor matrix
    imp_target <- parms$vmap$ta
    preds <- c(parms$vmap$ta, parms$vmap$mp)
    pred_mat <- make.predictorMatrix(dat_miss)
    pred_mat[imp_target, -preds] <- 0

    ## Impute
    imp_mids <- mice::mice(dat_miss,
                           predictorMatrix = pred_mat,
                           m = parms$mice_ndt,
                           maxit = parms$mice_iters,
                           method = "norm",
                           printFlag = FALSE)

    logged[i, (K %in% k)] <- is.null(imp_mids$loggedEvents)
  }
  # update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

# Proportion of times multicollinearity is found
colMeans(logged)
colMeans(avg_corrs)
colMeans(max_corrs)