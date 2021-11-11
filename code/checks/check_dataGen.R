### Title:    Data Generation Checks
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-09-03

## Make sure we have a clean environment:
rm(list = ls())

## Initialize the environment:
source("./init.R")

# Simpler version of conditions
conds <- expand.grid(K = K,
                     D = D,
                     interval = interval,
                     pj = pj,
                     stringsAsFactors = FALSE)

# Correlations as expected ------------------------------------------------
store_cors <- matrix(nrow = nrow(conds), ncol = 3)

for (i in 1:nrow(conds)){
  print(i)
  dat_list <- genData(parms = parms, cond = conds[i, ])
  cormat <- round(cor(dat_list$x), 3)
  ta_cor <- cormat[parms$vmap$ta, parms$vmap$ta][upper.tri(diag(length(parms$vmap$ta)))]
  mp_cor <- cormat[parms$vmap$mp, parms$vmap$mp][upper.tri(diag(length(parms$vmap$mp)))]
  ax_cor <- cormat[parms$vmap$ax, parms$vmap$ax][upper.tri(diag(length(parms$vmap$ax)))]
  store_cors[i, ] <- sapply(list(ta_cor, mp_cor, ax_cor), mean)
}

round(store_cors, 1)

# Latent Data Gen works for all conditions of pj --------------------------

# Simpler version of conditions
conds <- expand.grid(K = K,
                     D = D,
                     pj = pj,
                     stringsAsFactors = FALSE)

# Store correlations
store_cors <- matrix(nrow = nrow(conds), ncol = 3)

for(i in 1:nrow(conds)){
  cond <- conds[i, ]
  dat <- genDataLatent(parms = parms, cond = cond)
  cormat <- round(cor(dat$x), 3)
  ta_cor <- cormat[parms$vmap$ta, parms$vmap$ta][upper.tri(diag(length(parms$vmap$ta)))]
  mp_cor <- cormat[parms$vmap$mp, parms$vmap$mp][upper.tri(diag(length(parms$vmap$mp)))]
  ax_cor <- cormat[parms$vmap$ax, parms$vmap$ax][upper.tri(diag(length(parms$vmap$ax)))]
  store_cors[i, ] <- sapply(list(ta_cor, mp_cor, ax_cor), mean)
  dat_ordi <- disData(x = dat$x, K = cond$K, parms = parms) # no errors is enough
}
round(store_cors, 1)
colMeans(round(store_cors, 1))


