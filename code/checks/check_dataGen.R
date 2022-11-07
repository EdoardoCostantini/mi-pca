### Title:    Data Generation Checks
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-11-15

## Make sure we have a clean environment:
rm(list = ls())

## Initialize the environment:
source("./init.R")

# Simpler version of conditions
parms$largeP <- FALSE # low dimensional
conds <- expand.grid(K = K,
                     D = D,
                     interval = interval,
                     pj = pj,
                     stringsAsFactors = FALSE)

# No latent structure ---------------------------------------------------------

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

# Latent structure ------------------------------------------------------------

# Store correlations
store_cors <- matrix(nrow = nrow(conds), ncol = 5,
                     dimnames = list(NULL, c("ta_ta", "ta_mp", "ta_ax", "mp_mp", "ax_ax")))

# Loop over conditions
for(i in 1:nrow(conds)){

  # Store temporary condition
  cond <- conds[i, ]

  # Generate data
  dat <- genDataLatent(parms = parms, cond = cond)

  # Compute correlation for the generated multivariate data
  cormat <- round(cor(dat$x), 3)

  # Store the bivariate correlations between items
  ta_ta <- cormat[parms$vmap$ta, parms$vmap$ta][upper.tri(diag(length(parms$vmap$ta)))]
  ta_mp <- cormat[parms$vmap$ta, parms$vmap$mp]
  ta_ax <- cormat[parms$vmap$ta, parms$vmap$ax]
  mp_mp <- cormat[parms$vmap$mp, parms$vmap$mp][upper.tri(diag(length(parms$vmap$mp)))]
  ax_ax <- cormat[parms$vmap$ax, parms$vmap$ax][upper.tri(diag(length(parms$vmap$ax)))]

  # Compute average correlation
  store_cors[i, ] <- sapply(
    list(
      ta_ta = ta_ta,
      ta_mp = ta_mp,
      ta_ax = ta_ax,
      mp_mp = mp_mp,
      ax_ax = ax_ax
    ),
    mean
  )
}

# Average correlations by condition (~ .7)
cbind(conds, round(store_cors, 1))

# Average correlation in auxiliary blocks
round(cormat, 1)[(parms$P - 13):parms$P, (parms$P - 13):parms$P]
# Even auxiliary variables in the same block are correlated .7,
# but across blocks, they are not correlated