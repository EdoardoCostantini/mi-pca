# Title:    Check fmi for parameters as we include exclud predictors
# Project:  MI-PCA study
# Author:   Edoardo Costantini
# Created:  2021-07-20
# Modified: 2022-10-15

# Make sure we have a clean environment:
rm(list = ls())

# Initialize the environment:
source("./init.R")

# Select a condition
parms$largeP <- FALSE
conds
cond <- conds[296, ]

# Correlation btw mar latent and missing items stays the same -------------

# Gen fully observed data
dat <- genData(parms = parms, cond = cond)

# Discretise data
dat_ordi <- disData(x = dat$x, K = cond$K, parms = parms)

# Impose Missingness
preds <- dat$x[, parms$vmap$mp, drop = FALSE]
targets <- dat_ordi[, parms$vmap$ta, drop = FALSE]
target_miss <- amputePerVar(
  targets = targets,
  preds = preds,
  pm = parms$pm,
  type = "high"
)
dat_miss <- cbind(target_miss, dat_ordi[, -parms$vmap$ta])

# Set up imputation
dry_run <- mice(dat_miss, maxit = 0)
pred_mat_init <- dry_run$predictorMatrix

# Create storing objects
store1 <- NULL
store2 <- NULL

# Repeat 100 times
for(i in 1:100){
  print(i)
  # Omit MAR predictors
  pred_mat_noMAR <- pred_mat_init
  pred_mat_noMAR[, c(1:8, 20:ncol(dat_miss))] <- 0
  mids_noMAR <- mice(dat_miss,
                     maxit = 50,
                     printFlag = FALSE,
                     predictorMatrix = pred_mat_noMAR)

  # Include MAR predictors
  pred_mat_yeMAR <- pred_mat_init
  pred_mat_yeMAR[, -(1:8)] <- 0

  mids_yeMAR <- mice(dat_miss,
                     maxit = 50,
                     printFlag = FALSE,
                     predictorMatrix = pred_mat_yeMAR)
  # plot(mids_yeMAR, c("z1","z2", "z3"))

  colMeans(dat_miss, na.rm = T)
  cor(na.omit(dat_miss))[1:10, 1:5]
  cor(dat$x)[1:10, 1:5]

  # FMI for some model
  fit1 <- with(mids_noMAR, lm(z1 ~ 1))
  est1 <- pool(fit1)
  store1 <- rbind(store1, est1$pooled[, c("estimate", "riv", "fmi")])

  fit2 <- with(mids_yeMAR, lm(z1 ~ 1))
  est2 <- pool(fit2)
  store2 <- rbind(store2, est2$pooled[, c("estimate", "riv", "fmi")])
}

rbind(noMAR = apply(store1, 2, mean),
      yeMAR = apply(store2, 2, mean))

rbind(noMAR = apply(store1, 2, sd),
      yeMAR = apply(store2, 2, sd))

  # Descriptive Stats
  library(miceadds)

  data(nhanes, package="mice")
  set.seed(9090)

  # nhanes data in one chain
  imp <- miceadds::mice.1chain( nhanes, burnin=5, iter=40, Nimp=4 )
  # apply linear regression
  res <- with( imp, expr=stats::lm( hyp ~ age + bmi  ) )
  summary(res)
  # pool results
  summary( mice::pool(res))

  # calculate some descriptive statistics
  res2 <- with( imp, expr=c("M1"=mean(hyp), "SD_age"=stats::sd(age) ) )
  # pool estimates
  withPool_MI(res2)
