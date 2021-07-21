### Title:    Check MAR based on predictors
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-07-21

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Initialize the environment:
  source("./init.R")
  source("./helper/fun_ampute.R")

  ## Select a condition
  cond <- conds[446, ]

# MAR Predictors ----------------------------------------------------------

  ## Gen fully observed data
  dat_list <- genData(parms = parms, cond = cond)

  ## Impose Missingness Univariate based
  dat_miss <- imposeNA(dat_list, parms = parms)
  head(dat_miss)

  var_miss <- "z1"
  MAR_pred <- c("z2", "z5", "z7", "z10", "z11")[2]

  densityplot(~ dat_miss[, MAR_pred],
              groups =  factor(is.na(dat_miss[, var_miss]),
                               labels = c(paste0(var_miss, " observed"),
                                          paste0(var_miss, " missing"))),
              par.settings = list(superpose.line = list(lty = 1:2)),
              main = paste0("Does missingness on ", var_miss,
                            " depend on ", MAR_pred, "?"),
              xlab = MAR_pred,
              auto.key = TRUE)

# Effect of including excluding MAR predictors in mice --------------------

  set.seed(1234)

  store_yeMAR <- NULL
  store_noMAR <- NULL
  store_CC <- NULL

  for (i in 1:1e2){
    print(i)
    ## Gen fully observed data
    dat_list <- genData(parms = parms, cond = cond)

    ## Impose Missingness Univariate based
    dat_miss_uni <- imposeNA(dat_list, parms = parms)

    ## Impute with and witout MAR predictors
    mids_uni <- mice(dat_miss_uni[, 1:8],
                     maxit = 25,
                     printFlag = FALSE)
    mids_uni_noMAR <- mice(dat_miss_uni[, 1:4],
                           maxit = 25,
                           printFlag = FALSE)

    ## Extract results (item 1 mean)
    yeMAR <- pool(with(mids_uni, lm(z1 ~ 1)))$pooled[, c("estimate", "riv", "fmi")]
    noMAR <- pool(with(mids_uni_noMAR, lm(z1 ~ 1)))$pooled[, c("estimate", "riv", "fmi")]
    store_CC[i] <- mean(dat_miss_uni$z1, na.rm = TRUE)
    store_yeMAR <- rbind(store_yeMAR, yeMAR)
    store_noMAR <- rbind(store_noMAR, noMAR)
  }

  colMeans(store_yeMAR)
  colMeans(store_noMAR)
  mean(store_CC)

# Effect of imposing miss with ampute vs univariate strategy --------------

  # Simulation: monitor differences in bias, riv, and FMI
  cond <- conds[446, ]

  set.seed(1234)

  store_uni <- NULL
  store_mul <- NULL

  for (i in 1:1e2){
    print(i)
    ## Gen fully observed data
    dat_list <- genData(parms = parms, cond = cond)

    ## Impose Missingness
    dat_miss_uni <- imposeNA(dat_list, parms = parms)
    dat_miss_mul <- ampute_step(dat_list, parms = parms)

    ## Impute with and witout MAR predictors
    mids_uni <- mice(dat_miss_uni[, 1:8],
                     maxit = 25,
                     printFlag = FALSE)
    mids_mul <- mice(dat_miss_mul[, 1:8],
                           maxit = 25,
                           printFlag = FALSE)
    uni <- pool(with(mids_uni, lm(z1 ~ 1)))$pooled[, c("estimate", "riv", "fmi")]
    mul <- pool(with(mids_mul, lm(z1 ~ 1)))$pooled[, c("estimate", "riv", "fmi")]
    store_uni <- rbind(store_uni, uni)
    store_mul <- rbind(store_mul, mul)
  }

  colMeans(store_uni)
  colMeans(store_mul)

  # Monitor difference in coverage, sample size, pm
  dat_list <- genData(parms = parms, cond = cond)

  dat_miss_uni <- imposeNA(dat_list, parms = parms) # univariate miss
  dat_miss_mul <- ampute_step(dat_list, parms = parms) # multivariate miss

  colMeans(is.na(dat_miss_uni)) # Per variable missing cases
  colMeans(is.na(dat_miss_mul))

  sum(is.na(dat_miss_uni)) # Total number of missing values
  sum(is.na(dat_miss_mul))

  parms$N - nrow(na.omit(dat_miss_uni)) # Total number of rows with missing values
  parms$N - nrow(na.omit(dat_miss_mul))

  md.pairs(dat_miss_uni[, 1:4])$rr # Coverages
  md.pairs(dat_miss_mul[, 1:4])$rr