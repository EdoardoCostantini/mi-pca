### Title:    Check MAR based on predictors
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-07-21

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Initialize the environment:
  source("./init.R")

  ## Select a condition
  cond <- conds[446, ]

# Correlation btw mar latent and missing items stays the same -------------

  ## Gen fully observed data
  dat_list <- genData(parms = parms, cond = cond)

  ## Impose Missingness Univariate based
  dat_miss_uni <- imposeNA(dat_list, parms = parms)
  head(dat_miss_uni)

  ## Impose Missingness Multivariate based
  dat_miss_mul <- ampute_step(dat_list, parms = parms)
  head(dat_miss_mul)

  ## Compare proportions
  # Per variable missing cases
  colMeans(is.na(dat_miss_uni))
  colMeans(is.na(dat_miss_mul))

  # Total number of missing values
  sum(is.na(dat_miss_uni))
  sum(is.na(dat_miss_mul))

  # Total number of rows with missing values
  parms$N - nrow(na.omit(dat_miss_uni))
  parms$N - nrow(na.omit(dat_miss_mul))

  # Coverages
  md.pairs(dat_miss_uni[, 1:4])$rr
  md.pairs(dat_miss_mul[, 1:4])$rr

  ## Does the missing data on variable z1 to z4 depend z5 to z8?
  dat_miss <- dat_miss_uni
  dat_miss <- dat_miss_mul
  head(dat_miss)
  var_miss <- "z1"
  MAR_pred <- c("z2", "z5", "z7", "z10", "z11")[5]

  densityplot(~ dat_miss[, MAR_pred],
              groups =  factor(is.na(dat_miss[, var_miss]),
                               labels = c(paste0(var_miss, " observed"),
                                          paste0(var_miss, " missing"))),
              par.settings = list(superpose.line = list(lty = 1:2)),
              main = paste0("Does missingness on ", var_miss,
                            " depend on ", MAR_pred, "?"),
              xlab = MAR_pred,
              auto.key = TRUE)

  # Compare Bias
  mids_uni <- mice(dat_miss_uni[, 1:10],
                     maxit = 50,
                     printFlag = FALSE,
                   seed = 123)

  mids_mul <- mice(dat_miss_mul[, 1:10],
                     maxit = 50,
                     printFlag = FALSE,
                   seed = 123)

  colMeans(dat_miss, na.rm = T)
  cor(na.omit(dat_miss))[1:10, 1:5]
  cor(dat_list$dat_ob)[1:10, 1:5]

  # FMI for some model
  fit1 <- with(mids_uni, lm(z1 ~ 1))
  est1 <- pool(fit1)
  est1$pooled[, c("estimate", "riv", "fmi")]

  fit2 <- with(mids_mul, lm(z1 ~ 1))
  est2 <- pool(fit2)
  est2$pooled[, c("estimate", "riv", "fmi")]

  lm(z1 ~ 1, dat_in$dat_ob)

# Effect of including excluding MAR predictors in mice --------------------

  set.seed(1234)

  store_yeMAR <- NULL
  store_noMAR <- NULL

  for (i in 1:1e2){
    print(i)
    ## Gen fully observed data
    dat_list <- genData(parms = parms, cond = cond)

    ## Impose Missingness Univariate based
    dat_miss_uni <- imposeNA(dat_list, parms = parms)
    head(dat_miss_uni)

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
    store_yeMAR <- rbind(store_yeMAR, yeMAR)
    store_noMAR <- rbind(store_noMAR, noMAR)
  }

  colMeans(store_yeMAR)
  colMeans(store_noMAR)

# Effect of imposing miss with ampute vs univariate strategy --------------

  rm(list = ls())
  source("./init.R")
  source("../helper/fun_ampute.R")
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
