### Title:    Check MAR based on predictors
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-07-23

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Initialize the environment:
  source("./init.R")
  source("./helper/fun_amputeMultivariate.R")

  ## Select a condition
  cond <- conds[46, ]

# MAR Predictors ----------------------------------------------------------

  ## Gen fully observed data
  dat_list <- genData(parms = parms, cond = cond)

  ## Impose Missingness Univariate based
  target_miss <- amputePerVar(targets = dat_list$dat_ob[, parms$varMap_items$ta],
                              preds = dat_list$dat_ob[, parms$varMap_items$mp],
                              pm = parms$pm,
                              type = "high")
  dat_miss <- cbind(target_miss, dat_list$dat_ob[, -parms$varMap_items$ta])

  var_miss <- "z1"
  MAR_pred <- colnames(dat_miss)[10]

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

  for (i in 1:1e3){
    print(i)
    ## Gen fully observed data
    dat_list <- genData(parms = parms, cond = cond)

    ## Impose Missingness Univariate based
    target_miss <- amputePerVar(targets = dat_list$dat_ob[, parms$varMap_items$ta],
                                preds = dat_list$dat_ob[, parms$varMap_items$mp],
                                pm = parms$pm,
                                type = "high")
    dat_miss_uni <- cbind(target_miss, dat_list$dat_ob[, -parms$varMap_items$ta])

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

  saveRDS(list(rbind(yeMAR = colMeans(store_yeMAR),
                     noMAR = colMeans(store_noMAR)),
               mean(store_CC)),
          file = "../output/checks/mar_preds.rds")

# latent variable vs observed items as MAR predictors in mice -------------

  set.seed(1234)

  store_lv <- NULL
  store_it <- NULL
  store_CC <- NULL

  for (i in 1:1e3){
    print(i)
    ## Gen fully observed data
    dat_list <- genData(parms = parms, cond = cond)

    ## Impose Missingness based on latent variable
    target_miss_lv <- amputePerVar(targets = dat_list$dat_ob[, parms$varMap_items$ta],
                                   preds = dat_list$dat_lv[, parms$varMap$mp,
                                                             drop = FALSE],
                                   pm = parms$pm,
                                   type = "high")
    dat_miss_lv <- cbind(target_miss_lv, dat_list$dat_ob[, -parms$varMap_items$ta])

    ## Impose Missingness based on observed items
    target_miss_it <- amputePerVar(targets = dat_list$dat_ob[, parms$varMap_items$ta],
                                   preds = dat_list$dat_ob[, parms$varMap_items$mp,
                                                             drop = FALSE],
                                   pm = parms$pm,
                                   type = "high")
    dat_miss_it <- cbind(target_miss_it, dat_list$dat_ob[, -parms$varMap_items$ta])

    ## Impute with and witout MAR predictors
    mids_lv <- mice(dat_miss_lv[, 1:8],
                    maxit = 25,
                    printFlag = FALSE)
    mids_it <- mice(dat_miss_it[, 1:8],
                    maxit = 25,
                    printFlag = FALSE)

    ## Extract results (item 1 mean)
    pool_lv <- pool(with(mids_lv, lm(z1 ~ 1)))$pooled[, c("estimate", "riv", "fmi")]
    pool_it <- pool(with(mids_it, lm(z1 ~ 1)))$pooled[, c("estimate", "riv", "fmi")]

    ## Store
    store_CC <- rbind(store_CC,
                      c(lv = mean(dat_miss_lv$z1, na.rm = TRUE),
                        it = mean(dat_miss_it$z1, na.rm = TRUE)))
    store_lv <- rbind(store_lv, pool_lv)
    store_it <- rbind(store_it, pool_it)
  }

  colMeans(store_lv)
  colMeans(store_it)
  colMeans(store_CC)

saveRDS(list(rbind(lv = colMeans(store_lv),
                   it = colMeans(store_it)),
             mean(store_CC)),
        file = "../output/checks/mar_preds_lv_vs_it.rds")

# Effect of imposing miss with ampute vs univariate strategy --------------

  # Simulation: monitor differences in bias, riv, and FMI
  cond <- conds[46, ]

  set.seed(1234)

  store_uni <- NULL
  store_mul <- NULL

  for (i in 1:2){
    print(i)
    ## Gen fully observed data
    dat_list <- genData(parms = parms, cond = cond)

    ## Impose Missingness w/ univariate strategy
    target_miss_uni <- amputePerVar(targets = dat_list$dat_ob[, parms$varMap_items$ta],
                                   preds = dat_list$dat_ob[, parms$varMap_items$mp,
                                                             drop = FALSE],
                                   pm = parms$pm,
                                   type = "high")
    dat_miss_uni <- cbind(target_miss_uni, dat_list$dat_ob[, -parms$varMap_items$ta])

    ## Impose Missingness w/ multivariate strategy
    dat_miss_mul <- amputeMultivariate(miss_target = dat_list$dat_ob[, parms$varMap_items$ta],
                                       miss_preds = dat_list$dat_lv[, 2, drop = FALSE],
                                       parms = parms)
    dat_miss_mul <- cbind(dat_miss_mul[, parms$varMap_items$ta],
                          dat_list$dat_ob[, -parms$varMap_items$ta])

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

  saveRDS(rbind(uni = colMeans(store_uni),
                mul = colMeans(store_mul)),
          file = "../output/checks/mar_uni_vs_mul.rds")

  # Monitor difference in coverage, sample size, pm
  dat_list <- genData(parms = parms, cond = cond)

  target_miss_uni <- amputePerVar(targets = dat_list$dat_ob[, parms$varMap_items$ta],
                                  preds = dat_list$dat_ob[, parms$varMap_items$mp,
                                                            drop = FALSE],
                                  pm = parms$pm,
                                  type = "high")
  dat_miss_uni <- cbind(target_miss_uni, dat_list$dat_ob[, -parms$varMap_items$ta])
  dat_miss_mul <- amputeMultivariate(miss_target = dat_list$dat_ob[, parms$varMap_items$ta],
                             miss_preds = dat_list$dat_lv[, 2, drop = FALSE],
                             parms = parms) # multivariate miss

  colMeans(is.na(dat_miss_uni)) # Per variable missing cases
  colMeans(is.na(dat_miss_mul))

  sum(is.na(dat_miss_uni)) # Total number of missing values
  sum(is.na(dat_miss_mul))

  parms$N - nrow(na.omit(dat_miss_uni)) # Total number of rows with missing values
  parms$N - nrow(na.omit(dat_miss_mul))

  md.pairs(dat_miss_uni[, 1:4])$rr # Coverages
  md.pairs(dat_miss_mul[, 1:4])$rr

# MAR strength ------------------------------------------------------------
# Using R2 like statistics for the logistics missingness models

  # Data Generated as in study
  dat_list <- genData(parms = parms, cond = cond)

  ## Impose Missingness w/ univariate strategy
  X_items <- dat_list$dat_ob[, parms$varMap_items$mp, drop = FALSE]
  X_lv <- dat_list$dat_lv[, parms$varMap$mp, drop = FALSE]

  set.seed(123)
  nR <- simMissingness(pm    = .5,
                       data  = X_items,
                       type  = "high",
                       beta = rep(1, ncol(X_items)))
  yX <- cbind(nR, X_items)

  glm_fit <- glm(nR ~ ., data = yX, family = "binomial")
  nullmod <- glm(nR ~ 1, family = "binomial")
  prob <- predict(glm_fit, type = c("response"))
  roc_out <- roc(nR ~ prob, data = yX, plot = FALSE, print.auc = TRUE)
  pR2 <- 1-logLik(glm_fit)/logLik(nullmod) # McFadden's Psuedo R2
  round(c(auc = as.numeric(roc_out$auc),
          pR2 = as.numeric(pR2)), 3)

  # Data generated by me, miss impose with simMissingness()
  N <- 1e3
  p <- 3
  x <- MASS::mvrnorm(N, rep(0, p), diag(p))
  b <- rep(5, p)
  a <- 0

  std <- c(no = 1, yes = 2)[2] # do you want to see differences?
  if(std == 1){
    lin_pred <- a + x %*% b # effect
  } else {
    lin_pred <- scale(a + x %*% b)
  }

  pi_x <- exp(lin_pred) / (1 + exp(lin_pred))
  y <- rbinom(N, 1, pi_x)
  glm_fit <- glm(y ~ x, family = "binomial")
  nullmod <- glm(y ~ 1, family = "binomial")
  prob <- predict(glm_fit, type = c("response"))
  roc_out <- roc(y ~ prob, plot = FALSE, print.auc = TRUE)
  pR2 <- 1-logLik(glm_fit)/logLik(nullmod) # McFadden's Psuedo R2
  round(c(auc = as.numeric(roc_out$auc),
          pR2 = as.numeric(pR2)), 3)
  # plot(roc_out, print.auc = TRUE)
  par(mfrow = c(1, 2))
  plot(sort(lin_pred), sort(pi_x), type = "l",
       main = c("Raw", "Standardized")[std],
       ylab = "Probability", xlab = "Linear combination of predictors")

  # Data generated by me, miss impose with simMissingness()
  patts <- weights <- c(0, 0, 1)

  x_ampute <- ampute(
    data = x,
    patterns = patts,
    weights = weights,
    type = c("RIGHT")
  )

  glm_fit <- glm(is.na(V1) ~ V3, x_ampute$amp, family = "binomial")
  nullmod <- glm(is.na(V1) ~ 1, x_ampute$amp, family = "binomial")
  prob <- predict(glm_fit, type = c("response"))
  roc_out <- roc(is.na(x_ampute$amp$V1) ~ prob, plot = FALSE, print.auc = TRUE)
  pR2 <- 1-logLik(glm_fit)/logLik(nullmod) # McFadden's Psuedo R2
  round(c(auc = as.numeric(roc_out$auc),
          pR2 = as.numeric(pR2)), 3)

  # Boys Data, miss impose with ampute()
  compl_boys <- cc(boys)[1:3]

  # Perform amputation with default settings
  mads_boys <- ampute(data = compl_boys)
  mads_boys$amp

  # Change default matrices as desired
  my_patterns <- mads_boys$patterns
  my_patterns[, 3] <- 1
  my_patterns[2, 1] <- 0
  my_patterns[3, 2] <- 0

  my_weights <- mads_boys$weights
  my_weights[, 1] <- 0
  my_weights[, 2] <- 0
  my_weights[, 3] <- 1

  # Rerun amputation
  my_mads_boys <- ampute(
    data = compl_boys,
    patterns = my_patterns,
    weights = my_weights,
    type = c("RIGHT")
  )
  miss_dat <- my_mads_boys$amp

  y <- is.na(miss_dat$age)
  x <- miss_dat$wgt

  glm_fit <- glm(is.na(V1) ~ V3, x_ampute$amp, family = "binomial")
  nullmod <- glm(is.na(V1) ~ 1, x_ampute$amp, family = "binomial")
  prob <- predict(glm_fit, type = c("response"))
  roc_out <- roc(is.na(x_ampute$amp$V1) ~ prob, plot = FALSE, print.auc = TRUE)
  pR2 <- 1-logLik(glm_fit)/logLik(nullmod) # McFadden's Psuedo R2
  round(c(auc = as.numeric(roc_out$auc),
          pR2 = as.numeric(pR2)), 3)

