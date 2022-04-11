# Project:   mipca_compare
# Objective: Compute the pseudo R2 for logistic missingness model
# Author:    Edoardo Costantini
# Created:   2022-04-06
# Modified:  2022-04-06

# Initialize environment -------------------------------------------------------

  rm(list = ls())
  source("./init.R")
  library(pROC) # for Auc

# Set up storing objects
  reps <- 1e3
  shelf_pR2 <- shelf_AUC <-  matrix(NA, nrow = reps,
                                    ncol = length(parms$vmap$ta),
                                    dimnames = list(NULL, paste0("z", parms$vmap$ta)))

# All conditions should have the same MAR mechanism
  cond <- conds[1, ]

# Loop over data generation and computation of psuedoR2 and AUC ----------------

  set.seed(20220406)

  for (r in 1:reps){
    # r <- 1
    # Generate data
    dat <- genDataLatent(parms = parms, cond = cond)

    # Discretize data
    dat_ordi <- disData(x = dat$x, K = cond$K, parms = parms)

    # Impose Missingness
    preds   <- dat$x[, parms$vmap$mp, drop = FALSE]
    targets <- dat_ordi[, parms$vmap$ta, drop = FALSE]
    target_miss <- amputePerVar(targets = targets,
                                preds = preds,
                                pm = parms$pm,
                                type = "high")
    dat_miss <- cbind(target_miss, dat_ordi[, -parms$vmap$ta])

    # Set up logistic regressions
    X <- preds
    y <- data.frame(is.na(target_miss))
    yX <- data.frame(y, X)

    # Compute pseudo R2 for every variable under imputaiton
    shelf_pR2[r, ] <- sapply(1:ncol(y), function (j){
      form <- as.formula(paste0(as.symbol(names(y[, j, drop = FALSE])), " ~ ."))
      glmout <- glm(form, family = "binomial", data = cbind(y[, j, drop = FALSE], X))
      (1 - glmout$deviance / glmout$null.deviance) * 100 # works for glm
    })

    # Create a training test data for the AUC computation
    sample <- sample(c(TRUE, FALSE), nrow(yX), replace = TRUE, prob = c(0.7, 0.3))
    train_X <- X[sample, ]
    train_y <- y[sample, ]
    test_X  <- X[!sample, ]
    test_y  <- y[!sample, ]

    # Fir the logistic regression predicting missing values based on the MAR predictors
    mod_list <- lapply(1:ncol(y), function (j){
      temp_data <- cbind(train_y[, j, drop = FALSE], train_X)
      form <- as.formula(paste0(as.symbol(names(train_y[, j, drop = FALSE])), " ~ ."))
      glm(form, family = "binomial", data = temp_data)
    })

    # AUC for every model
    shelf_AUC[r, ] <- sapply(1:length(mod_list), function (i){
      # Obtain prediction on test data
      y_hat <- predict(mod_list[[i]], test_X, type = "response")

      # Try to run it (fails if the train-test split leads to all FALSE all TRUE on the DV
      tryCatch({
        suppressMessages(auc(test_y[, i], y_hat))
      },
        error = function(e){
          return(NA)
        }
      )
    })

  }

# Pseudo R2
  meltData <- reshape2::melt(shelf_pR2)
  boxplot(data = meltData, value ~ Var2)
  colMeans(shelf_pR2)

# AUC
  meltData <- reshape2::melt(shelf_AUC)
  boxplot(data = meltData, value ~ Var2)
  colMeans(shelf_AUC, na.rm = TRUE)
