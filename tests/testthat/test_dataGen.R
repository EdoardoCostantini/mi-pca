### Title:    Test File for the data generation context
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-25
### Modified: 2021-07-08

  context('- test function genData()')

# Correct proportion of discrete vars -------------------------------------
  set.seed(2134)

  # Expectation Storing Objects
  expect_discrete <- rep(NA, nrow(conds))
  expect_list <- rep(NA, nrow(conds))
  expect_df_observed <- rep(NA, nrow(conds))
  expect_matrix_rest <- rep(NA, nrow(conds))

  for (i in 1:nrow(conds)){
    dat_list <- genData(parms = parms, cond = conds[i, ])

    # Discreteness
    nauxiliaries <- parms$P-(length(parms$varMap$ta) * parms$J)
    ratio <- sum(sapply(dat_list$dat_ob, is.factor))/nauxiliaries
    expect_discrete[i] <- abs(conds[i, "D"] - ratio) < .1

    # Object Types
    expect_list[i] <- is.list(dat_list) & !is.atomic(dat_list)
    expect_df_observed[i] <- is.data.frame(dat_list$dat_ob)
    expect_matrix_rest[i] <- all(sapply(dat_list[-1], is.matrix))
  }

  # Tests
  test_that("Correct Proportion of discrete variables in all conditions", {
    expect_equal(all(expect_discrete), TRUE)
  })
  test_that("Output is a list", {
    expect_equal(all(expect_list), TRUE)
  })
  test_that("Data set observed items is data.frame", {
    expect_equal(all(expect_matrix_rest), TRUE)
  })
  test_that("Other objects are matrices", {
    expect_equal(all(expect_matrix_rest), TRUE)
  })


# Factor Loadings Generated and recovered ---------------------------------
# Checks the CFA estimates return the true values
  
  # Example Condition
  cond <-  conds[1, ]

  dat <- genData(parms = parms, cond = cond,
                 fl_ta = parms$fl, fl_ax = parms$fl)
  
  # Define CFA model
  ids_items <- split(x = colnames(dat$dat_ob), 
                     f = rep(1:(ncol(dat$dat_ob)/parms$J), each = parms$J))
  ids_lv <- colnames(dat$dat_lv)
  names(ids_items) <- ids_lv
  
  lv_models <- sapply(1:length(ids_items), function(i){
    paste0(ids_lv[i], 
           " =~ ",
           paste0(ids_items[[i]], collapse = " + ")
    )
    
  })
  CFA_model <- paste(lv_models, collapse = "\n")
  
  # Fit CFA
  fit <- cfa(CFA_model, data = dat$dat_ob, std.lv = TRUE)
  
  # Compare
  CFA_par <- parameterEstimates(fit, 
                                se = FALSE, zstat = FALSE, 
                                pvalue = FALSE, ci = FALSE,
                                standardized = TRUE)
  
  # Prepare test
  par_index <- apply(CFA_par[, 1:3], 1, paste0, collapse = "")
  
  idx_load <- 1:(parms$L*parms$J)
  idx_error <- (parms$L*parms$J+1):(2*parms$L*parms$J)
  idx_lvcov <- (2*parms$L*parms$J+1+parms$L):nrow(CFA_par)
  
  # Factor loadings
  est_loadings <- mean(CFA_par[idx_load, "est"])
  tru_loadings <- mean(dat$Lambda[dat$Lambda != 0])
  
  # Latent Var covar
  est_cov <- mean(CFA_par[idx_lvcov, "est"])
  tru_cov <- mean(c(parms$lv_cov_ta, parms$lv_cov_mp, parms$lv_cov_ax))
  
  # Error variance
  est_err <- mean(CFA_par[idx_error, "est"])
  tru_err <- mean(diag(dat$Theta))
  
  # Test
  test_that("Correct Recovery of CFA parameters in condition 1", {
    expect_equal(est_loadings, tru_loadings, tolerance = 1e-1)
    expect_equal(est_err, tru_err, tolerance = 1e-1)
    expect_equal(est_cov, tru_cov, tolerance = 1e-1)
  })
