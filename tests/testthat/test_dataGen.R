### Title:    Test File for the data generation context
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-25
### Modified: 2021-06-23

  context('- test function genData()')

# Correct return of objects -----------------------------------------------

  expect_list <- rep(NA, nrow(conds))
  expect_data_matrix <- rep(NA, nrow(conds))

  for (i in 1:nrow(conds)){
    dat_list <- genData(parms = parms, cond = conds[i, ],
                        fl_ta = parms$fl, fl_ax = parms$fl)
    expect_list[i] <- is.list(dat_list) & !is.atomic(dat_list)
    expect_data_matrix[i] <- all(sapply(dat_list, is.matrix))
  }

  # Tests
  test_that("Output is a list", {
    expect_equal(all(expect_list), TRUE)
  })
  test_that("Datasets generated are matrices", {
    expect_equal(all(expect_data_matrix), TRUE)
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
