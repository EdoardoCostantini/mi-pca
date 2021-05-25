### Title:    Test File for the data generation context
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-25

  context('Data Generation')

# Correct discretization per condition ------------------------------------
  
  store <- conds[0, c("n_ord", "n_bin")]
  
  for(i in 1:nrow(conds)){
    dat <- genData(parms, conds[i, ])
    dat_dis <- disData(dat$dat_ob, parms, conds[i, ])
    
    # Count variables
    count <- apply(dat_dis[, -(1:(length(parms$varMap$ta)*parms$J))], 
                   2, 
                   function(x){
      length(unique(x))
    })
    store[i, ] <- c(ord = mean(count %in% parms$K),
                    bin = mean(count %in% 2))
  }
  
  prop_achived <- store
  prop_target  <- conds[, c("n_ord", "n_bin")]
  
  # Test
  test_that("Correct Proportion of Discretized Variables", {
    expect_equal(prop_achived, prop_target)
  })

# Factor Loadings Generated and recovered ---------------------------------
# Checks the CFA estimates return the true values
  
  # Example Condition
  cond <-  conds[1, ]
  
  dat <- genData(parms, cond)
  
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
