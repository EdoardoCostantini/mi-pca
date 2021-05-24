### Title:    Checking Functions do what expected
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-24

# Factor Loadings Generated and recovered ---------------------------------
# Checks the CFA estimates return the true values

  rm(list=ls())
  source("./init.R")
  
  # Example Condition
  cond <-  conds[1, ]
  
  set.seed(20210127)
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
  
  # Evaluate Fit
  fit.out <- summary(fit, fit.measures = TRUE, standardized = TRUE)
  round(fit.out$FIT[c("cfi", "tli", "rmsea", "rmsea.pvalue")], 3)

  # Compare
  CFA_par <- parameterEstimates(fit, 
                                se = FALSE, zstat = FALSE, 
                                pvalue = FALSE, ci = FALSE,
                                standardized = TRUE)
  
  # Measured
  par_index <- apply(CFA_par[, 1:3], 1, paste0, collapse = "")
  idx_load <- grep("=~", par_index)
  idx_error <- grep("~~z.*", par_index)
  idx_lvcov <- grep("~~lv.*", par_index)
  
  # Factor loadings
  CFA_par[idx_load, "est"]
  dat$Lambda[dat$Lambda != 0]
  
  # Latent Var covar
  CFA_par[idx_lvcov, "est"]
  dat$Phi
  
  # Error variance
  CFA_par[idx_error, "est"]
  diag(dat$Theta)
  

# Correct discretization per condition ------------------------------------

  rm(list=ls())
  source("./init.R")
  
  store <- matrix(NA, nrow = nrow(conds), ncol = 3)
  
  for(i in 1:nrow(conds)){
    dat <- genData(parms, conds[i, ])
    dat_dis <- disData(dat$dat_ob, parms, conds[i, ])
    
    # Count variables
    count <- apply(dat_dis[, -(1:(length(parms$varMap$ta)*parms$J))], 2, function(x){
      length(unique(x))
    })
    store[i, ] <- c(num = mean(count %in% parms$N),
                    ord = mean(count %in% parms$K),
                    bin = mean(count %in% 2))
  }
  store
  store[, 2:3] - conds[, c("n_ord", "n_bin")]
  
  