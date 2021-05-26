conds[13:16, ]
conds_list <- 13:16
store <- matrix(NA, nrow = length(conds_list), ncol = 3)
i <- 1
for(i in seq_along(conds_list)){
  ## Generate Data
  dat_list <- genData(parms, conds[conds_list[i], ])
  dat_disc <- disData(dat_list$dat_ob, parms, conds[conds_list[i], ])
  
  ## Define CFA model
  ids_items <- split(x = colnames(dat_list$dat_ob), 
                     f = rep(1:(ncol(dat_list$dat_ob)/parms$J), each = parms$J))
  ids_lv <- colnames(dat_list$dat_lv)
  names(ids_items) <- ids_lv
  
  lv_models <- sapply(1:length(ids_items), function(it){
    paste0(ids_lv[it], 
           " =~ ",
           paste0(ids_items[[it]], collapse = " + ")
    )
    
  })
  CFA_model <- paste(lv_models, collapse = "\n")
  
  ## Fit CFA
  fit_c <- cfa(CFA_model, data = dat_list$dat_ob, std.lv = TRUE)
  fit_d <- cfa(CFA_model, data = dat_disc, std.lv = TRUE)
  
  # Compare
  CFA_par_c <- parameterEstimates(fit_c, 
                                  se = FALSE, zstat = FALSE, 
                                  pvalue = FALSE, ci = FALSE,
                                  standardized = TRUE)
  CFA_par_d <- parameterEstimates(fit_d, 
                                  se = FALSE, zstat = FALSE, 
                                  pvalue = FALSE, ci = FALSE,
                                  standardized = TRUE)
  # Prepare test
  par_index <- apply(CFA_par_c[, 1:3], 1, paste0, collapse = "")
  
  idx_load <- 1:(parms$L*parms$J)
  idx_error <- (parms$L*parms$J+1):(2*parms$L*parms$J)
  idx_lvcov <- (2*parms$L*parms$J+1+parms$L):nrow(CFA_par_c)
  
  # Error variance
  
  summary(fit_d)
  CFA_par_d[idx_error, 1:4]
  obj <- cbind(CFA_par_c[idx_error, c(1:3)], 
               disc = round(CFA_par_d[idx_error, "est"], 2),
               cont = round(CFA_par_c[idx_error, "est"], 2),
               ratio = round(CFA_par_d[idx_error, "est"] / CFA_par_c[idx_error, "est"], 2),
               type = apply(dat_disc, 2, max))
  CFA_par_d[idx_error, ]
  CFA_par_c[idx_error, ]
  ## Store Results
  store[i, ] <- c(mean(obj[obj$type > 5, "ratio"]),
                  mean(obj[obj$type == 5, "ratio"]),
                  mean(obj[obj$type == 2, "ratio"]))

  # Factor loadings
  obj <- cbind(CFA_par_c[idx_load, c(1:3)],
               disc = round(CFA_par_d[idx_load, "est"], 2),
               cont = round(CFA_par_c[idx_load, "est"], 2),
               ratio = round(CFA_par_d[idx_load, "est"] / CFA_par_c[idx_load, "est"], 2),
               type = apply(dat_disc, 2, max))

  mean(obj[obj$type > 5, "ratio"])
  mean(obj[obj$type == 5, "ratio"])
  mean(obj[obj$type == 2, "ratio"])
  # 
  # est_loadings <- mean(CFA_par[idx_load, "est"])
  # tru_loadings <- mean(dat_list$Lambda[dat_list$Lambda != 0])
  # 
  # # Latent Var covar
  # est_cov <- mean(CFA_par[idx_lvcov, "est"])
  # tru_cov <- mean(c(parms$lv_cov_ta, parms$lv_cov_mp, parms$lv_cov_ax))
  # 
  # 
  # ## Store Results
  # store[i, ] <- c(ord = mean(count %in% parms$K),
  #                 bin = mean(count %in% 2))
}

prop_achived <- store
prop_target  <- conds[, c("n_ord", "n_bin")]