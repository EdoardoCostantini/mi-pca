# Project:   mipca_compare
# Objective: Function to compute bias, coverage, and CIW
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-09-29

evaPerf <- function ()

## Prep ##
  est <- paste0(model, "_EST")
  ci  <- paste0(model, "_CI")
  select_cond <- names(out[[1]])[condition]
  cond <- unique(out$tag)[1]
  reps <- 5
  ## Step 2. Bias ##
  # avg <- sapply(out$parms$methods, function(m){
  #   # m <- out$parms$methods[2]
  #   store <- NULL
  #   count <- 0
  #   for (i in 1:out$parms$dt_rep) {
  #     # i <- 1
  #     succ_method <- colnames(out[[i]][[select_cond]][[est]])
  #     result <- as.matrix(out[[i]][[select_cond]][[est]])[, succ_method %in% m]
  #     if(any(is.na(result))) {
  #       count <- count+1
  #       next
  #     }
  #     store <- cbind(store, result)
  #   }
  #   # apply(store, 1, var)
  #   c(rowMeans(store, na.rm = TRUE), rep = ncol(store)) # MCMC statistics
  # })

  par_avg <- NULL
  par_var <- NULL

# Cast experimental factors to ordered factors
out$par <- factor(out$par, levels = unique(out$par), ordered = TRUE)
out$tag <- factor(out$tag, levels = unique(out$tag), ordered = TRUE)
out$method <- factor(out$method, levels = unique(out$method), ordered = TRUE)

# True values
ref_df <- out %>%
  filter(method == "OG") %>%
  group_by(par, tag) %>%
  dplyr::summarize(ref = mean(Q_bar))
ref_vec <- ref_df$ref
out$ref <- rep(ref_vec, nrow(out) / length(ref_vec))
head(out, 50)

# Idea of bias
out %>%
  filter(method == "MITR") %>%
  group_by(par, tag) %>%
  dplyr::summarize(Mean = mean(Q_bar))

out %>%
  filter(method == "CC") %>%
  group_by(par, tag) %>%
  dplyr::summarize(Mean = mean(Q_bar))

data.frame(out %>%
             filter(method == "vbv") %>%
             group_by(par, tag) %>%
             dplyr::summarize(Mean = mean(Q_bar)))

# Confidence interval coverage for a given method across other factors
# In or out?
out$cover_log <- out$lwr < out$ref & out$ref < out$upr
data.frame(out %>%
             # filter(method == "OG") %>%
             group_by(method, par, tag) %>%
             dplyr::summarize(coverage = mean(cover_log)))

out %>%
  filter(method == "OG") %>%
  group_by(par, tag) %>%
  dplyr::summarize(coverage = mean(cover_log))

# Confidence interval width for a given method across other factors
out$CIW <- abs(out$lwr - out$upr)
data.frame(out %>%
             # filter(method == "OG") %>%
             group_by(method, par, tag) %>%
             dplyr::summarize(coverage = mean(CIW)))

out %>%
  filter(method == "MIMI") %>%
  group_by(par, tag) %>%
  dplyr::summarize(Mean = mean(Q_bar))

  for(m in out$parms$methods){
    # m <- out$parms$methods[2]
    store <- NULL
    count <- 0
    out_cond <- out[out$tag == cond, ]
    for (i in 1:reps) {
      # i <- 1
      result <- as.matrix(out[[i]][[select_cond]][[est]])[, succ_method %in% m]
      if(any(is.na(result))) {
        count <- count+1
        next
      }
      store <- cbind(store, result)
    }
    par_avg <- cbind(par_avg,
                     c(rowMeans(store, na.rm = TRUE),
                       rep = ncol(store)))
    par_var <- cbind(par_var,
                     apply(store, 1, var))
  }
  colnames(par_avg) <- colnames(par_var) <- out$parms$methods

  # Store Objects
  avg <- par_avg
  avg <- data.frame(avg[, colSums(is.nan(avg)) == 0])
    # get rid of NaNs that come up in exp3 for conditions that are not using
    # certain methods
  validReps  <- avg["rep", ] # number of successes
  avg        <- avg[-which(rownames(avg) == "rep"), ]
  psd_tr_vec <- avg[, "GS"] # pseudo true values

  if(out$parms$exp == 1 & model == "sem"){
    # Fixes a problem of old results from privous runs of eperiment 1
    # in the future I will delte these part as exp 1 results wiil be
    # uniform with rest
    fit <- lavaan::sem(out$parms$lav_model,
                       data = out[[1]][[select_cond]]$dat_full,
                       likelihood = "wishart")
    rownames(avg) <- apply(parameterEstimates(fit)[,1:3],
                           1,
                           paste0,
                           collapse = "")
  }

  # Raw bias
  bias <- avg - psd_tr_vec

  # Bias as percentage of true value
  bias_per <- cbind(ref = round(psd_tr_vec, 3),
                    bias / psd_tr_vec * 100)

  meths <- out$parms$methods[-which(out$parms$methods == "GS")]
  # Bias Mean Standardized
  if(bias_sd == TRUE){
    # Empirical Standard error
    sd_emp <- sapply(out$parms$methods, function(m){
      # m <- out$parms$methods[1]
      store <- NULL
      for (i in 1:out$parms$dt_rep) {
        succ_method <- colnames(out[[i]][[select_cond]][[est]])
        store <- cbind(store,
                       out[[i]][[select_cond]][[est]][,
                                                      succ_method %in% m]
        )
      }
      return( apply(t(store), 2, sd, na.rm = TRUE) )
    })

    bias_sd_out <- (avg - psd_tr_vec) / sd_emp

  } else {
    bias_sd_out <- NULL
  }

  ## Step 3. CI Coverage ##
  # storing threshold
  str_thrs <- nrow(out[[1]][[select_cond]][[ci]])/2

  # Confidence Interval Coverage
  CIC <- sapply(out$parms$methods, function(m){
    store <- NULL
    for (i in 1:out$parms$dt_rep) {
      succ_method <- colnames(out[[i]][[select_cond]][[est]])
      col_indx <- succ_method %in% m
      cond_est <- out[[i]][[select_cond]][[est]]
      cond_CI  <- out[[i]][[select_cond]][[ci]]
      ci_low   <- cond_CI[1:str_thrs, col_indx]
      ci_hig   <- cond_CI[-(1:str_thrs), col_indx]
      store <- cbind(store,
                     ci_low < psd_tr_vec & psd_tr_vec < ci_hig
      )
    }
    rowMeans(store, na.rm = TRUE) # MCMC statistics
  })

  if(is.vector(CIC)){ # if it's a vector make it a dataframe with special care
    CIC_nan <- data.frame(t(is.nan(CIC)))
    CIC <- data.frame(t(CIC[colSums(CIC_nan) == 0]))
  } else {
    CIC <- CIC[, colSums(is.nan(CIC)) == 0]
    CIC <- data.frame(CIC)
  }
    # get rid of NaNs that come up in exp3 for conditions that are not using
    # certain methods
  rownames(CIC) <- rownames(bias)

  ## Step 4 - Euclidean distances
  # MCMC estimates (per model)
  ed_est <- sapply(avg, function(avg_col){
    # avg_col <- avg[[1]]
    dist(rbind(avg_col, psd_tr_vec), method = "euclidean")
  }
  )

  # Confidence intervals
  ed_ci <- sapply(CIC, function(CIC_col){
    # avg_col <- avg[[1]]
    dist(rbind(CIC_col, rep(.95, nrow(CIC))), method = "euclidean")
  }
  )

  ## Step 5 - Confidence interval width
  # Confidence Interval Coverage
  CIW <- sapply(out$parms$methods, function(m){
    store <- NULL
    for (i in 1:out$parms$dt_rep) {
      succ_method <- colnames(out[[i]][[select_cond]][[est]])
      col_indx <- succ_method %in% m
      cond_est <- out[[i]][[select_cond]][[est]]
      cond_CI  <- out[[i]][[select_cond]][[ci]]
      ci_low   <- cond_CI[1:str_thrs, ]
      ci_hig   <- cond_CI[-(1:str_thrs), ]

      store <- cbind(store, (ci_hig[, col_indx] - ci_low[, col_indx]))
    }
    rowMeans(store, na.rm = TRUE) # MCMC statistics
  })
  CIW <- CIW[, colSums(is.nan(CIW)) == 0]
  CIW <- data.frame(CIW)
  rownames(CIW) <- rownames(bias)