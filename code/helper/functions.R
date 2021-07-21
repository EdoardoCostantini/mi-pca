### Title:    helper functions
### Project:  Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-07-21
### Note:     This script contains functions of level-1 which are functions
###           that cannot contain other functions I defined). This is the
###           lower functional level. I can use functions defined by other
###           pacakges, but not other functions I have written for this
###           project. If I wanted to include them I would then be working
###           with a subroutine.

# Estimate Descriptives ---------------------------------------------------

satModWrite <- function(var_id){
  ## Description:
  # Given a set of variable names it returns a character vector containing
  # the model description that estiamtes the mean, variance, and covariance
  # of all variables (when given to a lavaan::sem() function
  ## Example input:
  # var_id <- paste0("z", parms$varMap_items$ta)

  # Means
  head_means <- "# Means\n"
  all_means <- paste0(var_id, " ~ ", "1")
  all_means <- paste(all_means, collapse = "\n")

  # Variances
  head_vars <- "# Variances \n"
  all_vars <- paste0(var_id, " ~~ ", var_id)
  all_vars <- paste(all_vars, collapse = "\n")

  # Coivariances
  head_covs <- "# Covariances \n"
  all_covs <- combn(var_id, 2)
  all_covs <- apply(all_covs, 2, paste0, collapse = " ~~ ")
  all_covs <- paste(all_covs, collapse = "\n")

  # Put together
  SAT_mod <- paste(head_means, all_means,
                   head_vars, all_vars,
                   head_covs, all_covs
  )

  return(SAT_mod)
}

miFitSat <- function (mi_data, model){
  # Fit saturated model
  fits <- lapply(mi_data, function (x){
    lavaan::sem(model = model,
                data = x,
                likelihood = "wishart",
                std.lv = TRUE)
  })
}

miFitCFA <- function (mi_data, model){
  lapply(mi_data, function (x){
    cfa(model,
        data = x,
        std.lv = TRUE)
  })
}

miPool <- function(mi_fits, m, N){
  ## Description:
  # Given a list of outputs from a lavaan::cfa or sem model, fitted on multiple
  # datasets obtained by MI, it returns all parameter estiamtes, confidence
  # intervals, and FMI
  ## Example Inputs
  # mi_fits = miFitCFA(mi_data = imp_out$dats, model = parms$CFA_model)
  # mi_fits = miFitSat(mi_data = imp_out$dats,
  #                       model = satModWrite(names(dat[, parms$varMap_items$ta])))
  # m = parms$mice_ndt
  # N = parms$N

  ## Pool estiamtes
  ests <- lapply(mi_fits, function(x) {
    est_all <- parameterEstimates(x, standardized = TRUE)
    est <- est_all[, c("est", "std.all")]
    rownames(est) <- apply(est_all[, 1:3], 1, paste0, collapse = "")
    return(est)
  })
  ests_both <- do.call(cbind, ests)
  coefs_raw <- as.matrix(ests_both[, grep("est", colnames(ests_both))])
  coefs_std <- ests_both[, grep("std.all", colnames(ests_both))]
  Q_bar <- rowMeans(coefs_raw)
  Q_bar_std <- rowMeans(coefs_std)

  ## Pool Confidence Intervals
  all_vcov <- lapply(X = mi_fits, vcov)
  U_bar <- diag(Reduce('+', all_vcov) / m)
  B <- diag(1 / (m-1) * (coefs_raw - Q_bar) %*% t(coefs_raw - Q_bar))
  T_var <- U_bar + B + B/m

  # Degrees of freedom
  nu_com <- N - length(Q_bar) # n - k where k number of paramteres estimated
  nu <- miDf(length(mi_fits), b = B, t = T_var, nu_com)

  # CI computation
  t_nu <- qt(1 - (1-.95)/2, df = nu)
  CI <- data.frame(lwr = Q_bar - t_nu * sqrt(T_var),
                   upr = Q_bar + t_nu * sqrt(T_var))

  ## Store
  pooled <- cbind(names(Q_bar), Q_bar_std, Q_bar, CI)
  colnames(pooled) <- c("par", "std.all", "est", "ci.lower", "ci.upper")

  return(pooled)
}

# Multiple Imputation Related ---------------------------------------------
lambda <- function(x) (x + 1) / (x + 3)

fmi <- function(m, b, t){
  # proportion of variation attributable to the missing data
  # aka fmi (not adjusted for the finite number of imps)
  fmi <- (1 + 1/m) * b/t
  return(fmi)
}

miDf <- function(m, b, t, dfCom) {
  fmi   <- fmi(m, b, t)
  df0   <- (m - 1) * (1 / fmi^2)
  dfObs <- lambda(dfCom) * dfCom * (1 - fmi)

  df0 / (1 + (df0 / dfObs))
}