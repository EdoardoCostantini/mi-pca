# Project:   mipca_compare
# Objective: Collection of functions needed for MI tasks
# Author:    Edoardo Costantini
# Created:   2021-08-24
# Modified:  2021-08-24

fmi <- function(m, b, t){
  # proportion of variation attributable to the missing data
  # aka fmi
  # (van Buuren, 2018, p. 46)
  fmi <- (1 + 1/m) * b/t
  return(fmi)
}

riv <- function(m, b, u){
  # relative increase in variance due to nonresponse
  # (van Buuren, 2018, p. 47)
  riv <- (1 + 1/m) * b/u
  return(riv)
}

miDf <- function(m, b, t, dfCom) {
  fmi   <- fmi(m, b, t)
  df0   <- (m - 1) * (1 / fmi^2)
  dfObs <- (dfCom + 1) / (dfCom + 3) * dfCom * (1 - fmi)

  df0 / (1 + (df0 / dfObs))
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
  #                       model = satModWrite(names(dat[, parms$vmap_it$ta])))
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