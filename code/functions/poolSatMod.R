# Project:   mipca_compare
# Objective: Pool parameter estimated with saturated model
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-10-04

poolSatMod <- function(fits, alphaCI = .95){
  ## Description
  # Given a list fits of (saturated) SEM models on multiply imputed datasets
  # it returns the pooled parameter estaitmes, CIs and building blocks

  ## Body
  ## Coef estimates
  tryCatch({
    ### START TRYCATCH EXPRESSION
    m <- length(fits)

    coefs <- sapply(X = fits,
                    FUN = function(x) coef(x))
    Q_bar <- rowMeans(coefs)

    ## Variances (squared standard error for each parameter)
    all_vcov <- lapply(X = fits,
                       FUN = function(x) vcov(x))
    U_bar <- diag(Reduce('+', all_vcov) / m)

    B <- diag(1 / (m-1) * (coefs - Q_bar) %*% t(coefs - Q_bar))

    T_var <- U_bar + B + B/m

    ## Degrees of freedom
    nu_com <- parms$N - nrow(coefs) # n - k where k number of paramteres estimated
    nu <- miDf(length(fits), b = B, t = T_var, nu_com)

    ## CI computation
    t_nu <- qt(1 - (1 - alphaCI)/2,
               df = nu)

    CI <- data.frame(lwr = Q_bar - t_nu * sqrt(T_var),
                     upr = Q_bar + t_nu * sqrt(T_var))

    ## Store results
    return(cbind(
      par = rownames(CI),
      Q_bar,
      CI
    ))

    ### END TRYCATCH EXPRESSION
  }, error = function(e){
    err <- paste0("Original Error: ", e)
    print(err)
    return(NULL)
  }
  )
}
