# Project:   mipca_compare
# Objective: Function to generate data with a latent structure
# Author:    Edoardo Costantini
# Created:   2021-11-10
# Modified:  2021-11-15

genDataLatent <- function(parms, cond){

  # Define parameters -----------------------------------------------------
  N <- parms$N
  P <- parms$P
  L <- 7
  J <- P / L
  n_aux <- L - 1
  p_junk <- cond$pj
  rho_high <- parms$cov_ta
  rho_junk <- parms$cov_junk

  # Latent Variables Covariance matrix --------------------------------------

  # Base
  Phi <- toeplitz(c(1, rep(rho_high, L-1)))

  # Distinguish between important variables and possible auxiliary
  index_junk_aux <- tail(1:ncol(Phi),
                         round(n_aux * p_junk, 0))
  # Change rho if needed values
  Phi[index_junk_aux, ] <- rho_junk # junk
  # Fix diagonal
  diag(Phi) <- 1
  # Make symmetric
  Phi[upper.tri(Phi)] <- t(Phi)[upper.tri(Phi)]

  # Factor loadings ---------------------------------------------------------
  lambda <- rep(.85, P)

  # Observed Items Error Covariance matrix ----------------------------------
  # Note: here we create uncorrelated errors for the observed items

  Theta <- diag(P)
  for (i in 1:length(lambda)) {
    Theta[i, i] <- 1 - lambda[i]^2
  }

  # Items Factor Complexity = 1 (simple measurement structure) --------------
  # Reference: Bollen1989 p234

  Lambda <- matrix(nrow = P, ncol = L)
  start <- 1
  for (j in 1:L) {
    end <- (start + J) - 1
    vec <- rep(0, P)
    vec[start:end] <- lambda[start:end]
    Lambda[, j] <- vec
    start <- end + 1
  }

  # Sample Scores -----------------------------------------------------------

  scs_lv    <- mvrnorm(N, rep(0, L), Phi)
  scs_delta <- mvrnorm(N, rep(0, P), Theta)

  # Compute Observed Scores -------------------------------------------------

  x <- matrix(nrow = N, ncol = P)
  for(i in 1:N){
    x[i, ] <- t(0 + Lambda %*% scs_lv[i, ] + scs_delta[i, ])
  }

  # Give meaningful names ---------------------------------------------------

  colnames(x) <- paste0("z", 1:ncol(x))
  colnames(scs_lv) <- paste0("lv", 1:ncol(scs_lv))

  # Scale it correctly
  x_scaled <- apply(x, 2, function(j) j*sqrt(parms$item_var))
  x_center <- x_scaled + parms$item_mean
  x_cont <- data.frame(x_center)

  # Return ------------------------------------------------------------------
  return(
    list(x = data.frame(x_cont),
         index_junk_aux = index_junk_aux)
  )

}