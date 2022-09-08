# Project:   mipca_compare
# Objective: Function to generate data with a latent structure
# Author:    Edoardo Costantini
# Created:   2021-11-10
# Modified:  2022-09-08

genDataLatent <- function(parms, cond){

  # Define parameters -----------------------------------------------------
  N <- parms$N
  P <- parms$P
  L <- parms$L
  J <- P / L
  n_aux <- L - 1
  p_junk <- cond$pj
  rho_high <- parms$cov_ta
  rho_junk <- parms$cov_junk
  largeP <- parms$largeP

  # Latent Variables Covariance matrix --------------------------------------

  # Base
  Phi <- toeplitz(c(1, rep(rho_high, L-1)))

  # Distinguish between important variables and possible auxiliary (latent)
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

  if(largeP == TRUE){

    # How many items to add to each possible auxiliary latent variable
    J_extra <- 31

    # Update the total number of items
    P <- parms$P + J_extra * (parms$L - 1)

    # Create a new lamba vector
    lambda <- rep(.85, P)

    # Create a new Theta
    Theta <- diag(1 - lambda^2, P)

    # Store the first part of the Lambda matrix (first latent variable measured by 8 items) 
    Lambda_pt1 <- matrix(0, nrow = J, ncol = L)
    Lambda_pt1[, 1] <- lambda[1]
    
    # Store the second part of the Lambda matrix (all other latent variables measured by all other items)
    Lambda_pt2 <- matrix(0, nrow = (P - J), ncol = L)
    start <- 1
    for (j in 2:L) {
      end <- (start + J + J_extra) - 1
      vec <- rep(0, (P - J))
      vec[start:end] <- lambda[start:end]
      Lambda_pt2[, j] <- vec
      start <- end + 1
    }

    # Combine the two parts
    Lambda <- rbind(Lambda_pt1, Lambda_pt2)


  }

  # Distinguish between important variables and possible auxiliary (items)
  index_junk_aux_items <- tail(9:P, floor(length(9:P) * cond$pj))

  # Distinguish between important variables and possible auxiliary (items)
  index_good_aux_items <- head(9:P, ceiling(length(9:P) * (1 - cond$pj)))

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
         index_junk_aux = index_junk_aux_items,
         index_good_aux = index_good_aux_items)
  )

}