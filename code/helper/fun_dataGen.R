### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-20

genData <- function(parms, cond){

# Example Input -----------------------------------------------------------

  # cond    <-  conds[6, ]
  
# Latent Variables Covariance matrix --------------------------------------

  Phi <- diag(cond$L)
  
  # Target Variables
  Phi[parms$varMap$ta, ] <- parms$lv_cov_ta
  
  # MAR Predictors
  Phi[parms$varMap$mp, ] <- parms$lv_cov_mp
  
  # Other Predictors
  Phi[parms$varMap$ax, ] <- parms$lv_cov_ax
  
  # Fix diagonal
  diag(Phi) <- 1
  
  # Make symmetric
  Phi[upper.tri(Phi)] <- t(Phi)[upper.tri(Phi)]
  
  # Make it covariance instead of correlation matrix (if lv_var != 1)
  Phi <- Phi * sqrt(parms$lv_var) * sqrt(parms$lv_var)
  
# Factor loadings (random factor) -----------------------------------------

  lambda <- runif(parms$P, 
                  min = parms$fl - .02, 
                  max = parms$fl + .02)
  
# Observed Items Error Covariance matrix ----------------------------------
# Note: you are creating uncorrelated errors for the observed items
  
  Theta <- diag(parms$P)
  for (i in 1:length(lambda)) {
    Theta[i, i] <- parms$item_var - lambda[i]^2 * Phi[1, 1]
  }
  
# Items Factor Complexity = 1 (simple measurement structure) --------------
# Reference: Bollen1989 p234
  
  Lambda <- matrix(nrow = parms$P, ncol = cond$L)
  start <- 1
  for (j in 1:parms$L) {
    end <- (start + parms$J) - 1
    vec <- rep(0, parms$P)
    vec[start:end] <- lambda[start:end]
    Lambda[, j] <- vec
    start <- end + 1
  }

# Sample Scores -----------------------------------------------------------
  
  scs_lv    <- mvrnorm(parms$N, rep(parms$lv_mean, parms$L), Phi)
  scs_delta <- mvrnorm(parms$N, rep(parms$item_mean, parms$P), Theta)
    
# Compute Observed Scores -------------------------------------------------

  x <- matrix(nrow = parms$N, ncol = parms$P)
  for(i in 1:parms$N){
    x[i, ] <- t(parms$item_mean + Lambda %*% scs_lv[i, ] + scs_delta[i, ])
  }


# Give meaningful names ---------------------------------------------------

  colnames(x) <- paste0("z", 1:ncol(x))
  colnames(scs_lv) <- paste0("lv", 1:ncol(scs_lv))
  
# Return Output -----------------------------------------------------------
  
  return( 
    list(dat_ob = x,
         dat_lv = scs_lv,
         Phi    = Phi,
         Theta  = Theta,
         Lambda = Lambda)
  )
}
