### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-20
### Modified: 2021-08-25

genData <- function(parms, cond){

# Example Input -----------------------------------------------------------

  # cond    <-  conds[56, ]

# Latent Variables Covariance matrix --------------------------------------

  Phi <- diag(parms$L)
  
  # Target Variables
  Phi[parms$vmap_lv$ta, ] <- parms$lv_cov_ta
  
  # MAR Predictors
  Phi[parms$vmap_lv$mp, ] <- parms$lv_cov_mp
  
  # Other Predictors (junk and no junk)
  nauxiliaries <- length(parms$vmap_lv$ax)
  index_junk_aux <- tail(parms$vmap_lv$ax, nauxiliaries * cond$pj)
  Phi[-index_junk_aux, ] <- parms$lv_cov_ax # not junk
  Phi[index_junk_aux, ] <- parms$lv_cov_junk # junk
  
  # Fix diagonal
  diag(Phi) <- 1
  
  # Make symmetric
  Phi[upper.tri(Phi)] <- t(Phi)[upper.tri(Phi)]
  
  # Make it covariance instead of correlation matrix (if lv_var != 1)
  Phi <- Phi * sqrt(parms$lv_var) * sqrt(parms$lv_var)
  
# Factor loadings (random factor) -----------------------------------------

  lambda <- parms$fl + runif(parms$P, min = -parms$fl_bound, max = parms$fl_bound)
  
# Observed Items Error Covariance matrix ----------------------------------
# Note: you are creating uncorrelated errors for the observed items
  
  Theta <- diag(parms$P)
  for (i in 1:length(lambda)) {
    Theta[i, i] <- 1 - lambda[i]^2 * Phi[1, 1]
  }
  
# Items Factor Complexity = 1 (simple measurement structure) --------------
# Reference: Bollen1989 p234
  
  Lambda <- matrix(nrow = parms$P, ncol = parms$L)
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
  scs_delta <- mvrnorm(parms$N, rep(0, parms$P), Theta)
    
# Compute Observed Scores -------------------------------------------------

  x <- data.frame(matrix(nrow = parms$N, ncol = parms$P))
  for(i in 1:parms$N){
    x[i, ] <- t(Lambda %*% scs_lv[i, ] + scs_delta[i, ])
  }

# Rescale Observed Scores -------------------------------------------------

  x_scaled <- sapply(x, function(x) x*sqrt(parms$item_var))
  x_center <- x_scaled + parms$item_mean
  x <- as.data.frame(x_center)

# Discretize if required --------------------------------------------------

  index_discrete <- c(
    # ta = tail(parms$vmap_it$ta,
    #           length(parms$vmap_it$ta) * cond$D),
    # mp = tail(parms$vmap_it$mp,
    #           length(parms$vmap_it$mp) * cond$D),
    ax = tail(parms$vmap_it$ax,
              length(parms$vmap_it$ax) * cond$D)
  )
  x_disc <- data.frame(matrix(nrow = nrow(x),
                              ncol = ncol(x)))
  for(j in index_discrete){
    x_disc[, j] <- cut(x[, j],
                       breaks = cond$K,
                       labels = 1:cond$K)
  }
  x[, index_discrete] <- x_disc[, index_discrete]

# Give meaningful names ---------------------------------------------------

  colnames(x) <- paste0("z", 1:ncol(x))
  colnames(scs_lv) <- paste0("lv", 1:ncol(scs_lv))
  
# Return Output -----------------------------------------------------------
  
  return( 
    list(dat_ob = as.data.frame(x),
         dat_lv = as.data.frame(scs_lv),
         Phi    = Phi,
         Theta  = Theta,
         Lambda = Lambda)
  )
}
