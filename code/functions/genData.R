### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-20
### Modified: 2021-11-10

genData <- function(parms, cond){

# Example Input -----------------------------------------------------------

  # cond    <-  conds[9, ]

# Latent Variables Covariance matrix --------------------------------------

  Phi <- diag(parms$P)
  
  # Target Variables
  Phi[parms$vmap$ta, ] <- parms$cov_ta
  
  # MAR Predictors
  Phi[parms$vmap$mp, ] <- parms$cov_mp
  
  # Other Predictors (junk and no junk)
  nauxiliaries <- length(parms$vmap$ax)
  index_junk_aux <- tail(parms$vmap$ax, nauxiliaries * cond$pj)
  if(length(index_junk_aux) == 0){
    Phi[-c(parms$vmap$ta, parms$vmap$mp), ] <- parms$cov_ax
  } else {
    Phi[-index_junk_aux, ] <- parms$cov_ax # not junk
    Phi[index_junk_aux, ] <- parms$cov_junk # junk
  }
  
  # Fix diagonal
  diag(Phi) <- 1
  
  # Make symmetric
  Phi[upper.tri(Phi)] <- t(Phi)[upper.tri(Phi)]

  # Sample from MVN
  x <- mvrnorm(n = parms$N,
               mu = rep(0, parms$P),
               Sigma = Phi)

  colnames(x) <- paste0("z", 1:ncol(x))

  # Scale it correctly
  x_scaled <- apply(x, 2, function(j) j*sqrt(parms$item_var))
  x_center <- x_scaled + parms$item_mean
  x_cont <- data.frame(x_center)

# Return Output -----------------------------------------------------------

  return(
    list(x = data.frame(x_cont),
         index_junk_aux = index_junk_aux)
  )
}
