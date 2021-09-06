### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-20
### Modified: 2021-09-06

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
  index_junk_aux <- head(parms$vmap$ax, nauxiliaries * cond$pj)
  if(length(index_junk_aux) == 0){
    Phi[-c(parms$vmap$ta, parms$vmap$mp), ] <- parms$cov_ax
  } else {
    Phi[, -index_junk_aux] <- parms$cov_ax # not junk
    Phi[, index_junk_aux] <- parms$cov_junk # junk
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

  # Discretise it
  x_ordi <- x_cont
  index_discrete <- c(
    # ta = tail(parms$vmap_it$ta,
    #           length(parms$vmap_it$ta) * cond$D),
    mp = tail(parms$vmap$mp,
              length(parms$vmap$mp) * cond$D),
    ax = tail(parms$vmap$ax,
              length(parms$vmap$ax) * cond$D)
  )
  for(j in index_discrete){
    x_ordi[, j] <- cut(x_cont[, j],
                       breaks = cond$K,
                       labels = 1:cond$K,
                       ordered_result = TRUE)
  }
  
# Return Output -----------------------------------------------------------

  return(
    list(cont = x_cont,
         ordi = x_ordi)
  )
}
