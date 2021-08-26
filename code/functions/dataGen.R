### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-20
### Modified: 2021-08-25

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
  Phi[, -index_junk_aux] <- parms$cov_ax # not junk
  Phi[, index_junk_aux] <- parms$cov_junk # junk
  
  # Fix diagonal
  diag(Phi) <- 1
  
  # Make symmetric
  Phi[upper.tri(Phi)] <- t(Phi)[upper.tri(Phi)]

  index_discrete <- c(
    # ta = tail(parms$vmap_it$ta,
    #           length(parms$vmap_it$ta) * cond$D),
    mp = tail(parms$vmap$mp,
              length(parms$vmap$mp) * cond$D),
    ax = tail(parms$vmap$ax,
              length(parms$vmap$ax) * cond$D)
  )
  n_ordi <- length(index_discrete)
  n_cont <- parms$P-n_ordi

  # Define marginals
  marginal <- round(pnorm(seq(-2, 2, length.out = cond$K-1)), 4)

  marginal_list <- as.list(
    as.data.frame(
      matrix(rep(marginal, (cond$K - 1) * n_ordi),
             nrow = (cond$K - 1), ncol = n_ordi)
    )
  )

  # Using package for correction of matrix
  cmat <- OrdNor::cmat.star(marginal_list, Phi,
                            no.ord = n_ordi,
                            no.norm = n_cont)

  # Obtain Sample
  x <- genOrdNor(n = n,
                 plist = marginal_list,
                 cmat.star = cmat,
                 no.ord = n_ordi,
                 mean.vec = rep(0, n_norm),
                 sd.vec = rep(1, n_norm),
                 no.norm = n_norm)

# Rescale Observed Scores -------------------------------------------------

  x_scaled <- apply(x[, parms$vmap$ta], 2, function(j) j*sqrt(parms$item_var))
  x_center <- x_scaled + parms$item_mean
  x[, parms$vmap$ta] <- x_center

# Give meaningful names ---------------------------------------------------

  colnames(x) <- paste0("z", 1:ncol(x))
  colnames(scs_lv) <- paste0("lv", 1:ncol(scs_lv))
  
# Return Output -----------------------------------------------------------
  
  return(x)
}
