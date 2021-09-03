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
  quantiles <- seq(-2, 2, length.out = 1e3) # from a normal distribution
  chunk_size <- 1e3/cond$K # size of chunks
  quntiles_groups <- split(quantiles,
                           ceiling(seq_along(quantiles)/chunk_size))
  bounds <- sapply(quntiles_groups, min)[-1]
  marginal <- round(pnorm(bounds), 4)
  marginal_list <- as.list(
    as.data.frame(
      matrix(rep(marginal, (cond$K - 1) * n_ordi),
             nrow = (cond$K - 1), ncol = n_ordi)
    )
  )
  # min(abs(unlist(valid.limits(marginal_list, n_ordi,n_cont))))

  if(cond$K > 2){

    # Using package for correction of matrix
    cmat <- OrdNor::cmat.star(marginal_list, Phi,
                              no.ord = n_ordi,
                              no.norm = n_cont)

    # Obtain Sample
    x <- genOrdNor(n = parms$N,
                   plist = marginal_list,
                   cmat.star = cmat,
                   no.ord = n_ordi,
                   mean.vec = rep(0, n_cont),
                   sd.vec = rep(1, n_cont),
                   no.norm = n_cont)
  } else {
    sigma.star <- compute.sigma.star(no.bin = n_ordi,
                                     no.nor = n_cont,
                                     prop.vec.bin = unlist(marginal_list),
                                     corr.mat = Phi)

    x <- jointly.generate.binary.normal(parms$N,
                                        no.bin = n_ordi,
                                        no.nor = n_cont,
                                        prop.vec.bin = unlist(marginal_list),
                                        mean.vec.nor = rep(0, n_cont),
                                        var.nor = rep(1, n_cont),
                                        sigma_star = sigma.star$sigma_star,
                                        continue.with.warning = TRUE)
  }

# Rescale Observed Scores -------------------------------------------------

  x_scaled <- apply(x[, parms$vmap$ta], 2, function(j) j*sqrt(parms$item_var))
  x_center <- x_scaled + parms$item_mean
  x[, parms$vmap$ta] <- x_center
  x <- data.frame(x)

# Transform into factors ordinal items -------------------------------------------------

  index_factor <- sapply(x, function (j) {length(unique(j)) <= 10})
  x[, index_factor] <- data.frame(lapply(x[, index_factor], function (j) {factor(j, ordered = TRUE)}))

# Give meaningful names ---------------------------------------------------

  colnames(x) <- paste0("z", 1:ncol(x))
  
# Return Output -----------------------------------------------------------
  
  return(x)
}
