# Project:   mipca_compare
# Objective: Function to discretize data
# Author:    Edoardo Costantini
# Created:   2021-11-10
# Modified:  2021-11-10

disData <- function (x, K = Inf, D = 1, interval = TRUE, parms){

  # If the categories are not infinite in this condition, go on
  # Else return original data
  if(K != Inf){

    index_discrete <- c(
      # ta = tail(parms$vmap_it$ta,
      #           length(parms$vmap_it$ta) * D),
      mp = tail(parms$vmap$mp,
                length(parms$vmap$mp) * D),
      ax = tail(parms$vmap$ax,
                length(parms$vmap$ax) * D)
    )

    # Is the target interval scaled or not?
    if (interval == TRUE){
      for(j in index_discrete){
        x[, j] <- as.numeric(cut(x[, j],
                                 breaks = K,
                                 labels = 1:K))
        if(K == 2){
          x[, j] <- x[, j] - 1
        }
      }
    } else {
      prob_reduction <- .6 # every subsequent bin contains .6 of the remaining obs
      prob_in <- .2 # first bin probability
      probs <- rep(NA, K)

      for(k in 1:K){
        if(k < K){
          probs[k] <- prob_in
          whats_left <- (1-sum(probs, na.rm = TRUE))
          prob_in <- whats_left * prob_reduction
        } else {
          probs[k] <- whats_left
        }
      }

      for(j in index_discrete){
        x <- x[, j]
        x_sort <- sort(x)
        x_mem <- sort(sample(1:K, length(x), probs, replace = TRUE))
        map <- data.frame(value = as.character(x_sort),
                          bin = x_mem)
        target <- as.character(x)
        x[, j] <- map[match(target, map$value), "bin"]
      }
    }
  }

  return(data.frame(x))
}
