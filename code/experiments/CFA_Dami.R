### Title:    Lisa Step 4: Pool results together
### Project:  Imputing High Dimensional Data
### Author:   Damiano D'Urso
### Created:  2021-05-31
### Notes:    Example script to see how he creates ordinal data 
###           according to CFA

# session clean up --------------------------------------------------------

rm(list=ls())

# packages ----------------------------------------------------------------

library(MASS)
library(lavaan)

# helper function ---------------------------------------------------------

# Function to cut data and make ordinal
Cuts_Data <- function(x, thresh){
  Datacut <- list()
  xordered <- x
  for (i in 1:ncol(x)){
    x[,i] <- as.numeric(cut(x[,i], breaks = c(-Inf, thresh[i,], +Inf))) 
    xordered[,i] <- ordered(x[,i])
  }
  Datacut$numeric <- x
  Datacut$ordered <- xordered
  return(Datacut)  
}

# generate data: continuous and discretized version -----------------------

  repetitions <- 10 # Data replications
  sample <- 1e4 # number of subjects
  LV <- 1 # Number of latent variables (factors)
  lambda <- 0.7 # Factor loadings size
  observed <- 5 # Number of items 
  categories <- 3 # categories
  itemset <- 1
  Thr <- matrix(rep(c(-2, 0, 2)), # Thresholds
                ncol = categories,
                nrow = observed, 
                byrow = T)  
  
  phi <- c(1,1)     # latent variable variances
  nu <- rep(0,observed)    # Intercepts for all indicators are 0
  
  lambda <- matrix(c(rep(lambda, itemset)), nrow = observed, ncol = LV)
  
  # Matrices
  kappa <- matrix(0, nrow = LV, ncol = LV)
    # latent mean group 1 is 0
  
  errorvar <- 1 - lambda[1:observed]^2 # error variance 
  theta <- diag(errorvar) # matrix of error variances
  phi <- matrix(phi, nrow = LV, ncol = LV)
  
  # Generate Data
  # Simulating data
  eta <- mvrnorm(sample, mu = kappa, Sigma = phi) # factor scores group 1
  
  epsilon <- mvrnorm(sample, 
                       rep(0, observed), 
                       Sigma = theta) # residual scores group 1 and 2
  
  dt <- nu + eta %*% t(lambda) + epsilon
    colnames(dt) <- paste0("z", 1:ncol(dt))
  dt_ord <- Cuts_Data(dt, Thr[1:observed,])
    head(dt_ord$numeric)
  
# generate data: continuous attenuated as discrete ------------------------

  # Define parameters
  lambda <- 0.35 # Factor loadings size
  lambda <- matrix(c(rep(lambda, itemset)), nrow = observed, ncol = LV)
  errorvar <- .34 - lambda[1:observed]*lambda[1:observed] # error variance 
  theta <- diag(errorvar) # matrix of error variances
  phi <- matrix(phi, nrow = LV, ncol = LV)
  
  # Sample latent scores
  eta <- mvrnorm(sample, mu = kappa, Sigma = phi)
  
  # Sample errors
  epsilon <- mvrnorm(sample, rep(0, observed), Sigma = theta)
  
  # Derive item scores
  dt_goal <- nu + eta %*% t(lambda) + epsilon
  
  # Give proper names
  colnames(dt_goal) <- paste0("z", 1:ncol(dt_goal))
  
# fit models --------------------------------------------------------------

  # Define CFA model
  CFA_model <- paste0("lv", 
                      " =~ ",
                      paste0(colnames(dt), collapse = " + ")
  )
  
  # Continuous version
  fit <- cfa(CFA_model, data = dt, std.lv = TRUE)
  CFAest <- parameterEstimates(fit, standardized = TRUE)
  
  # Ordinal Version
  fit_ord <- cfa(CFA_model, data = dt_ord$numeric, std.lv = TRUE)
  CFAest_ord <- parameterEstimates(fit_ord, standardized = TRUE)
  
  # Goal
  fit_goal <- cfa(CFA_model, data = dt_goal, std.lv = TRUE)
  CFAest_goal <- parameterEstimates(fit_goal, standardized = TRUE)
  
  # Look at results
  CFAest_comp <- cbind(CFAest[, 1:3],
                       cont = round(CFAest[, 4], 3),
                       ord  = round(CFAest_ord[, 4], 3),
                       goal = round(CFAest_goal[, 4], 3))
  CFAest_comp[, "cont"]/CFAest_comp[, "ord"]
  
# compare results ---------------------------------------------------------

  # Item variances
  cbind(cont = round(apply(dt, 2, var), 3),
        ord  = round(apply(dt_ord$numeric, 2, var), 3),
        goal = round(apply(dt_goal, 2, var), 3))

  # Observed Correlations
  lapply(list(cont = dt,
              ord  = dt_ord$numeric,
              goal = dt_goal), 
         function(x){round(cor(x), 3)})
  