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

# generate data: continuous and discretized version -----------------------

  sample      <- 1e4 # number of subjects
  LV          <- 1 # Number of latent variables (factors)
  itemset     <- 1
  observed    <- 5 # Number of items
  categories  <- 3 # categories
  lambda      <- 0.7 # Factor loadings size

  # Latent model true parameters
  nu          <- rep(0, observed) # Intercepts for all indicators are 0
  phi         <- 1                # latent variable variances
  lambda      <- matrix(c(rep(lambda, itemset)),
                        nrow = observed, ncol = LV)
  kappa       <- matrix(0, nrow = LV, ncol = LV) # latent mean group 1 is 0
  errorvar    <- 1 - lambda[1:observed]^2 # error variance
  theta       <- diag(errorvar) # matrix of error variances
  phi         <- matrix(phi, nrow = LV, ncol = LV)

  # Generate Latent variable scores
  eta         <- mvrnorm(sample, mu = kappa, Sigma = phi) # factor scores group 1

  # Generate uniqueness
  epsilon     <- mvrnorm(sample,
                         rep(0, observed),
                         Sigma = theta)

  # Generate Continuous Observed Data
  dt          <- nu + eta %*% t(lambda) + epsilon
    colnames(dt) <- paste0("z", 1:ncol(dt))

  # Discretize condituous data
  dt_ord <- apply(dt, 2, function(j){
    as.numeric(cut(j, breaks = categories))
  })
  dt_ord <- scale(dt_ord) # scale it to have same variance as original

  # Plot distributions
  hist(scale(dt_ord[, 2], scale = FALSE), freq = FALSE)
  lines(density(dt[, 2]))

# fit models --------------------------------------------------------------

  # Define CFA model
  CFA_model <- paste0("lv", 
                      " =~ ",
                      paste0(colnames(dt), collapse = " + ")
  )
  
  # Continuous version
  fit <- cfa(CFA_model, data = dt, std.lv = TRUE)
  CFAest <- parameterEstimates(fit, standardized = TRUE)

  # Ordinal Version (treated as ordinal)
  fit_ord_T <- cfa(CFA_model, data = dt_ord, std.lv = TRUE, ordered = TRUE)
  CFAest_ord_T <- parameterEstimates(fit_ord_T, standardized = TRUE)

  # Ordinal Version (treated as continuous)
  fit_ord_F <- cfa(CFA_model, data = dt_ord, std.lv = TRUE)
  CFAest_ord_F <- parameterEstimates(fit_ord_F, standardized = TRUE)

# generate data: continuous attenuated as discrete ------------------------

  # Define parameters
  lambda <- mean(CFAest_ord_F[grep("=~", CFAest_ord_F$op), "est"]) # Factor loadings size
  lambda <- matrix(c(rep(lambda, itemset)),
                   nrow = observed, ncol = LV)
  item_variance <- mean(apply(dt_ord, 2, var))
  errorvar <- item_variance - lambda[1:observed]*lambda[1:observed] # error variance
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

  # Goal
  fit_goal <- cfa(CFA_model, data = dt_goal, std.lv = TRUE)
  CFAest_goal <- parameterEstimates(fit_goal, standardized = TRUE)

# compare results ---------------------------------------------------------

  # Indexes
  CFA_cont_index <- c(grep("=~", CFAest$op),
                      grep("~~", CFAest$op))
  CFA_ordi_index <- c(grep("=~", CFAest_ord_T$op),
                      grep("~~", CFAest_ord_T$op))
  # Look at results
  CFAest_comp <- cbind(CFAest[CFA_cont_index, 1:3],
                       cont = round(CFAest[CFA_cont_index, 4], 3),
                       ord_T  = round(CFAest_ord_T[CFA_ordi_index, 4], 3),
                       ord_F  = round(CFAest_ord_F[CFA_cont_index, 4], 3),
                       goal = round(CFAest_goal[CFA_cont_index, 4], 3))
  CFAest_comp

# compare results ---------------------------------------------------------

  # Factor loadings comapred
  CFAest_comp[, "cont"]/CFAest_comp[, "ord_F"]

  # Item variances
  cbind(cont = round(apply(dt, 2, var), 3),
        ord  = round(apply(dt_ord, 2, var), 3),
        goal = round(apply(dt_goal, 2, var), 3))

  # Observed Correlations
  lapply(list(cont = dt,
              ord  = dt_ord,
              goal = dt_goal), 
         function(x){round(cor(x), 3)})