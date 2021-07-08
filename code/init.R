### Title:    Defining Fixed Parameters
### Project:  imputeHD-add
### Author:   Edoardo Costantini
### Created:  2021-06-23
### Modified: 2021-06-21

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan")
  
  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Functions ----------------------------------------------------------

  # Simulation
  # source("./subroutines.R")

  # Support Functions
  source("./helper/fun_dataGen.R")
  source("./helper/fun_imposeNA.R")
  source("./helper/simMissingness.R")
  source("./helper/functions.R")
  source("./helper/subroutines.R")
  
  # Imputation Functions
  source("./fun_impute/fun_PCA_impute.R")

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()
  
  # Data generation
  parms$N <- 1e3 # sample size
  parms$L <- 9 # number of latent variables
  parms$J <- 3 # number of measured items for latent variable
  parms$P <- parms$L*parms$J # number of latent variables
  parms$pm <- .2 # proportion of missings level
  parms$fl <- .8 # factor loadings level
  parms$fl_bound <- 0 # factor loadings level
  parms$lv_mean   <- 0 # true latent mean
  parms$lv_var    <- 1 # true latent variance
  parms$lv_cov_ta <- .8 # true latent cov for target variables
  parms$lv_cov_mp <- .8 # for mar predictors
  parms$lv_cov_ax <- .8 # for good auxiliary
  parms$lv_cov_junk <- .1 # for junk auxiliary
  parms$item_mean <- 0 # true item mean
  parms$item_var  <- 1 # true item variance
  
  # Map variables
  parms$varMap <- list(ta = 1:2, # TArget of analysis
                       mp = 3, # Mar Predictors
                       ax = 4:parms$L, # Auxiliary variables
                       disc_pool = 2:parms$P
  )

# Experimental Conditions -------------------------------------------------
  
  # Parallel Experiments: for the continuous and attenuated relationship
  # Alternative experimental factor
  K <- c(10, 7, 5, 3, 2) # number of categories
  D <- seq(1, 0, length.out = 5)
  interval <- c(TRUE, FALSE)
  pj <- round(seq(1, 0, length.out = 4), 2) # proportion of junk variables
  npcs <- c(1, 5, parms$N*(1 - .4)) # number of PCs extracted

  # Make Conditionsa
  conds <- expand.grid(K = K,
                       D = D,
                       interval = interval,
                       pj = pj,
                       npcs = npcs,
                       stringsAsFactors = FALSE)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    paste0(colnames(conds), conds[i, ], collapse = "_")
  }
  )