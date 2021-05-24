### Title:    Defining Fixed Parameters
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-12

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
  source("./helper/fun_discretize.R")
  source("./helper/simMissingness.R")
  source("./helper/functions.R")
  
  # Imputation Functions
  source("./fun_impute/fun_PCA_impute.R")

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()
  
  # Data generation
  parms$N <- 1e3 # sample size
  parms$L <- 18 # number of latent variables
  parms$J <- 3 # number of measured items for latent variable
  parms$K <- 5 # number of categories for ordinal variables
  parms$P <- parms$L*parms$J # number of latent variables
  parms$pm <- .2 # proportion of missings level
  parms$fl <- .8 # factor loadings level
  parms$lv_mean   <- 0 # true latent mean
  parms$lv_var    <- 1 # true latent variance
  parms$lv_cov_ta <- .8 # true latent cov for target variables
  parms$lv_cov_mp <- .8 # for mar predictors
  parms$lv_cov_ax <- .8 # for auxiliary set
  parms$item_mean <- 5 # true item mean
  parms$item_var  <- 1 # true item variance
  
  # Map variables
  parms$varMap <- list(ta = 1:2,  # TArget of analysis
                       mp = 3:5, # Mar Predictors
                       ax = 6:parms$L # Auxiliary variables
  )

# Experimental Conditions -------------------------------------------------

  # Define Experimental Factor Values
  n_ord <- c(0, 1/3, 2/3, 1)
  n_bin <- c(0, 1/3, 2/3, 1)
  p_junk <- c(0, 1/3, 2/3, 1)
  
  # Make Conditionsa
  conds <- expand.grid(N  = parms$N,
                       L  = parms$L,
                       J  = parms$J,
                       P  = parms$L * parms$J,
                       pm = parms$pm, 
                       fl = parms$fl,
                       p_junk = p_junk,
                       n_ord = n_ord,
                       n_bin = n_bin,
                       stringsAsFactors = FALSE)
  
  # Get rid of impossible combinations of n_ord and n_bin
  conds <- conds[rowSums(conds[, c("n_ord", "n_bin")]) <= 1, ]
  
  # Overwrite rownames
  rownames(conds) <- 1:nrow(conds)
  
  # Print
  conds
  
  # Parallel Experiments: for the continuous and attenuated relationship
  # Take to the extreme
  # Directly find second der at each point and select max
