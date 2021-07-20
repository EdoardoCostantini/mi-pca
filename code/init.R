### Title:    Defining Fixed Parameters
### Project:  imputeHD-add
### Author:   Edoardo Costantini
### Created:  2021-06-23
### Modified: 2021-06-21

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan",
                 "mice",
                 "FactoMineR")
  
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
  parms$J <- 4 # number of measured items for latent variable
  parms$P <- parms$L*parms$J # number of latent variables
  parms$pm <- .5 # proportion of missings level
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
  parms$varMap <- list(ta = 1, # TArget of analysis
                       mp = 2, # Mar Predictors
                       ax = 3:parms$L # Auxiliary variables
  )
  ta <- 1:(max(parms$varMap$ta)*parms$J)
  mp <- (max(ta)+1):(max(ta)+(parms$J*length(parms$varMap$mp)))
  ax <- (max(mp)+1):parms$P
  parms$varMap_items <- list(ta = ta, mp = mp, ax = ax)

  # CFA model
  lv_items <- split(x = paste0("z", 1:(length(parms$varMap$ta)*parms$J)),
                    f = rep(parms$varMap$ta, each = parms$J))
  lv_models <- sapply(1:length(lv_items), function(it){
    paste0("l", it,
           " =~ ",
           paste0(lv_items[[it]], collapse = " + ")
    )
  }
  )
  parms$CFA_model <- paste(lv_models, collapse = "\n")

  # Imputation Routine
  parms$mice_ndt <- 2
  parms$mice_iters <- 5

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