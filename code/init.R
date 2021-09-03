# Project:   mipca_compare
# Objective: initialization script
# Author:    Edoardo Costantini
# Created:   2021-06-23
# Modified:  2021-08-26

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",
                 "MASS",
                 "lavaan",
                 "mice",
                 "FactoMineR",
                 "OrdNor",
                 "BinNor")
  
  lapply(pack_list, library, character.only = TRUE, verbose = FALSE)

# Load Functions ----------------------------------------------------------

  # Subroutines
  all_subs <- paste0("./subroutines/",
                     list.files("./subroutines/"))
  lapply(all_subs, source)

  # Functions
  all_funs <- paste0("./functions/",
                     list.files("./functions/"))
  lapply(all_funs, source)

  # Helper
  all_help <- paste0("./helper/",
                     list.files("./helper/"))
  lapply(all_help, source)

# Fixed Parameters --------------------------------------------------------

  # Empty List
  parms    <- list()
  
  # Data generation
  parms$N <- 1e3 # sample size
  parms$P <- 10 # number of variables
  parms$pm <- .3 # proportion of missings level
  parms$cov_ta <- .7 # true latent cov for target variables
  parms$cov_mp <- .7 # for mar predictors
  parms$cov_ax <- .7 # for good auxiliary
  parms$cov_junk <- .1 # for junk auxiliary
  parms$item_mean <- 5 # 5 # true item mean
  parms$item_var  <- (2.5)^2 # true item variance
  
  # Map variables
  parms$vmap <- list(ta = (parms$P-3):parms$P, # TArget of analysis
                     mp = (parms$P-7):(parms$P-4), # Mar Predictors
                     ax = 1:(parms$P-8) # Auxiliary variables
  )

  # Imputation Routine
  parms$mice_ndt <- 2
  parms$mice_iters <- 5

  # Storing Objects
  parms$outDir <- "../output/"

# Experimental Conditions -------------------------------------------------
  
  # Parallel Experiments: for the continuous and attenuated relationship
  # Alternative experimental factor
  K <- c(10, 7, 5, 3, 2) # number of categories
  D <- 1 # seq(1, 0, length.out = 5)
  interval <- c(TRUE)
  pj <- round(seq(1, 0, length.out = 4), 2) # proportion of junk variables
  npc <- c(1, 5, parms$N*(1 - .4)) # number of PCs extracted
  fpc <- c("all", "imp", "vbv") # nature of PC

  # Make Conditionsa
  conds <- expand.grid(K = K,
                       D = D,
                       interval = interval,
                       pj = pj,
                       npc = npc,
                       fpc = fpc,
                       stringsAsFactors = FALSE)

  # Append Condition Tag
  conds$tag <- sapply(1:nrow(conds), function(i) {
    paste0(colnames(conds), conds[i, ], collapse = "_")
  }
  )