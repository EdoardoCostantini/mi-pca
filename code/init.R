# Project:   mipca_compare
# Objective: initialization script
# Author:    Edoardo Costantini
# Created:   2021-06-23
# Modified:  2021-09-28

# Packages ----------------------------------------------------------------

  pack_list <- c("parallel",    # simulation paralleliazion
                 "rlecuyer",    # simulation paralleliazion
                 "MASS",        # data generation
                 "lavaan",      # fitting analysis models
                 "mice",        # imputation
                 "mice.pcr.sim",# imputation
                 "FactoMineR",
                 "stringr",     # result pooling
                 "ggplot2",     # results analysis
                 "dplyr",       # results analysis
                 "forcats",     # results analysis
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
  parms$P <- 50 # number of variables (100 target)
  parms$pm <- .3 # proportion of missings level
  parms$cov_ta <- .7 # true latent cov for target variables
  parms$cov_mp <- .7 # for mar predictors
  parms$cov_ax <- .7 # for good auxiliary
  parms$cov_junk <- .1 # for junk auxiliary
  parms$item_mean <- 5 # 5 # true item mean
  parms$item_var  <- (2.5)^2 # true item variance
  
  # Map variables
  parms$vmap <- list(ta = 1:4, # TArget of analysis
                     mp = 5:8, # Mar Predictors
                     ax = 9:parms$P # Auxiliary variables
  )

  # Imputation Routine
  parms$mice_ndt <- 5
  parms$mice_iters <- 10

  # Results and replicability
  parms$rps      <- 100
  parms$seed     <- 20210929
  parms$nStreams <- 1000
  parms$outDir   <- "../output/"

# Experimental Conditions -------------------------------------------------
  
  # Parallel Experiments: for the continuous and attenuated relationship
  # Alternative experimental factor
  K <- 5 # c(Inf, 7, 5, 3, 2) # number of categories
  D <- 1 # seq(1, 0, length.out = 5)
  interval <- TRUE # c(TRUE, FALSE)
  pj <- .67 # round(seq(1, 0, length.out = 4), 2) # proportion of junk variables
  npc <- c(1, # min
           # 5, # low range forced
           # seq(20, (parms$P-length(parms$vmap$ta)), 30), # granularity in high range
           "max") # max
  method <- c("all", "aux", "vbv", "MITR", "MIMI", "CC", "OG") # nature of PC

  # Make Conditionsa
  conds_1 <- expand.grid(K = K,
                         D = D,
                         interval = interval,
                         pj = pj,
                         npc = npc,
                         method = method[1:3],
                         stringsAsFactors = FALSE)

  conds_2 <- expand.grid(K = K,
                         D = D,
                         interval = interval,
                         pj = pj,
                         npc = npc[1],
                         method = method[-c(1:3)],
                         stringsAsFactors = FALSE)

  conds <- rbind(conds_1, conds_2)

  # Append Condition Tag
  conds <- cbind(tag = sapply(1:nrow(conds),
                              function(i) {
                                paste0(colnames(conds), conds[i, ], collapse = "_")
                              }),
                 conds)