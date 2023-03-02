# Project:   mipca_compare
# Objective: initialization script
# Author:    Edoardo Costantini
# Created:   2023-03-02
# Modified:  2023-03-02

# Packages ----------------------------------------------------------------

  cran_list <- c("parallel",    # simulation paralleliazion
                 "rlecuyer",    # simulation paralleliazion
                 "MASS",        # data generation
                 "lavaan",      # fitting analysis models
                 "mice",        # imputation
                 "FactoMineR",
                 "stringr",     # result pooling
                 "ggplot2",     # results analysis
                 "dplyr",       # results analysis
                 "forcats",     # results analysis
                 "miceadds",    # results analysis
                 "OrdNor",
                 "BinNor",
                 "testthat",
                 "devtools",    # for detailed session info
                 "grDevices",   # for plotting gray.colors function
                 "nFactors"     # for non-graphical solutions to the scree plot
                 )

  local_list <- c("mice.pcr.sim")
  local_list_location <- c("../input/")

  # Put together
  pack_list <- c(cran_list, local_list)

  # Load packages
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

  # Run type
  parms$run_type <- c(final = 1,
                      convCheck = 2,
                      trial = 3,
                      supHD = 4)[4]

  # Data generation
  parms$N <- 500 # sample size
  parms$L <- 7
  parms$P <- 56 # number of variables
  parms$largeP <- c(TRUE, FALSE)
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

  # Storing objects
  parms$seed     <- 20230302
  parms$nStreams <- 1000
  parms$outDir   <- "../output/"

# Experimental Conditions -------------------------------------------------

  # Number of categories
  K <- c(Inf, 7, 5, 3, 2)

  # Proportion of discretized variables
  D <- 1

  # Intraval scale
  interval <- TRUE

  # Proportion of junk variables
  pj <- round(seq(0, 1, length.out = 4), 2)

  # Latent Structure
  lv <- c(TRUE, FALSE)

  # Number of variables
  P <- c(56, 242)

  # Make Conditions
  conds <- expand.grid(
    P = P,
    K = K,
    D = D,
    interval = interval,
    pj = pj,
    lv = lv,
    stringsAsFactors = FALSE
  )

  # Append Condition Tag
  conds <- cbind(
    tag = sapply(
      1:nrow(conds),
      function(i) {
        paste0(colnames(conds), conds[i, ], collapse = "_")
      }
    ),
    conds
  )

  # Make character vectors factors
  conds <- lapply(conds, function(j) {
    if (is.character(j)) {
      factor(j, levels = unique(j))
    } else {
      j
    }
  })

  # Revert to data.frame
  conds <- as.data.frame(conds)