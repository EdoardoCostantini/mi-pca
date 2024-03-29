# Project:   mipca_compare
# Objective: initialization script
# Author:    Edoardo Costantini
# Created:   2021-06-23
# Modified:  2022-09-11

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
                 "grDevices")   # for plotting gray.colors function

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
  parms$largeP <- list( # add extra items?
    final     = FALSE,
    convCheck = FALSE,
    trial     = FALSE,
    supHD     = TRUE
  )[[parms$run_type]]
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
  parms$mice_iters <- list(
    final     = 20,
    convCheck = 100,
    trial     = 10,
    supHD     = 20
  )[[parms$run_type]]

  parms$seed     <- 20220911
  parms$nStreams <- 1000
  parms$outDir   <- "../output/"

# Experimental Conditions -------------------------------------------------

  # Number of categories
  K <- list(
    final     = c(Inf, 7, 5, 3, 2),
    convCheck = c(7, 2),
    trial     = c(Inf, 2),
    supHD = c(Inf, 5, 2)
  )[[parms$run_type]]

  # Proportion of discretized variables
  D <- list(
    final     = 1,
    convCheck = 1,
    trial     = 1, # seq(1, 0, length.out = 5)
    supHD = 1
  )[[parms$run_type]]

  # Intraval scale
  interval <- list(
    final     = TRUE,
    convCheck = TRUE,
    trial     = TRUE,
    supHD = TRUE
  )[[parms$run_type]]

  # Proportion of junk variables
  pj <- list(
    final     = round(seq(0, 1, length.out = 4), 2),
    convCheck = c(0, 1),
    trial     = round(seq(0, 1, length.out = 3), 2),
    supHD     = c(0, 1/3, 2/3, 1)
  )[[parms$run_type]]

  # Latent Structure
  lv <- list(
    final     = c(TRUE, FALSE),
    convCheck = c(TRUE),
    trial     = c(TRUE, FALSE),
    supHD     = c(TRUE)
  )[[parms$run_type]]

  # Number of components to extract
  npc <- list(
    final     = c(1:10, 20, 25, "max"),
    convCheck = c(1, "max"),
    trial     = c(1, 20, "max"),
    supHD     = c(1:10)
  )[[parms$run_type]]

  # Methods
  method <- list(
    # final     = c("all", "aux", "vbv", "MIOP", "MIOR", "MIMI", "CC", "OG"),
    final     = c("all", "all_oracle", "aux", "vbv", "MIOP", "MIOR", "MIMI", "CC", "OG"),
    convCheck = c("all", "aux", "vbv", "MIOP", "MIOR", "MIMI"),
    trial     = c("all", "all_oracle", "aux", "vbv", "MIOP", "MIOR", "MIMI", "CC", "OG"),
    supHD     = c("all", "aux", "vbv", "MIOP", "MIOR", "MIMI", "CC", "OG")
  )[[parms$run_type]]

  # Make Conditionsa
  conds_1 <- expand.grid(K = K,
                         D = D,
                         interval = interval,
                         pj = pj,
                         npc = npc,
                         method = intersect(c("all", "all_oracle", "aux", "vbv"), method),
                         lv = lv,
                         stringsAsFactors = FALSE)

  conds_2 <- expand.grid(K = K,
                         D = D,
                         interval = interval,
                         pj = pj,
                         npc = 0,
                         method = intersect(c("MIOP", "MIOR", "MIMI",
                                              "CC", "OG"),
                                            method),
                         lv = lv,
                         stringsAsFactors = FALSE)

  conds <- rbind(conds_1, conds_2)

  # Append Condition Tag
  conds <- cbind(tag = sapply(1:nrow(conds),
                              function(i) {
                                paste0(colnames(conds), conds[i, ], collapse = "_")
                              }),
                 conds)

  # Make character vectors factors
  conds <- lapply(conds, function (j){
    if(is.character(j)){
      factor(j, levels = unique(j))
    } else {
      j
    }
  })
  conds <- as.data.frame(conds)