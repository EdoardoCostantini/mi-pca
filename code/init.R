### Title:    Defining Fixed Parameters
### Project:  Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2021-01-29

# Packages ----------------------------------------------------------------

pack_list <- c("parallel",
               "MASS")

lapply(pack_list, library, character.only = TRUE)

# Load Functions ----------------------------------------------------------

# # Simulation
# source("./subroutines.R")
# 
# # Support Functions
# source("./helper/simMissingness.R")
# 
# # Imputation Functions
# source("./fun_impute/fun_PCA_impute.R")

# Simulation Studi Fixed Parameters  --------------------------------------

# Empty List
parms    <- list()

# Data generation
parms$N <- 1e3
parms$L <- 18    # number of latent variables
parms$J <- 3
parms$P <- parms$L*parms$J    # number of latent variables
parms$pm <- .2    # proportion of missings level
parms$fl <- .8 # factor loadings level
parms$lv_mean   <- 0 # true latent mean
parms$lv_var    <- 1 # true latent variance
parms$item_mean <- 5 # true item mean
parms$item_var  <- 1 # true item variance

# Map variables
parms$latentMap <- list(target = 1:2,  # target of analysis
                        aux_marpre = 3:5, # MAR predictors
                        aux_other = 6:parms$L # other predicotrs
)
