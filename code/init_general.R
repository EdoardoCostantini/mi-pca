### Title:    Initialization scirpt, general (functions and packages)
### Project:  Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2021-05-12

# Packages ----------------------------------------------------------------

pack_list <- c("tidyverse",
               "CVTuningCov",
               "mvtnorm",
               "monomvn",
               "glmnet",
               "rlecuyer",
               "parallel",
               "caret", 
               "missForest",
               "truncnorm",
               "lavaan",
               "FactoMineR",
               "devtools",
               "xtable",
               "rstan",
               "gridExtra",
               "grid",
               "plyr", # for round_any
               "plot.matrix", # for missing data pattern plot
               "xtable",
               "PcAux",
               "blasso")

lapply(pack_list, library, character.only = TRUE)

# Load Functions ----------------------------------------------------------

source("./subroutines.R")

# Support Functions
source("./helper/simMissingness.R")
source("./helper/functions.R")
source("./helper/functions_genDt.R")
source("./helper/functions_EVS.R")

# Imputation Functions
source("./fun_impute/fun_DURR_impute.R")
source("./fun_impute/fun_IURR_impute.R")
source("./fun_impute/fun_blasso_impute.R")
source("./fun_impute/fun_bridge_impute.R")
source("./fun_impute/fun_PCA_impute.R")
source("./fun_impute/fun_MICE_TR_impute.R")
source("./fun_impute/fun_RANF_impute.R")
source("./fun_impute/fun_CART_impute.R")
source("./fun_impute/fun_missFor_impute.R")
