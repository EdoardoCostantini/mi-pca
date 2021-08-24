### Title:    helper functions
### Project:  Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-05-19
### Modified: 2021-07-21
### Note:     This script contains functions of level-1 which are functions
###           that cannot contain other functions I defined). This is the
###           lower functional level. I can use functions defined by other
###           pacakges, but not other functions I have written for this
###           project. If I wanted to include them I would then be working
###           with a subroutine.

# Estimate Descriptives ---------------------------------------------------

satModWrite <- function(var_id){
  ## Description:
  # Given a set of variable names it returns a character vector containing
  # the model description that estiamtes the mean, variance, and covariance
  # of all variables (when given to a lavaan::sem() function
  ## Example input:
  # var_id <- paste0("z", parms$vmap_it$ta)

  # Means
  head_means <- "# Means\n"
  all_means <- paste0(var_id, " ~ ", "1")
  all_means <- paste(all_means, collapse = "\n")

  # Variances
  head_vars <- "# Variances \n"
  all_vars <- paste0(var_id, " ~~ ", var_id)
  all_vars <- paste(all_vars, collapse = "\n")

  # Coivariances
  head_covs <- "# Covariances \n"
  all_covs <- combn(var_id, 2)
  all_covs <- apply(all_covs, 2, paste0, collapse = " ~~ ")
  all_covs <- paste(all_covs, collapse = "\n")

  # Put together
  SAT_mod <- paste(head_means, all_means,
                   head_vars, all_vars,
                   head_covs, all_covs
  )

  return(SAT_mod)
}