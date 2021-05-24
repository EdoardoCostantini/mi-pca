### Title:    Data Discretization Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-24

disData <- function(dat, parms, cond){

# Example Input -----------------------------------------------------------

  # cond = conds[6, ]
  # dat = genData(parms, cond)$dat_ob
  
# Break Input data in parts -----------------------------------------------

  n_lv <- length(parms$varMap$ta)
  n_mp <- length(parms$varMap$mp)
  
  dat_ta <- dat[, 1:(n_lv*parms$J)]
  dat_mp <- dat[, (n_lv * parms$J + 1):(n_lv * parms$J + n_mp * parms$J)]
  dat_ax <- dat[, (n_lv * parms$J + n_mp * parms$J+1):ncol(dat)]
  
# Transform MAR predictors according to condition -------------------------

  n_ord <- ncol(dat_mp)*cond$n_ord
  n_bin <- ncol(dat_mp)*cond$n_bin
  n_con <- ncol(dat_mp)-(n_ord+n_bin)
  var_type <- c(cont = rep(1, n_con), 
                ord = rep(2, n_ord),
                bin = rep(3, n_bin))
  dat_mp_out <- sapply(1:ncol(dat_mp), function(x){
    disVar(dat_mp[, x], var_type = var_type[x], parms)
  })
  
# Transform Auxiliary part according to condition -------------------------

  n_ord <- ncol(dat_ax)*cond$n_ord
  n_bin <- ncol(dat_ax)*cond$n_bin
  n_con <- ncol(dat_ax)-(n_ord+n_bin)
  var_type <- c(cont = rep(1, n_con), 
                ord = rep(2, n_ord),
                bin = rep(3, n_bin))
  dat_ax_out <- sapply(1:ncol(dat_ax), function(x){
    disVar(dat_ax[, x], var_type = var_type[x], parms)
  })

  # Form output data set
  dat_out <- cbind(dat_ta, dat_mp_out, dat_ax_out)
  colnames(dat_out) <- colnames(dat)
  
# Return Output -----------------------------------------------------------
  return(dat_out)
}
