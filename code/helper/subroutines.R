### Title:    helper functions
### Project:  Imputing High Dimensional Data
### Author:   Edoardo Costantini
### Created:  2020-06-21
### Modified: 2020-06-21
### Note:     This script contains functions of level-2 (contain functions
###           of level-1)

# Subroutine to generate all data for a condition -------------------------

data_step <- function(parms, cond){

  # Generate Continuous Data
  dat_list <- genData(parms = parms, cond = cond, fl = parms$fl)
  dat_cont <- dat_list$dat_ob

  # Discretise it
  index_keep_continuous <- 1:(max(parms$varMap$ta)*parms$J)
  dat_disc <- apply(dat_cont[, -index_keep_continuous],
                    2,
                    function(j){
                      as.numeric(cut(j, breaks = cond$K))
                    })
  dat_disc <- cbind(dat_cont[, index_keep_continuous], dat_disc)

  # Generate Continuous Data w/ attenuated relationships
  # Define CFA model text object
  ids_items <- split(x = colnames(dat_list$dat_ob),
                     f = rep(1:(ncol(dat_list$dat_ob)/parms$J),
                             each = parms$J))
  ids_lv <- colnames(dat_list$dat_lv)
  names(ids_items) <- ids_lv

  lv_models <- sapply(1:length(ids_items), function(it){
    paste0(ids_lv[it],
           " =~ ",
           paste0(ids_items[[it]], collapse = " + ")
    )

  })
  CFA_model <- paste(lv_models, collapse = "\n")

  # Fit CFA model on Categorical (scaled) data
  CFA_fit_d <- cfa(CFA_model,
                   data = scale(dat_disc),
                   std.lv = TRUE)
  CFA_par_d <- parameterEstimates(CFA_fit_d,
                                  se = FALSE, zstat = FALSE,
                                  pvalue = FALSE, ci = FALSE,
                                  standardized = TRUE)
  fl_atte <- mean(CFA_par_d[1:ncol(dat_disc), "est"])

  # Generate counterfactual continous data
  dat_list_atte <- genData(parms = parms, cond = cond, fl = fl_atte)
  dat_atte <- dat_list_atte$dat_ob

  return(
    list(dat = list(cont = dat_cont,
                    disc = dat_disc,
                    atte = dat_atte),
         CFA_model = CFA_model)
  )

}
