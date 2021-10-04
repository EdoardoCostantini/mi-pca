# Project:   mipca_compare
# Objective: function to obtain par estimates with single data methods
# Author:    Edoardo Costantini
# Created:   2021-09-21
# Modified:  2021-10-04

fitSingleData <- function (dt, targets){
  ## Inputs
  # dt = dat$cont
  # targets = parms$vmap$ta

  ## Body
  tryCatch({
    ### START TRYCATCH EXPRESSION
    fit <- sem(model = genLavaanMod(dt,
                                    targets = targets),
               data = dt)
    fit_parms <- parameterEstimates(fit)

    ## Store name of parameter
    fit_parms <- cbind(par = apply(fit_parms[, 1:3],
                                   1,
                                   paste0, collapse = ""),
                       fit_parms)

    ## Drop useless column
    fit_parms <- fit_parms[, c("par", "est", "ci.lower", "ci.upper")]

    ## Change names
    colnames(fit_parms) <- c("par", "Q_bar", "lwr", "upr")

    ## Correlations
    var_names <- colnames(dt[, targets])
    all_cors <- t(combn(var_names, 2))
    store <- data.frame(matrix(NA, ncol = 4, nrow = nrow(all_cors)))
    for (r in 1:nrow(all_cors)){
      y <- dt[, all_cors[r, 1]]
      x <- dt[, all_cors[r, 2]]
      cor_test <- cor.test(y, x)
      store[r, 1] <- paste0(all_cors[r, 1], "r", all_cors[r, 2]) # id
      store[r, -1] <- c(cor_test$estimate, cor_test$conf.int) # data
    }

    # Give proper names
    colnames(store) <- c("par", "Q_bar", "lwr", "upr")

    ## Output
    # Join results
    out <- rbind(fit_parms, store)

    return(out)

    ### END TRYCATCH EXPRESSION
  }, error = function(e){
    err <- paste0("Original Error: ", e)
    print(err)
    return(NULL)
  }
  )

}
