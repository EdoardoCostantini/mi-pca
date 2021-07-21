### Title:    Data Generation Checks
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-07-20

## Make sure we have a clean environment:
rm(list = ls())

## Initialize the environment:
source("./init.R")

# Correlation btw mar latent and missing items stays the same -------------
store_cors_lv <- NULL
store_cors_ob <- NULL

for (i in 1:25){
  print(i)
  dat_list <- genData(parms = parms, cond = conds[i, ])
  dat_ob_numeric <- sapply(dat_list$dat_ob[, parms$varMap_items$mp], as.numeric)
  cor4mar <- cor(cbind(dat_list$dat_ob[, parms$varMap_items$ta],
                       lv_mp = dat_list$dat_lv[, parms$varMap$mp],
                       ob_mp = dat_ob_numeric))
  store_cors_lv <- rbind(store_cors_lv, cor4mar[parms$varMap_items$ta, "lv_mp"])
  store_cors_ob[[i]] <- cor4mar[parms$varMap_items$ta,
                                grep("ob", colnames(cor4mar))]
}

# What conditions to check
to_check <- c(4, 9, 14, 19, 24)
conds[to_check, ]

# Trend when using latent variable as MAR predictor
store_cors_lv[to_check, ]
apply(store_cors_lv, 2, mean)
apply(store_cors_lv, 2, sd)

# Trend when using the observed items as predictors
mean_mat <- Reduce('+', store_cors_ob)/nrow(conds)
list_sub <- lapply(store_cors_ob, function (x){(x-mean_mat)^2})
sum <- sqrt((Reduce('+', list_sub))/nrow(conds))

conds[to_check, ]
store_cors_ob[to_check]
  # Correlation between observed items and items recieving missingness
  # change due to the discreteness factor. Therefore, we cannot use it
  # because it would lead to an effect on the outcomes (attenutaion of MAR,
  # gets closer to MCAR, imputaion matters less)
