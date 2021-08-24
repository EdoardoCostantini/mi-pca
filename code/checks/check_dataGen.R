### Title:    Data Generation Checks
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-07-20
### Modified: 2021-07-21

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
  dat_ob_numeric <- sapply(dat_list$dat_ob[, parms$vmap_it$mp], as.numeric)
  cor4mar <- cor(cbind(dat_list$dat_ob[, parms$vmap_it$ta],
                       lv_mp = dat_list$dat_lv[, parms$vmap_lv$mp],
                       ob_mp = dat_ob_numeric))
  store_cors_lv <- rbind(store_cors_lv, cor4mar[parms$vmap_it$ta, "lv_mp"])
  store_cors_ob[[i]] <- cor4mar[parms$vmap_it$ta,
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

# Factor Loadings Generated and recovered ---------------------------------

  rm(list=ls())
  source("./init.R")

  set.seed(20210127)
  cond <-  conds[446, ] # condition with continuous variables
  cond <-  conds[2, ] # condition with continuous variables

  res <- NULL
  for (i in 1:nrow(conds)){
    cond <- conds[i, ]
    for(rep in 1:10){
      print(paste0("Cond: ", i, " Rep: ", rep))
      # Gen data
      dat_list <- genData(parms = parms, cond = cond)
      Xy <- dat_list$dat_ob

      # Define CFA model
      n_items <- length(c(parms$vmap_lv$ta, parms$vmap_lv$mp))
      lv_items <- split(x = paste0("z", 1:(n_items*parms$J)),
                        f = rep(c(parms$vmap_lv$ta, parms$vmap_lv$mp),
                                each = parms$J))
      lv_models <- sapply(1:length(lv_items), function(it){
        paste0("l", it,
               " =~ ",
               paste0(lv_items[[it]], collapse = " + ")
        )
      }
      )
      CFA_model <- paste(lv_models, collapse = "\n")

      # Fit CFA model and check factor loadings
      if("factor" %in% sapply(Xy[, unlist(lv_items)], class)){
        temp_index <- which(sapply(Xy[, unlist(lv_items)], class) %in% "factor")
        ordered_items <- colnames(Xy[, temp_index, drop = FALSE])
      } else {
        ordered_items <- NULL
      }

      fit <- cfa(CFA_model, data = Xy,
                 ordered = ordered_items,
                 std.lv = TRUE)

      # Extract Parameter estiamtes
      CFA_par <- parameterEstimates(fit,
                                    se = FALSE, zstat = FALSE,
                                    pvalue = FALSE, ci = FALSE,
                                    standardized = TRUE)
      par_all <- cbind(par = apply(CFA_par[, 1:3], 1, paste0, collapse = ""),
                       CFA_par[, c(6), drop = FALSE])

      # Store Results
      rep_res <- c(fal = mean(par_all[grep("=~", par_all$par), "std.all"]),
                   erv = mean(par_all[grep("z\\d~~", par_all$par), "std.all"]),
                   lcv = par_all[grep("l1~~l2", par_all$par), "std.all"])
      res <- rbind(res, data.frame(c(cond, rep_res)))
    }
  }

summar <- res %>%
  group_by(tag) %>%
  dplyr::summarize(fal = mean(fal),
                   erv = mean(erv),
                   lcv = mean(lcv))

lapply(summar[, -1], function (x) boxplot(x))