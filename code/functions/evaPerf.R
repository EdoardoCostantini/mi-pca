# Project:   mipca_compare
# Objective: Function to compute bias, coverage, and CIW
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-11-10

evaPerf <- function (results, sInfo){

# Get rid of failed rows
  to_remove <- sapply(results[, c("Q_bar", "lwr", "upr")], function (j){
    which(is.nan(j))
  })
  if(length(unique(unlist(to_remove))) != 0){
    results <- results[-unique(unlist(to_remove)), ]
  }

# Transform npc = max to appropriate number
  npc_temp <- as.character(results$npc)
  max_position <- npc_temp %in% "max"
  npc_temp[max_position & results$method == "all"] <- 56
  npc_temp[max_position & results$method == "all_oracle"] <- 56
  npc_temp[max_position & results$method == "aux"] <- 52
  npc_temp[max_position & results$method == "vbv"] <- 55
  results$npc <- factor(npc_temp)

# Cast discrete experimental factors to r factors
  results$K <- factor(results$K, levels = rev(unique(results$K)))
  results$par <- factor(results$par, levels = unique(results$par))

# Define "True" values
  ref_grouping <- c("K", "D", "interval", "pj", "par", "lv")
  ref_df <- data.frame(results %>%
                         filter(method == "OG") %>%
                         group_by_at(ref_grouping) %>%
                         dplyr::summarize(ref = mean(Q_bar)))

  # Attach reference value based on matching par to original dataset
  results_ref <- merge(x = results,
                       y = ref_df,
                       by = ref_grouping,
                       all.x = TRUE)
  results_ref <- arrange(results_ref, rp, tag, par)

# Bias Computation
  comp_grouping <- c("K", "D", "interval", "pj", "npc", "method", "par", "lv")
  bias_df <- data.frame(results_ref %>%
                          group_by_at(comp_grouping) %>%
                          dplyr::summarize(Mean = mean(Q_bar),
                                           mcsd = sd(Q_bar),
                                           PC_exp = mean(PC_exp))
  )
  bias_df <- merge(x = bias_df, y = ref_df, by = ref_grouping)
  bias_df$bias <- round(abs(bias_df$Mean - bias_df$ref) / bias_df$ref*100, 3)
  bias_df <- arrange_at(bias_df, comp_grouping)

# Coverage
  results_ref$cover_log <- results_ref$lwr < results_ref$ref & results_ref$ref < results_ref$upr
  CIC <- data.frame(results_ref %>%
                      group_by_at(comp_grouping) %>%
                      dplyr::summarize(coverage = mean(cover_log, na.rm = TRUE)))

# Confidence interval width
  results_ref$CIW <- results_ref$upr - results_ref$lwr
  CIW <- data.frame(results_ref %>%
                      group_by_at(comp_grouping) %>%
                      dplyr::summarize(CIW = mean(CIW)))
  upr_avg <- data.frame(results_ref %>%
                          group_by_at(comp_grouping) %>%
                          dplyr::summarize(upr_avg = mean(upr)))
  lwr_avg <- data.frame(results_ref %>%
                          group_by_at(comp_grouping) %>%
                          dplyr::summarize(lwr_avg = mean(lwr)))

# Merge all
  res <- cbind(bias_df,
               CIC = round(CIC$coverage, 3),
               CIW = round(CIW$CIW, 3),
               lwr_avg = lwr_avg$lwr_avg,
               upr_avg = upr_avg$upr_avg)
  return(res)
}