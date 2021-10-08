# Project:   mipca_compare
# Objective: Function to compute bias, coverage, and CIW
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-10-07

evaPerf <- function (out, output){
  # Cast experimental factors to ordered factors
  out$npc[out$npc == "max"] <- output$sInfo$parms$P
  out$npc <- as.numeric(out$npc)
  out$K <- factor(out$K, levels = rev(unique(out$K)), ordered = TRUE)
  out$tag <- factor(out$tag, levels = unique(out$tag), ordered = TRUE)
  out$par <- factor(out$par, levels = unique(out$par), ordered = TRUE)
  out$method <- factor(out$method,
                       levels = unique(output$sInfo$conds$method),
                       ordered = TRUE)

  # True values
  ref_grouping <- c("K", "D", "interval", "pj", "par")
  ref_df <- data.frame(out %>%
                         filter(method == "OG") %>%
                         group_by_at(ref_grouping) %>%
                         dplyr::summarize(ref = mean(Q_bar)))

  ## Add based on matching par to original dataset
  out_ref <- merge(x = out,
                   y = ref_df,
                   by = ref_grouping,
                   all.x = TRUE)
  out_ref <- arrange(out_ref, rp, tag, par)

  # Bias Computation
  comp_grouping <- c("K", "D", "interval", "pj", "npc", "method", "par")
  bias_df <- data.frame(out_ref %>%
                          group_by_at(comp_grouping) %>%
                          dplyr::summarize(Mean = mean(Q_bar),
                                           PC_exp = mean(PC_exp))
  )
  bias_df <- merge(x = bias_df, y = ref_df, by = ref_grouping)
  bias_df$bias <- round(abs(bias_df$Mean - bias_df$ref) / bias_df$ref*100, 3)
  bias_df <- arrange_at(bias_df, comp_grouping)

  # Confidence interval coverage for a given method across other factors
  # In or out?
  # Check if the confidence interval contains them
  out_ref$cover_log <- out_ref$lwr < out_ref$ref & out_ref$ref < out_ref$upr
  CIC <- data.frame(out_ref %>%
                      group_by_at(comp_grouping) %>%
                      dplyr::summarize(coverage = mean(cover_log)))

  # Confidence interval width for a given method across other factors
  out_ref$CIW <- abs(out_ref$lwr - out_ref$upr)
  CIW <- data.frame(out_ref %>%
                      group_by_at(comp_grouping) %>%
                      dplyr::summarize(CIW = mean(CIW)))
  upr_avg <- data.frame(out_ref %>%
                          group_by_at(comp_grouping) %>%
                          dplyr::summarize(upr_avg = mean(upr)))
  lwr_avg <- data.frame(out_ref %>%
                          group_by_at(comp_grouping) %>%
                          dplyr::summarize(lwr_avg = mean(lwr)))

  # Merge all
  res <- cbind(bias_df,
               CIC = CIC$coverage,
               CIW = CIW$CIW,
               lwr_avg = lwr_avg$lwr_avg,
               upr_avg = upr_avg$upr_avg)
  return(res)
}