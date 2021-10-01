# Project:   mipca_compare
# Objective: Function to compute bias, coverage, and CIW
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-10-01

evaPerf <- function (){

}

# Cast experimental factors to ordered factors
out$tag <- factor(out$tag, levels = unique(out$tag), ordered = TRUE)
out$par <- factor(out$par, levels = unique(out$par), ordered = TRUE)
out$method <- factor(out$method,
                     levels = unique(output$sInfo$conds$method),
                     ordered = TRUE)

# True values
ref_df <- out %>%
  filter(method == "OG") %>%
  group_by(tag, par) %>%
  dplyr::summarize(ref = mean(Q_bar))

## Add based on matching par to original dataset
ref_vec <- ref_df$ref
out$ref <- rep(ref_vec, nrow(out) / length(ref_vec))

# Bias Computation
temp <- data.frame(out %>%
                     # filter(method == "aux") %>%
                     group_by(tag, par) %>%
                     dplyr::summarize(Mean = mean(Q_bar)))
temp$ref <- ref_vec[match(temp$par, ref_df$par)]
temp$bias <- abs(temp$Mean - temp$ref) / temp$ref*100

# Start Building dataset of results
res <- cbind(out[match(temp$tag, out$tag), -c(1, 2, 10:14)],
             temp[, -1])
rownames(res) <- NULL

# Confidence interval coverage for a given method across other factors
# In or out?
# Append reference values in the correct spot of the original results
out$ref <- ref_vec[match(out$par, ref_df$par)]

# Check if the confidence interval contains them
out$cover_log <- out$lwr < out$ref & out$ref < out$upr
CIC <- data.frame(out %>%
                    # filter(method == "OG") %>%
                    group_by(tag, par) %>%
                    dplyr::summarize(coverage = mean(cover_log)))
res <- cbind(res, CIC = CIC$coverage)

# Confidence interval width for a given method across other factors
out$CIW <- abs(out$lwr - out$upr)
CIW <- data.frame(out %>%
                    # filter(method == "OG") %>%
                    group_by(tag, par) %>%
                    dplyr::summarize(CIW = mean(CIW)))
res <- cbind(res, CIW = CIW$CIW)