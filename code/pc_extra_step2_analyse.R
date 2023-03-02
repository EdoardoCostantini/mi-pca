# Project:   mipca_compare
# Objective: Unzip and prepare data for plotting
# Author:    Edoardo Costantini
# Created:   2023-03-02
# Modified:  2023-03-02

# Prep environment --------------------------------------------------------

  rm(list = ls()) # to clean up
  source("./init_extra_ngdr.R") # only for support functions

# Load Results ------------------------------------------------------------
  output <- readTarGz(target_tar)

  # Collect main results
  rds_main <- do.call(rbind, output$out)

  # Read error results
  rds_error_names <- grep("ERROR", output$file_names)
  if(length(rds_error_names) != 0){
    rds_error <- do.call(rbind, output$out[rds_error_names])
  } else {
    rds_error <- NULL
  }

# Process results --------------------------------------------------------------

# Make value a number
rds_main$value <- as.numeric(rds_main$value)

# Take average of value across repetitions
res <- rds_main %>% # Specify data frame
    group_by(tag, P, K, D, interval, pj, lv, method, data) %>% # Specify group indicator
    summarise(
        outcome = mean(value)
    )

# Make result a data.frame
res <- data.frame(res)

# Make K a factor
res$K <- factor(res$K)
head(res)

# Make plots that number of components per
res_sub <- res %>%
    filter(
        lv == TRUE,
        # K == Inf,
        pj == 1,
        P == 56
    ) 
res_sub %>%
    ggplot(aes(x = method, y = outcome, label = factor(outcome))) +
    geom_point() +
    geom_label() +
    facet_grid(
        rows = vars(K),
        cols = vars(data),
        scales = "free",
        switch = "y"
    ) +
    coord_cartesian(
        ylim = c(0, max(res_sub$outcome) + 5)
    ) + 
    theme_bw()
    
