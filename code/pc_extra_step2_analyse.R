# Project:   mipca_compare
# Objective: Unzip and prepare data for plotting
# Author:    Edoardo Costantini
# Created:   2023-03-02
# Modified:  2023-03-02

# Prep environment --------------------------------------------------------

  rm(list = ls()) # to clean up
  source("./init_extra_ngdr.R") # only for support functions

# Load Results ------------------------------------------------------------

    inDir <- "../output/"
    target_tar <- "20230302_153750.tar.gz"
    output <- readTarGz(target_tar)

  # Collect main results
  rds_main <- do.call(rbind, output$out[-grep("ERROR", output$file_names)])

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

# Make some variables factors
rds_main$K <- factor(rds_main$K)
rds_main$method <- factor(rds_main$method, levels = c("noc", "naf", "nkaiser", "nparallel"))
rds_main$data <- factor(rds_main$data, levels = c("og", "na"), labels = c("Original data", "Data with NAs"))

# Store the rds results
saveRDS(
    object = rds_main,
    file = paste0(
        inDir,
        "extra_ngdr_gg_shape.rds"
    )
)

# Subset the data as desired
rds_main_sub <- rds_main %>%
    filter(
        lv == TRUE,
        # K == Inf,
        pj == 0,
        P == 56
    )

# Take average of value across repetitions for conditions of interest
res <- rds_main_sub %>%
    group_by(tag, P, K, D, interval, pj, lv, method, data) %>%
    summarise(
        outcome = mean(value)
    )

# Make result a data.frame
res <- data.frame(res)

# Violin plots -----------------------------------------------------------------

# Add violin plot on top
ggplot(rds_main_sub, aes(factor(method), value)) +
    geom_violin(
        adjust = 2,
        trim = TRUE
    ) +
    facet_grid(
        rows = vars(K),
        cols = vars(data),
        scales = "free"
    ) +
    theme_bw() +
    geom_label(
        data = res,
        aes(x = method, y = outcome, label = factor(outcome))
    ) +
    coord_cartesian(
        ylim = c(0, max(res$outcome) + 5)
    ) +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
    )
