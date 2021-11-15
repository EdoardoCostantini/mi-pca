# Project:   mipca_compare
# Objective: Plot bias for the PCA based methods
# Author:    Edoardo Costantini
# Created:   2021-10-27
# Modified:  2021-11-01

# Clean environment:
rm(list = ls())

# Support Functions and packages
source("./init.R")

# Read results
inDir <- "../output/"
files <- grep("rds", list.files(inDir), value = TRUE)
runName <- files[3]

# Read output
gg_shape <- readRDS(paste0(inDir, runName))

# Define what to plot
target_par <- c(
  Mean = "z1~1",
  Variance = "z1~~z1",
  Covariance = "z1~~z2",
  Correlation = "z1rz2"
)

# Inputs
dat = gg_shape
par_est = target_par[[4]]
sel_meths = levels(gg_shape$method)[c(1:7)] # all
plot_x_axis = "K"
plot_y_axis = "bias"
moderator = "npc"
grid_x_axis = "pj"
grid_y_axis = "method"
x_axis_name = "Number of categories (K)"
y_axis_name = "PRB"
scales = NULL
error_bar = FALSE
# filters = list(npc = c(1, 5, 10, 20))
filters = list()

  # Subset data
  dat_sub <- dat %>%
    filter(par == par_est) %>%
    filter(method %in% sel_meths)

  # Apply extra filters
  for (f in seq_along(filters)){
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat_sub <- dat_sub %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

  # Make factors
  dat_sub[, plot_x_axis] <- factor(dat_sub[, plot_x_axis])
  dat_sub[, moderator] <- factor(dat_sub[, moderator])

  # Main Plot
  plot_main <- dat_sub %>%
    ggplot(aes_string(x = plot_x_axis,
                      y = plot_y_axis,
                      fill = moderator)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    scale_fill_manual(values = gray.colors(nlevels(dat_sub[, moderator]),
                                           start = .7,
                                           end = .1)) +
    geom_hline(aes(yintercept = 10),
               size = .25)

  # Grid
  plot_grid <- plot_main +
    facet_grid(reformulate(grid_x_axis, grid_y_axis),
               labeller = label_both,
               scales = "free")

  # Format
  plot_themed <- plot_grid +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          # axis.text = element_text(size = 15),
          # axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 10)) +
    labs(title = paste("Bias for ", par_est),
         x     = x_axis_name,
         y     = y_axis_name) +
    coord_cartesian(ylim = c(0, 15))

  # Return final plot
  plot_themed
