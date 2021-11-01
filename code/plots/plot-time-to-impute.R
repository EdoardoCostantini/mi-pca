# Project:   mipca_compare
# Objective: Plot time to impute
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
runName <- files[4]

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
dat = res_time
sel_meths = unique(res_time$method)#[c(1, 2, 3)] # all
plot_x_axis = "K"
plot_y_axis = "time"
moderator = "npc"
grid_x_axis = "pj"
grid_y_axis = "method"
x_axis_name = "Number of categories (K)"
y_axis_name = "Minutes"
scales = NULL
error_bar = FALSE
filters = list()

  # Subset data
  dat_sub <- dat %>%
    filter(method %in% sel_meths)

  # Apply extra filters
  for (f in seq_along(filters)){
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat_sub <- dat_sub %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

  # Make factors for plot
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
                                           end = .1))

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
    labs(title = "Time to impute",
         x     = x_axis_name,
         y     = y_axis_name)

  # Return final plot
  plot_themed
