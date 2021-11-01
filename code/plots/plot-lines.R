# Project:   mipca_compare
# Objective: Line plots for bias, CIC and CIW
# Author:    Edoardo Costantini
# Created:   2021-11-01
# Modified:  2021-11-01

  # Read results
  inDir <- "../output/"
  files <- grep("rds", list.files(inDir), value = TRUE)
  runName <- files[3]

  # Read output
  gg_shape <- readRDS(paste0(inDir, runName))

# Line plot

target_par <- c(
  Mean = "z1~1",
  Variance = "z1~~z1",
  Covariance = "z1~~z2",
  Correlation = "z1rz2"
)
meth_sel <- levels(gg_shape$method)[c(1, 2, 3)]

# Plot Style 1:
# - main: bias x npcs
# - grid: K    x pj

lapply(target_par, function (x){
  plotLine(
    dat = gg_shape,
    par_est = x,
    sel_meths = meth_sel,
    plot_x_axis = "npc",
    plot_y_axis = "bias",
    moderator = "method",
    grid_x_axis = "pj",
    grid_y_axis = "K",
    x_axis_name = "Number of principal components extracted",
    y_axis_name = "Bias for ",
    scales = NULL,
    error_bar = FALSE,
    filters = list(npc = c(1, 5, 10, 20, 46, 49, 50))
  )
})

# Plot Style 1.5:
# - main: bias x PC explained variance
# - grid: K    x pj

lapply(target_par, function (x){
  plotLine(
    dat = gg_shape,
    par_est = x,
    sel_meths = meth_sel,
    plot_x_axis = "PC_exp",
    plot_y_axis = "bias",
    moderator = "method",
    grid_x_axis = "pj",
    grid_y_axis = "K",
    x_axis_name = "Proportion of explained variance",
    y_axis_name = "Bias for ",
    scales = NULL,
    error_bar = FALSE,
    scale_x_cont = FALSE,
    filters = list(npc = c(1, 5, 10, 20, 46, 49, 50))
  )
})

# Plot Style 3:
# - main: CIC x PC explained variance
# - grid: K   x pj
lapply(target_par, function (x){
  plotLine(
    dat = gg_shape,
    par_est = x,
    sel_meths = meth_sel,
    plot_x_axis = "PC_exp",
    plot_y_axis = "CIC",
    moderator = "method",
    grid_x_axis = "pj",
    grid_y_axis = "K",
    x_axis_name = "Proportion of explained variance",
    y_axis_name = "CIC for ",
    scales = NULL,
    error_bar = FALSE,
    scale_x_cont = FALSE,
    filters = list(npc = c(1, 5, 10, 20, 46, 49, 50)),
    plot_y_lim = c(.75, 1)
  )
})

# Plot style 3
# Plot Style 1:
# - main: bias x npcs
# - grid: K    x pj

meth_sel <- unique(gg_shape$method)[c(3, 4, 5, 6)]

lapply(target_par, function (x){
  plotLine(
    dat = gg_shape,
    par_est = x,
    sel_meths = meth_sel,
    plot_x_axis = "pj",
    plot_y_axis = "bias",
    moderator = "method",
    grid_x_axis = "npc",
    grid_y_axis = "K",
    x_axis_name = "Proportion of junk variables",
    y_axis_name = "Bias for ",
    scales = NULL,
    error_bar = FALSE,
    filters = list(npc = c(1, 5, 10, 20))
  )
})
