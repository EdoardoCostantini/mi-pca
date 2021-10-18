# Project:   mipca_compare
# Objective: pooling simulation results (not imputations!)
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-10-18

## Make sure we have a clean environment:
rm(list = ls())

## Support Functions
source("./init.R")

# Load Results ----------------------------------------------------------

inDir <- "../output/"
files <- grep("tar", list.files(inDir), value = TRUE)
target_tar <- files[length(files)]
output <- readTarGz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

output$sInfo$conds
output$sInfo$parms

# Were there any errors?
errors <- grep("ERROR", output$file_names)
out_errors <- output$out[errors] # check that these are all trivial
out_errors <- do.call(rbind, out_errors)

sapply(unique(out_errors$tag), function (x){
  sum(out_errors$tag %in% x)/length(out_errors$tag)
})*100

# Put together main results
out_main <- output$out[grepl("main", output$file_names)]
out <- do.call(rbind, out_main)

# Put together CPVE results from VBV method
out_CPVE_list <- output$out[grepl("CPVE", output$file_names)]
out_CPVE <- do.call(rbind, out_CPVE_list)

# Extract Results ----------------------------------------------------------

results <- evaPerf(out, output)
gg_shape <- results

# Analysis ----------------------------------------------------------

# Line plot

target_par <- c(
  Mean = "z1~1",
  Variance = "z1~~z1",
  Covariance = "z1~~z2",
  Correlation = "z1rz2"
)

# Plot Style 1:
# - main: bias x npcs
# - grid: K    x pj

lapply(target_par, function (x){
  plotLine(
    dat = gg_shape,
    par_est = x,
    sel_meths = unique(gg_shape$method)[c(1,2,3)],
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
    sel_meths = unique(gg_shape$method)[c(1,2,3)],
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
    sel_meths = unique(gg_shape$method)[c(1,2,3)],
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


## INCLUDE THESE IN YOUR PLOT THOUGHT!
D_conditions <- sort(unique(gg_shape$D))
int_conditions <- unique(gg_shape$interval)[1]

  library(kableExtra)
  # Paramters
  dat = gg_shape
  sel_meths = unique(gg_shape$method)[c(1,2,3)]
  filters = list(npc = c(1, 5, 46, 49, 50),
                 K = c("Inf", "5"))

  # Subset data
  conds_columns <- c("pj", "K", "npc", "method")
  value_columns <- c("bias", "CIC", "CIW", "PC_exp")
  table_columns <- c(conds_columns, value_columns)
  dat_list <- list()
  for (i in seq_along(target_par)){
    dat_sub <- dat %>%
      filter(par == target_par[i]) %>%
      filter(method %in% sel_meths) %>%
      arrange(pj, K, npc, method)

    # Apply extra filters
    for (f in seq_along(filters)){
      filter_factor <- names(filters)[f]
      filter_lvels <- filters[[f]]
      dat_sub <- dat_sub %>%
        filter(!!as.symbol(filter_factor) %in% filter_lvels)
    }
    dat_list[[i]] <- dat_sub[, value_columns]
  }

  # Combine columns of data for different paramters
  dat_sub <- cbind(dat_sub[, conds_columns], do.call(cbind, dat_list))

  # Round
  dat_sub[, -(1:length(conds_columns))] <- round(dat_sub[, -(1:length(conds_columns))], 3)

  # Get rid of number of rows
  rownames(dat_sub) <- c()

  # Create header grouping vector
  header <- c(length(conds_columns),
              rep(length(value_columns), length(target_par)))
  names(header) <- c(" ", names(target_par))

  # Make table
  kbl(dat_sub,
      align = "c",
      format = "latex") %>%
    kable_styling(font_size = 6) %>%
    add_header_above(header) %>% # to name parameter columns
    row_spec(0, bold = T, background = "#D3D3D3") %>%
    column_spec(1:4, bold = T)  %>%
    collapse_rows(c(1, 2, 3), latex_hline = "full") # to combine factor levels
