# Project:   mipca_compare
# Objective: function to obtain the line plot results
# Author:    Edoardo Costantini
# Created:   2021-10-07
# Modified:  2021-10-07

plotLine <- function (
  dat,
  par_est,
  sel_meths,
  plot_x_axis,
  plot_y_axis,
  x_axis_name = NULL,
  y_axis_name = NULL,
  moderator,
  grid_x_axis,
  grid_y_axis,
  scales = NULL, # or "free"
  error_bar = FALSE,
  scale_x_cont = TRUE
){

  # Inputs
  # dat = gg_shape
  # par_est = "z1~1"
  # sel_meths = unique(gg_shape$method)[c(1,2,3)]
  # plot_x_axis = "PC_exp"
  # plot_y_axis = "bias"
  # x_axis_name = "Number of principal components extracted"
  # y_axis_name = "Estimate of "
  # moderator = "method"
  # grid_x_axis = "pj"
  # grid_y_axis = "K"
  # scales = NULL # or "free"
  # error_bar = FALSE
  # scale_x_cont = TRUE

  # Subset data
  dat_sub <- dat %>%
    filter(par == par_est) %>%
    filter(method %in% sel_meths)

  # Main Plot
  plot_main <- dat_sub %>%
    ggplot(aes_string(x = plot_x_axis,
                      y = plot_y_axis,
                      group = moderator)) +
    geom_line(aes_string(linetype = moderator)) +
    geom_point()

  # Add error bars if wanted
  if(error_bar == TRUE){
    plot_main <- plot_main +
      geom_errorbar(aes(ymin = lwr_avg,
                        ymax = upr_avg,
                        group = method),
                    width = .1)
  }

  # Grid
  plot_grid <- plot_main +
    facet_grid(reformulate(grid_x_axis, grid_y_axis),
               labeller = label_both,
               scales = scales)

  # Format
  plot_themed <- plot_grid +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 10)) +
    labs(title = NULL,
         x     = x_axis_name,
         y     = paste(y_axis_name, par_est))

  if(is.numeric(dat[, plot_x_axis]) & scale_x_cont == TRUE){
    plot_themed <- plot_themed +
      scale_x_continuous(breaks = unique(dat[, plot_x_axis]),
                         limits = range(dat[, plot_x_axis]))
  }

  # Return final plot
  plot_themed
}