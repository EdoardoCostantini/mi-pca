# Project:   mipca_compare
# Objective: plot bias as a line plot
# Author:    Edoardo Costantini
# Created:   2021-12-16
# Modified:  2022-01-11

# Clean environment:
rm(list = ls())

# Support Functions and packages
source("./init.R")

# Read results
inDir <- "../output/"
files <- grep("rds", list.files(inDir), value = TRUE)
runName <- "20220111_165620_pc_main_gg_shape.rds" # toy run with PC
runName <- "8447019_main_gg_shape.rds" # good run with lisa
runName <- "8469421_main_gg_shape.rds" # final run with lisa

# Read output
gg_shape <- readRDS(paste0(inDir, runName))

# Define what to plot
target_par <- c(
  Mean = "z1~1",
  Variance = "z1~~z1",
  Covariance = "z1~~z2",
  Correlation = "z1rz2",
  Correlation = "z1rz3",
  Correlation = "z2rz3"
)

# Change names of factors for plot
levels(gg_shape$method) <- c("MI-PCR-ALL", "MI-PCR-ALL (oracle)",
                             "MI-PCR-AUX", "MI-PCR-VBV",
                             "MI-QP", "MI-OR", "MI-MI",
                             "CC", "OG")

# Inputs
dat         <- gg_shape
par_est     <- target_par[[4]]
sel_meths   <- levels(gg_shape$method)[c(1, 3:4)] # all
plot_x_axis <- "npc"
plot_y_axis <- "bias"
moderator   <- "method"
grid_x_axis <- "K"
grid_y_axis <- "pj"
x_axis_name <- "Number of categories (K)"
y_axis_name <- "PRB"
scales      <- NULL
filters     <- list(pj = c(0, 1),
                    K = c(Inf, 5, 2),
                    npc = 0:10,
                    lv = TRUE)
# filters     <- list()

# Reference data OG model
  dat_og <- dat %>%
    filter(par == par_est) %>%
    filter(method %in% "MI-OR")
  for (f in seq_along(filters)){
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat_og <- dat_og %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

# Reference data MI-QP model
  dat_qp <- dat %>%
    filter(par == par_est) %>%
    filter(method %in% "MI-QP")
  for (f in seq_along(filters)){
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat_qp <- dat_qp %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

# Main PCA based methods
  dat_sub <- dat %>%
    filter(par == par_est) %>%
    filter(method %in% sel_meths)
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
                      group = moderator)) +
    geom_line(aes_string(linetype = moderator), size = .75) +
    geom_segment(data = dat_og, aes(x = 1, xend = 10,
                                    y = bias, yend = bias),
                 color = "gray") +
    geom_text(data = dat_og, aes(x = 10.25, y = bias), label = "OG",
              size = 3) +
    geom_segment(data = dat_qp, aes(x = 1, xend = 10,
                                    y = bias, yend = bias),
                 color = "gray") +
    geom_text(data = dat_qp, aes(x = 10.25, y = bias), label = "MI-QP",
              size = 3) +
    geom_point(size = 0)
    # scale_linetype_manual(values = c(1, 5, 3))

    geom_hline(aes(yintercept = 10),
               size = .25)

  # Grid
  plot_grid <- plot_main +
    facet_grid(reformulate(grid_x_axis, grid_y_axis),
               labeller = labeller(.rows = label_both, .cols = label_value),
               scales = "free",
               switch = "y")

  # Format
  plot_themed <- plot_grid +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          strip.text.y.left = element_text(angle = 0),
          legend.position = "bottom",
          axis.title = element_text(size = 10),
          axis.title.y = element_blank()) +
    labs(x     = x_axis_name) +
    scale_y_continuous(position = "right")

  # Return final plot
  plot_themed

# Save it
ggsave("./plots/bias2.pdf",
  scale = 1,
  width = 25,
  height = 15,
  units = "cm",
  dpi = 300
)