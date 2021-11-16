# Project:   mipca_compare
# Objective: Plot confidence interval width
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
runName <- "20211027_154635_res.rds" # results for IOPS
runName <- "20211116_120623_res.rds" # with latent + subfolders

# Read output
gg_shape <- readRDS(paste0(inDir, runName))

# Define what to plot
target_par <- c(
  Mean = "z1~1",
  Variance = "z1~~z1",
  Covariance = "z1~~z2",
  Correlation = "z1rz2"
)

# Confidence Interval Width --------------------------------------------

dat         <- gg_shape
par_est     <- target_par[[4]]
sel_meths   <- unique(gg_shape$method)#[c(2:3, 8)] # all
plot_x_axis <- "K"
plot_y_axis <- "Mean"
moderator   <- "npc"
grid_x_axis <- "pj"
grid_y_axis <- "method"
x_axis_name = "Number of categories (K)"
y_axis_name <- "Average Estimate"
scales      <- NULL
error_bar   <- FALSE
filters = list(lv = "FALSE")
# filters     <- list()

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
  y_start <- "upr_avg"
  y_end <- "lwr_avg"

  # Main Plot
  plot_main <- dat_sub %>%
    ggplot(aes_string(x = plot_x_axis,
                      y = plot_y_axis,
                      ymax = y_start, ymin = y_end,
                      color = moderator)) +
    # geom_point(position = position_dodge(.5)) +
    geom_linerange(position = position_dodge(.5)) +
    scale_color_manual(values = gray.colors(nlevels(dat_sub[, moderator]),
                                            start = .7,
                                            end = .1))

  # Grid
  plot_grid <- plot_main +
    facet_grid(reformulate(grid_x_axis, grid_y_axis),
               labeller = label_both,
               scales = scales,
               switch = "y")

  # Format
  plot_themed <- plot_grid +
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          strip.text.y.left = element_text(angle = 0),
          legend.position = "left",
          axis.title = element_text(size = 10)) +
    labs(title = paste("Confidence Interval Width for ", par_est),
         x     = x_axis_name,
         y     = y_axis_name) +
    scale_y_continuous(position="right") # y-axis labels on the right

  # Return final plot
  plot_themed

## V2
# Inputs
dat         <- gg_shape
par_est     <- target_par[[4]]
sel_meths   <- levels(gg_shape$method)[c(1:5, 8)] # all
plot_x_axis <- "K"
plot_y_axis <- "mcsd"
moderator   <- "npc"
grid_x_axis <- "pj"
grid_y_axis <- "method"
x_axis_name = "Number of categories (K)"
y_axis_name <- "CIW"
scales      <- "free"
error_bar   <- FALSE
# filters     <- list(npc <- c(1, 5, 10, 20))
filters     <- list()

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
                                         end = .1))

# Grid
plot_grid <- plot_main +
  facet_grid(reformulate(grid_x_axis, grid_y_axis),
             labeller = label_both,
             scales = scales,
             switch = "y")

# Format
plot_themed <- plot_grid +
  theme(text = element_text(size = 15),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "left",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  labs(title = paste0("Confidence Interval Width for ", par_est),
       x     = x_axis_name,
       y     = y_axis_name) +
  scale_y_continuous(position="right") + # y-axis labels on the right
  coord_cartesian(ylim = c(0, .10))

# Return final plot
plot_themed

# Confidence Interval Coverage --------------------------------------------

# Inputs
dat         <- gg_shape
par_est     <- target_par[[4]]
sel_meths   <- levels(gg_shape$method)[c(1:5, 8)] # all
plot_x_axis <- "K"
plot_y_axis <- "CIC"
moderator   <- "npc"
grid_x_axis <- "pj"
grid_y_axis <- "method"
x_axis_name <- "Number of categories (K)"
y_axis_name <- "CIC"
scales      <- NULL
error_bar   <- FALSE
# filters     <- list(npc <- c(1, 5, 10, 20))
filters     <- list()

# Subset data
dat_sub <- dat %>%
  filter(par == par_est) %>%
  filter(method %in% sel_meths) %>%
  mutate(CIC = CIC - .95)

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
  scale_y_continuous(breaks = c(-.25, -.15, -.05, 0, .05),
                     labels = c(70, 80, 90, 95, 100),
                     limits = c(-.95, .05),
                     position = "right") +
  geom_hline(aes(yintercept = -.05),
             size = .25)

# Grid
plot_grid <- plot_main +
  facet_grid(reformulate(grid_x_axis, grid_y_axis),
             labeller = label_both,
             switch = "y")

# Format
plot_themed <- plot_grid +
  theme(text = element_text(size = 15),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "left",
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10)) +
  labs(title = paste0("Confidence Interval Coverage for ", par_est),
       x     = x_axis_name,
       y     = y_axis_name) +
  coord_cartesian(ylim = c(-.15, .05))


# Return final plot
plot_themed
