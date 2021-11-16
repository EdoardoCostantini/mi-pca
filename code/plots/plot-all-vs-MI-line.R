# Project:   mipca_compare
# Objective: Plot All methods against MIOP and MIOR
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

# Inputs
dat = gg_shape
par_est = target_par[[4]]
sel_meths = levels(gg_shape$method)[c(1, 4, 5)] # all
plot_x_axis = "pj"
plot_y_axis = "bias"
moderator = "method"
grid_x_axis = "npc"
grid_y_axis = "K"
x_axis_name = "Number of categories (K)"
y_axis_name = "PRB"
scales = NULL
error_bar = FALSE
filters = list(npc = unique(gg_shape$npc)[c(1, 2, 6, 7, 12)])
# filters = list()

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

# transfor to character for pasting
dat_sub$method <- as.character(dat_sub$method)

# identify PCA method
to_mesh <- !grepl("MI", dat_sub$method)

# Create new names
dat_sub[to_mesh, "method"] <- paste0(dat_sub[to_mesh, "method"],
                                     "-npc-",
                                     dat_sub[to_mesh, "npc"])
dat_sub$method <- factor(dat_sub$method,
                         levels = str_sort(unique(dat_sub$method),
                                           numeric = TRUE))

# Define Colors
same_line <- length(unique(dat_sub[to_mesh, "method"]))

# Main Plot
plot_main <- dat_sub %>%
  ggplot(aes_string(x = plot_x_axis,
                    y = plot_y_axis,
                    group = moderator,
                    color = moderator)) +
  geom_line(aes_string(linetype = moderator), size = .75) +
  scale_linetype_manual(values = c(rep(1, same_line), 5, 3)) +
  scale_color_manual(values = c(gray.colors(same_line,
                                            start = .7,
                                            end = .1),
                                "blue",
                                "blue")) +
  geom_point(size = 0)

# Grid
plot_grid <- plot_main +
  facet_grid(reformulate(".", grid_y_axis),
             labeller = label_both,
             switch = "y",
             scales = "free")

# Format
plot_themed <- plot_grid +
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5),
        # axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)) +
  labs(title = paste("Bias for ", par_est),
       x     = x_axis_name,
       y     = y_axis_name)

# Return final plot
plot_themed