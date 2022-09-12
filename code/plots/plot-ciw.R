# Project:   mipca_compare
# Objective: Plot the distribution of Confidence Interval Width across repetitions
# Author:    Edoardo Costantini
# Created:   2021-12-16
# Modified:  2022-09-12

# Clean environment:
rm(list = ls())

# Support Functions and packages
source("./init.R")

# Confidence Interval widths distribution ---------------------------------

# Read results
inDir <- "../output/"
grep("rds", list.files(inDir), value = TRUE)
runName <- "20220111_165620_pc_main_gg_shape.rds" # toy run with PC
runName <- "8447019_main_gg_shape.rds" # good run with lisa
runName <- "8469421_main_gg_shape.rds" # final run with lisa
runName <- "9985893_main_gg_shape.rds" # HD version with full pj
runName <- "9987321_main_gg_shape.rds" # HD version with full pj fix aux

# Read output
ciw_raw <- readRDS(paste0(inDir, runName))

# Define what to plot
target_par <- c(
  Mean = "z1~1",
  Variance = "z1~~z1",
  Covariance = "z1~~z2",
  Correlation = "z1rz2"
)

# Change names of factors for plot
# levels(ciw_raw$method) <- c("MI-PCR-ALL", "MI-PCR-ALL (oracle)",
#                             "MI-PCR-AUX", "MI-PCR-VBV",
#                             "MI-QP", "MI-OR", "MI-MI",
#                             "CC", "OG")

# Confidence Interval Width --------------------------------------------

# Inputs
dat         <- ciw_raw
par_est     <- target_par[[4]]
sel_meths   <- levels(ciw_raw$method)[c(1, 3:5, 6, 9)] # all
plot_x_axis <- "method"
plot_y_axis <- "CIW"
moderator   <- "npc"
grid_x_axis <- "K"
grid_y_axis <- "pj"
x_axis_name <- "Method"
y_axis_name <- "CIW"
scales      <- NULL
error_bar   <- FALSE
filters     <-  list(par = par_est,
                     method = sel_meths,
                     npc = c(0, 7),
                    #  pj = c(0, 1),
                     K = c(Inf, 2),
                     lv = TRUE)
# filters     <- list()

# Subset data
dat_sub <- dat

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
  geom_boxplot(position = position_dodge(.5)) +
  scale_fill_manual(values = gray.colors(nlevels(dat_sub[, moderator]),
                                          start = .5,
                                          end = .8))


# Grid
plot_grid <- plot_main +
  facet_grid(reformulate(grid_x_axis, grid_y_axis),
             labeller = labeller(.rows = label_both, .cols = label_both),
             switch = "y")

# Format
plot_themed <- plot_grid +
    theme(text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5),
          strip.text.y.left = element_text(angle = 0),
          legend.position = "left",
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_y_continuous(position="right") # y-axis labels on the right

# Return final plot
plot_themed

# Save it
ggsave("./plots/ciw.pdf",
  scale = 1,
  width = 15,
  height = 15,
  units = "cm",
  dpi = 300
)

# Confidence Interval widths trends ---------------------------------------

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
  Correlation = "z1rz2"
)

# Change names of factors for plot
levels(gg_shape$method) <- c("MI-PCR-ALL", "MI-PCR-ALL (oracle)",
                             "MI-PCR-AUX", "MI-PCR-VBV",
                             "MI-QP", "MI-OR", "MI-MI",
                             "CC", "OG")
# gg_shape$npc <- as.numeric(as.character(gg_shape$npc))

# Inputs
dat         <- gg_shape
par_est     <- target_par[[4]]
sel_meths   <- levels(gg_shape$method)[c(1, 3:4)] # all
plot_x_axis <- "npc"
plot_y_axis <- "CIW"
moderator   <- "method"
grid_x_axis <- "K"
grid_y_axis <- "pj"
x_axis_name <- "Number of components"
y_axis_name <- "CIW"
scales      <- NULL
error_bar   <- FALSE
filters     <-  list(pj = c(0, 1),
                     K = c(Inf, 5, 2),
                     npc = c(0:10),
                     lv = TRUE)
# filters     <- list()

# Reference data OG model
dat_og <- dat %>%
    filter(par == par_est) %>%
    filter(method %in% "OG")

# Apply extra filters
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

# Apply extra filters
  for (f in seq_along(filters)){
    filter_factor <- names(filters)[f]
    filter_lvels <- filters[[f]]
    dat_qp <- dat_qp %>%
      filter(!!as.symbol(filter_factor) %in% filter_lvels)
  }

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

plot_main <- dat_sub %>%
  ggplot(aes_string(x = plot_x_axis,
                    y = plot_y_axis,
                    group = moderator)) +
  geom_line(aes_string(linetype = moderator), size = .75) +
  geom_segment(data = dat_og, aes(x = 1, xend = 10,
                                  y = CIW, yend = CIW),
               color = "gray") +
  geom_text(data = dat_og, aes(x = 10.25, y = CIW), label = "OG",
            size = 3) +
  geom_segment(data = dat_qp, aes(x = 1, xend = 10,
                                  y = CIW, yend = CIW),
               color = "gray") +
  geom_text(data = dat_qp, aes(x = 10.25, y = CIW), label = "MI-QP",
            size = 3) +
  geom_point(size = 0) +
  scale_linetype_manual(values = c(1, 5, 3))

# plot_main <- plot_main +
#   geom_ribbon(aes(ymin = CIW - CIW_sd,
#                   ymax = CIW + CIW_sd),
#               linetype = 2, alpha = 0.1)

# Grid
plot_grid <- plot_main +
  facet_grid(reformulate(grid_x_axis, grid_y_axis),
             labeller = label_both,
             switch = "y",
             scales = "free")

# Format
plot_themed <- plot_grid +
  theme(text = element_text(size = 10),
        strip.text.y.left = element_text(angle = 0),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.title.y = element_blank()) +
  labs(x = x_axis_name)

# Return final plot
plot_themed

# Save it
ggsave("./plots/ciw2.pdf",
       scale = 2,
       width = 22.5,
       height = 15,
       units = "cm",
       dpi = 300
)
