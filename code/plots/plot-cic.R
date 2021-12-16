# Project:   mipca_compare
# Objective: Plot confidence interval width
# Author:    Edoardo Costantini
# Created:   2021-12-16
# Modified:  2021-12-16

# Clean environment:
rm(list = ls())

# Support Functions and packages
source("./init.R")

# Read results
inDir <- "../output/"
files <- grep("rds", list.files(inDir), value = TRUE)
runName <- files[3]
runName <- files[7]

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

# Confidence Interval Coverage --------------------------------------------

# Inputs
dat         <- gg_shape
par_est     <- target_par[[4]]
sel_meths   <- levels(gg_shape$method)[c(1, 3:5, 6)] # all
plot_x_axis <- "K"
plot_y_axis <- "CIC"
moderator   <- "npc"
grid_x_axis <- "method"
grid_y_axis <- "pj"
x_axis_name <- "Number of categories (K)"
y_axis_name <- "CIC"
scales      <- NULL
error_bar   <- FALSE
filters     <-  list(pj = c(0, 1),
                     K = c(Inf, 5, 2),
                     npc = c(0:10),
                     lv = TRUE)
# filters     <- list()

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
             labeller = labeller(.rows = label_both, .cols = label_value),
             switch = "y")

# Format
plot_themed <- plot_grid +
  theme(text = element_text(size = 10),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "left",
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank()) +
  labs(x = x_axis_name) +
  coord_cartesian(ylim = c(-.95, .05))

# Return final plot
plot_themed

# Save it
ggsave("./plots/cic.pdf",
  scale = 1,
  width = 25,
  height = 15,
  units = "cm",
  dpi = 300
)