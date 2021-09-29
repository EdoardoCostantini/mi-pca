# Project:   mipca_compare
# Objective: Analysing results
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-09-29

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Support Functions
  source("./init.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  files <- grep("rds", list.files(inDir), value = TRUE)
  runName <- files[length(files)]

  # Read output
  gg_shape <- readRDS(paste0(inDir, runName))
  gg_shape <- gg_shape[, -which(colnames(gg_shape) == "tag")]

# Plots -------------------------------------------------------------------

  ## Obtain plots
  result <- unique(gg_shape$variable)[1]

  K_conditions <- rev(sort(unique(gg_shape$K)))
  D_conditions <- sort(unique(gg_shape$D))
  int_conditions <- unique(gg_shape$interval)[1]
  pj_sel <- unique(gg_shape$pj)
  npc_sel <- unique(gg_shape$npc)
  par_sel <- unique(gg_shape$par)
  # method_sel <- unique(gg_shape$method)
  methods <- paste(unique(gg_shape$method)[c(1,2)],
    collapse = "|"
  )

  plot1 <- gg_shape %>%
    # Subset
    filter(grepl(result, variable)) %>%
    filter(grepl(methods, method)) %>%
    filter(D %in% D_conditions) %>%
    filter(K %in% K_conditions) %>%
    filter(interval %in% int_conditions) %>%

    # Change labels of X axis
    # mutate(variable = fct_relabel(variable, str_replace, result, "")) %>%
    # Main Plot
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot() +
    # Grid
    facet_grid(rows = vars(factor(D,
                                  labels = paste0("D = ", D_conditions))),
               cols = vars(factor(K,
                                  levels = K_conditions,
                                  labels = paste0("K = ", K_conditions))),
               scales = "free") +
    # Format
    theme(text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          axis.text = element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 0.95),
          axis.title = element_text(size = 15)) +
    labs(title = paste0("Interval Scale = ", int_conditions,
                        " (", result, ")"),
         x     = NULL,
         y     = NULL)

  plot1

# Save plots --------------------------------------------------------------

  file_format <- ".png"
  plot_name <- paste0("interval_sacle_", int_conditions)
  out_dir <- "~/Desktop/"
  file_name <- paste0(out_dir, plot_name, file_format)
  if(file_format == ".pdf"){
    pdf(file_name, width = 15, height = 15)
  }
  if(file_format == ".png"){
    png(file_name, width = 15, height = 15, units = "in", res = 384)
  }
  plot1
  dev.off()