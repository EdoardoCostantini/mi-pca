# Project:   mipca_compare
# Objective: pooling simulation results (not imputations!)
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-09-29

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

  # Were there any errors?
  grep("ERROR", output$file_names)

  # Put together main results
  out_main <- output$out[grepl("main", output$file_names)]
  out <- do.call(rbind, out_main)

  # Put together CPVE results from VBV method
  out_CPVE_list <- output$out[grepl("CPVE", output$file_names)]
  out_CPVE <- do.call(rbind, out_CPVE_list)

# Extract Results ----------------------------------------------------------

results <- evaPerf(out, output)
head(results)
gg_shape <- results

# Analysis ----------------------------------------------------------

# Line plot

K_conditions <- rev(sort(unique(gg_shape$K)))
D_conditions <- sort(unique(gg_shape$D))
int_conditions <- unique(gg_shape$interval)[1]
pj_sel <- unique(gg_shape$pj)
npc_sel <- unique(gg_shape$npc)
par_sel <- unique(gg_shape$par)
meth_sel <- unique(gg_shape$method)[c(1,2,3)]

plot1 <- gg_shape %>%
  # Subset
  filter(par == "z1~1") %>%
  filter(method %in% meth_sel) %>%

  # Main Plot
  ggplot(aes(x = pj, y = Mean, group = method)) +
  # geom_errorbar(aes(ymin = lwr_avg,
  #                   ymax = upr_avg,
  #                   group = method),
  #               width = .1) +
  geom_line(aes(linetype = method)) +
  geom_point() +

  # Grid
  facet_grid(cols = vars(factor(npc,
                                labels = paste0("npcs = ", npc_sel))),
             rows = vars(factor(K,
                                levels = K_conditions,
                                labels = paste0("K = ", K_conditions))),
             scales = "free") +
  # Format
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 15),
        # axis.text.x = element_text(angle = 45, hjust = 0.95),
        axis.title = element_text(size = 15)) +
  # scale_y_continuous(name = "Estimate Value",
  #                    limits = c(.5, .8)) +
  scale_x_continuous(name = "Proportion of junk variables",
                     breaks = pj_sel,
                     limits = range(pj_sel)) +
  labs(title = NULL,
       x     = NULL,
       y     = NULL)

plot1