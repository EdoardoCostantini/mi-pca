# Project:   mipca_compare
# Objective: Analyse convergence checks
# Author:    Edoardo Costantini
# Created:   2021-10-12
# Modified:  2021-10-12

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
errors <- grep("ERROR", output$file_names)
out_errors <- output$out[errors] # check that these are all trivial
out_errors <- do.call(rbind, out_errors)

# Condiitons checked
output$sInfo$conds
output$sInfo$conds %>%
  arrange(interval)

# Mids names
mids_index <- grep("mids", output$file_names)
output$file_names[mids_index]
out_mids <- output$out[mids_index]
out_mids
names(out_mids) <- output$file_names[mids_index]

# Decide which condition you want
method <- paste0("method",
                 unique(output$sInfo$conds$method)
                  [6])
interval <- paste0("interval",
                   unique(output$sInfo$conds$interval)
                   [2])
K <- paste0("K",
            unique(output$sInfo$conds$K)
             [2])
pj <- paste0("pj",
             unique(output$sInfo$conds$pj)
             [2])
npc <- paste0("npc",
              unique(output$sInfo$conds$npc)
              [1])
D <- "D1"
search_pattern <- paste(K, D, interval, pj, npc, method, sep = "_")
plot_title <- paste(K, D, interval, pj, npc, method)
cond_selected <- grep(search_pattern, output$sInfo$conds$tag, value = TRUE)

# Condition to plot
dat_rep <- 5
to_plot <- grep(cond_selected,
                output$file_names[mids_index],
                value = TRUE)[dat_rep]

# Make plot
plot(out_mids[[to_plot]],
     xlim = c(0, 50),
     layout = c(2, 4),
     main = plot_title)