# Project:   mipca_compare
# Objective: Analyse convergence checks
# Author:    Edoardo Costantini
# Created:   2021-10-12
# Modified:  2022-06-10

## Make sure we have a clean environment:
rm(list = ls())

## Support Functions
source("./init.R")

# Load Results -----------------------------------------------------------------

output <- readTarGz("20220607_175414.tar.gz")

# Restructure Results ----------------------------------------------------------
# list of conditions containing results for every repetition

# Were there any errors?
errors <- grep("ERROR", output$file_names)
out_errors <- output$out[errors] # check that these are all trivial
out_errors <- do.call(rbind, out_errors)

# Condiitons checked
output$sInfo$conds %>%
  arrange(interval)

# Mids names
mids_index <- grep("mids", output$file_names)
output$file_names[mids_index]
out_mids <- output$out[mids_index]
names(out_mids) <- output$file_names[mids_index]

# Decide which condition you want
D <- "D1"
method <- paste0("method",
                 unique(output$sInfo$conds$method)
                  [4])
interval <- paste0("interval",
                   unique(output$sInfo$conds$interval)
                   [1])
K <- paste0("K",
            unique(output$sInfo$conds$K)
             [2])
pj <- paste0("pj",
             unique(output$sInfo$conds$pj)
             [2])
npc <- paste0("npc",
              unique(output$sInfo$conds$npc)
              [3])
lv <- paste0("lv", c(TRUE, FALSE))[1]

# Search for the condition
search_pattern <- paste(K, D, interval, pj, npc, method, lv, sep = "_")
cond_selected <- grep(search_pattern, output$sInfo$conds$tag, value = TRUE)

# Make plot --------------------------------------------------------------------

# Condition to plot
dat_rep <- 2 # data repetition
to_plot <- grep(cond_selected,
                output$file_names[mids_index],
                value = TRUE)[dat_rep]
plot_title <- paste(K, D, interval, pj, npc, method, lv)

# Make plot
plot(out_mids[[to_plot]],
     xlim = c(0, 50),
     layout = c(2, 4),
     main = plot_title)