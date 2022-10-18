# Project:   mipca_compare
# Objective: Analyse convergence checks
# Author:    Edoardo Costantini
# Created:   2021-10-12
# Modified:  2022-10-18

## Make sure we have a clean environment:
rm(list = ls())

## Support Functions
source("./init.R")

# Load Results -----------------------------------------------------------------

# P = 56
output <- readTarGz("20220607_175414.tar.gz")

# P ~ 250
output <- readTarGz("20220906_171917.tar.gz")

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
                  [2])
interval <- paste0("interval",
                   unique(output$sInfo$conds$interval)
                   [1])
K <- paste0("K",
            unique(output$sInfo$conds$K)
             [2])
pj <- paste0("pj",
             unique(output$sInfo$conds$pj)
             [1])
npc <- paste0("npc",
              unique(output$sInfo$conds$npc)
              [1])
lv <- paste0("lv", c(TRUE, FALSE))[1]

# Search for the condition
search_pattern <- paste(K, D, interval, pj, npc, method, lv, sep = "_")
cond_selected <- grep(search_pattern, output$sInfo$conds$tag, value = TRUE)

# Make plot --------------------------------------------------------------------

# Condition to plot
dat_rep <- 1 # data repetition
to_plot <- grep(cond_selected,
                output$file_names[mids_index],
                value = TRUE)[dat_rep]
plot_title <- paste(K, D, interval, pj, npc, method, lv)

# Make plot
plot(out_mids[[to_plot]],
     # xlim = c(0, 100),
     layout = c(2, 4),
     main = plot_title)

# Export some meaningful mids for paper ----------------------------------------
# for the 242 condition

# Which conditions?
cond_sub <- output$sInfo$conds %>%
  filter(
    K == 2,
    pj == 0,
    method %in% levels(output$sInfo$conds$method)[-1],
    npc %in% c(0, 1))

# Define the names based on convention
tokeep_names <- paste0("rep_", dat_rep, "_", cond_sub$tag, "_mids.rds")

# Which repetition
dat_rep <- 1 # data repetition

# Extract
tokeep_mids <- out_mids[output$file_names[mids_index] %in% tokeep_names]

# Check them
lapply(1:length(tokeep_mids), function(x) {
  plot(tokeep_mids[[x]],
     layout = c(2, 4),
     main = names(tokeep_mids[x]))
})

# Save them
saveRDS(tokeep_mids, "../output/convergence-sim2-p242.rds")