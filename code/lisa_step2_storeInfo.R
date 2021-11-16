# Project:   mipca_compare
# Objective: Store the session information
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-16

## Prepare Environment
rm(list = ls())

## Initialize the environment:
source("./init.R")

## Prepare storing results
source("./fs.R")

# Output Directory from Terminal inputs
args <- commandArgs(trailingOnly = TRUE)
fs$outDir <- args[1]

# Create Empty storing object
out <- list(parms = parms,
            conds = conds,
            session_info = devtools::session_info())

# Save it in the root
saveRDS(out,
        paste0(fs$outDir,
               fs$fileName_res, ".rds")
)
