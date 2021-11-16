# Project:   mipca_compare
# Objective: Obtain estimate of wall time for lisa
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-16

# Make sure we have a clean environment:
rm(list = ls(all = TRUE))

# Initialize the environment:
source("./init.R")

# Prepare storing results
source("./fs.R")

# Input values
rp        <- 1
fs$outDir <- "../output/trash/"

## Run one replication of the simulation:
start <- Sys.time()
runRep(rp = rp,
       conds = conds,
       parms = parms,
       fs = fs)
end <- Sys.time()
end - start