# Project:   mipca_compare
# Objective: Run the simulation study
# Author:    Edoardo Costantini
# Created:   2021-08-24
# Modified:  2021-10-16

## Make sure we have a clean environment:
rm(list = ls())

## Initialize the environment:
source("./init.R")

## Prepare storing results
source("./fs.R")

## Progress report file
dir.create(fs$outDir)
file.create(paste0(fs$outDir, fs$fileName_prog, ".txt"))

cat(paste0("SIMULATION PROGRESS REPORT",
           "\n",
           "Starts at: ", Sys.time(),
           "\n", "------", "\n" ),
    file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
    sep = "\n",
    append = TRUE)

## Define repetitions and clusters
reps <- 1 : 500
clus <- makeCluster(10)

## Export to worker nodes
# export global env
clusterExport(cl = clus, varlist = "fs", envir = .GlobalEnv)
# export script to be executed
clusterEvalQ(cl = clus, expr = source("./init.R"))

# mcApply parallel --------------------------------------------------------

sim_start <- Sys.time()

## Run the computations in parallel on the 'clus' object:
out <- parLapply(cl    = clus,
                 X     = reps,
                 fun   = runRep,
                 conds = conds,
                 parms = parms,
                 fs = fs)

## Kill the cluster:
stopCluster(clus)

sim_ends <- Sys.time()

cat(paste0("\n", "------", "\n",
           "Ends at: ", Sys.time(), "\n",
           "Run time: ",
           round(difftime(sim_ends, sim_start, units = "hours"), 3), " h",
           "\n", "------", "\n"),
    file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
    sep = "\n",
    append = TRUE)

# Attach Extract Info Objects
out_support <- list()
out_support$parms <- parms
out_support$conds <- conds
out_support$session_info <- devtools::session_info()

# Save output -------------------------------------------------------------

saveRDS(out_support,
        paste0(fs$outDir, "sInfo.rds"))

# Zip output folder -------------------------------------------------------

writeTarGz(fs$fileName_res)