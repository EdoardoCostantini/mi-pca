# Project:   mipca_compare
# Objective: Run the small simulation study to check non-graphical solutions
# Author:    Edoardo Costantini
# Created:   2023-03-02
# Modified:  2023-03-02

# Make sure we have a clean environment:
rm(list = ls())

# Initialize the environment:
source("./init_extra_ngdr.R")

# Special package for parallelization
library(parabar)

# Run specs
reps <- 1:5     # define repetitions
clusters <- 5   # define clusters

# File System
fs <- list()
fs$start_time <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Main Folder
fs$outDir <- paste0("../output/", fs$start_time, "/")
dir.create(fs$outDir)

# File names
fs$fileName_res <- fs$start_time
fs$fileName_prog <- fs$start_time

# Progress report file
file.create(paste0(fs$outDir, fs$fileName_prog, ".txt"))

# Create header for report file
cat(
    paste0(
        "SIMULATION PROGRESS REPORT",
        "\n",
        "Starts at: ", Sys.time(),
        "\n", "------", "\n"
    ),
    file = paste0(fs$outDir, fs$fileName_prog, ".txt"),
    sep = "\n",
    append = TRUE
)

# Start a synchronous backend
backend <- makeCluster(clusters)
# backend <- start_backend(
#     cores = clusters,
#     cluster_type = "psock",
#     backend_type = "sync"
# )

# Export global env to worker nodes
# export(backend, variables = ls(), environment = .GlobalEnv)
clusterExport(cl = backend, varlist = "fs", envir = .GlobalEnv)
clusterEvalQ(cl = backend, expr = source("./init_extra_ngdr.R"))

# Store the time stamp at the beginning of the simulation
sim_start <- Sys.time()

# Run the computations in parallel on the 'clus' object:
out <- parLapply(
    backend,
    X = reps,
    fun = runRep.extra.ngdr,
    conds = conds,
    parms = parms,
    fs = fs
)

# Kill the cluster:
# stop_backend(backend)
stopCluster(backend)

# Store the time stamp at the end of the simulation
sim_ends <- Sys.time()

# Compute computation time
run_time <- difftime(sim_ends, sim_start, units = "hours")

# Update report with time stamps
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
out_support$run_time <- run_time

# Save output
saveRDS(out_support, paste0(fs$outDir, "sInfo.rds"))

# Zip output folder
writeTarGz(fs$fileName_res)