# Project:   mipca_compare
# Objective: Obtain estimate of wall time for lisa
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-26

# Make sure we have a clean environment:
rm(list = ls(all = TRUE))

# Initialize the environment:
source("./init.R")

# Prepare storing results
source("./fs.R")

# Input values
rp        <- 1
fs$outDir <- "../output/trash/"

# Run one replication of the simulation:
start <- Sys.time()
runRep(rp = rp,
       conds = conds,
       parms = parms,
       fs = fs)
end <- Sys.time()
time_to_run <- end - start # after run
# time_to_run <- 5 # manual

## Calculate expected CPU time
goal_reps <- 500 # should match your total goal of repetitions
ncores    <- 15 # I want to use this many cores in each node
narray    <- ceiling(goal_reps/ncores)  # I want to specify a sbatch array of 2 tasks (sbatch -a 1-2 job_script_array.sh)
n_nodes   <- goal_reps/ncores # number of arrays
time_est  <- time_to_run # h it took on blade to run 1 repetition
wall_time <- time_est * 2 # expected job time on lisa
n_nodes * ncores * wall_time # expecgted SBU consumption