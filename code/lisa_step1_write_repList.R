# Project:   mipca_compare
# Objective: Obtain estimate of wall time for lisa
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-16

# Prepare Environment
rm(list = ls())

# Source initialization script
source("./init.R")

# Calculate
goal_reps <- 30 # should match your total goal of repetitions
ncores    <- 15 # I want to use this many cores in each node
narray    <- ceiling(goal_reps/ncores)  # I want to specify a sbatch array of 2 tasks (sbatch -a 1-2 job_script_array.sh)

# Save in input folder for Stopos
outDir <- "../input/"
fileName <- paste0("stopos_lines")
write(as.character(1:goal_reps),
      file = paste0(outDir, fileName))

# Compute Estimated CPU time (not printed, just for yourself)
n_nodes <- goal_reps/ncores # this is also the number of nodes
n_cores <- ncores
job_time <- 40 * 1.5 # 60ish hours
n_nodes * n_cores * job_time