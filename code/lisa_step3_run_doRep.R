# Project:   mipca_compare
# Objective: Run a single repetition in lisa
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-16

## Make sure we have a clean environment:
rm(list = ls(all = TRUE))

## Initialize the environment:
source("./init.R")

## Prepare storing results
source("./fs.R")

## Extract commandline arguments
args      <- commandArgs(trailingOnly = TRUE)
rp        <- as.numeric(args[1]) # replication rp = 1 to desired
fs$outDir <- args[2]   # overwrite output directory defined in exp5_init.R

## Example Inputs Not to run
# rp        <- 1
# fs$outDir <- "../output/trash/"

## Run one replication of the simulation:
runRep(rp = rp,
       conds = conds[1:20, ],
       parms = parms,
       fs = fs)
