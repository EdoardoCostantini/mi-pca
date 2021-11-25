# Project:   mipca_compare
# Objective: put results together from tar archive obtained with lisa run
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-16

  rm(list = ls())
  source("./init.R")

# Load results ------------------------------------------------------------

  # Input directory
  inDir <- "../output/lisa/"
  
  # Output directory
  outDir <- "../output/lisa/"
  
  # Job ID
  idJob <- "8405642"
  
  # Define Output Run folder
  resDir <- paste0(inDir, idJob, "/") # location of results
  
  # Create Empty Directory to contain every output
  subDir_tar <- grep(".tar.gz", list.files(resDir), value = TRUE)
  subDir     <- paste0(resDir, "rds_objects/")
  system(command = paste0("mkdir -p ", subDir))
  
  # Untar everything and move to main output folder
  untar_commands <- paste0("tar ", 
                           "-C ", 
                           subDir, # where to place
                           " -xvf ", # untarring
                           resDir, subDir_tar)
  lapply(untar_commands, system)

  # Obtain unique names of all result files
  fileNames <- grep(".rds", list.files(subDir), value = TRUE)

  # Read all
  output <- lapply(paste0(subDir, fileNames), readRDS)

  # Delete Folder
  system(command = paste0("rm -rf ", resDir, "rds_objects"))

  # Read the session info object
  sInfo <- readRDS(paste0(resDir, "20211117_110718.rds"))
  sInfo$parms
  sInfo$conds
  sInfo$session_info

  # Save output
  saveRDS(list(output = output,
               fileNames = fileNames,
               sInfo = sInfo), paste0(resDir, "20211117_110718_unzipped.rds"))