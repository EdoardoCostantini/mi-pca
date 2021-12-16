# Project:   mipca_compare
# Objective: put results together from tar archive obtained with pc run
# Author:    Edoardo Costantini
# Created:   2021-11-25
# Modified:  2021-11-25

# Prep environment --------------------------------------------------------

  rm(list = ls()) # to clean up
  source("./init.R") # only for support functions

# Load Results ------------------------------------------------------------

  inDir <- "../output/"
  files <- grep("tar", list.files(inDir), value = TRUE)
  target_tar <- files[length(files)]
  output <- readTarGz(target_tar)

# Save output ------------------------------------------------------------

  saveRDS(list(output = output$out,
               fileNames = output$file_names,
               sInfo = output$sInfo),
          paste0(inDir, output$name_run, "_pc", "_unzipped.rds"))
