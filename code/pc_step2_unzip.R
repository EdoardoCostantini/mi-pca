# Project:   mipca_compare
# Objective: put results together from tar archive obtained with pc run
# Author:    Edoardo Costantini
# Created:   2021-11-25
# Modified:  2022-01-11

# Prep environment --------------------------------------------------------

  rm(list = ls()) # to clean up
  source("./init.R") # only for support functions

# Load Results ------------------------------------------------------------

  inDir <- "../output/"
  files <- grep("tar", list.files(inDir), value = TRUE)
  target_tar <- files[length(files)]
  output <- readTarGz(target_tar)

  # Collect main results
  rds_main_names <- grep("main", output$file_names)
  rds_main <- do.call(rbind, output$out[rds_main_names])

  # Collect time results
  rds_time_names <- grep("time", output$file_names)
  rds_time <- do.call(rbind, output$out[rds_time_names])

  # Read error results
  rds_error_names <- grep("ERROR", output$file_names)
  if(length(rds_error_names) != 0){
    rds_error <- do.call(rbind, output$out[rds_error_names])
  } else {
    rds_error <- NULL
  }

# Save output ------------------------------------------------------------

  saveRDS(list(main = rds_main,
               time = rds_time,
               error = rds_error,
               sInfo = output$sInfo),
          paste0(inDir,
                 tools::file_path_sans_ext(target_tar, compression = TRUE),
                 "_pc",
                 "_unzipped.rds"))