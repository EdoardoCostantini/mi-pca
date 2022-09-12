# Project:   mipca_compare
# Objective: put results together from tar archive obtained with lisa run
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-09-12

  rm(list = ls())
  source("./init.R")

# Unzip results -----------------------------------------------------------

  # Job ID
  idJob <- "9950505" # 1st BRM submission
  # idJob <- "9978614" # HD version
  # idJob <- "9985893" # HD version with full pj
  idJob <- "9987321" # HD version with full pj fix aux
  
  # Define location of results
  input_dir <- paste0("../output/lisa/", idJob, "/")
  tar_names <- grep(".tar.gz", list.files(input_dir), value = TRUE)

  # Create empty dir to contain results
  res_dir   <- paste0(input_dir, "res/")
  system(command = paste0("mkdir -p ", res_dir))

  # Read results in loop
  pb <- txtProgressBar(min = 0, max = length(tar_names), style = 3)

  for(i in 1:length(tar_names)){

    # Per every tar file do the following
    temp_dir  <- paste0(input_dir, "temp/")
    system(command = paste0("mkdir -p ", temp_dir))

    # Untar command definition
    untar_commands <- paste0("tar -C ",
                             temp_dir,   # where to place
                             " -xvf ", # untarring
                             input_dir,
                             tar_names[i])

    # Untar it
    system(command = untar_commands, ignore.stdout = TRUE, ignore.stderr = TRUE)

    # Obtain unique names of all result files
    fileNames <- grep(".rds", list.files(temp_dir), value = TRUE)

    # Read all
    output <- lapply(paste0(temp_dir, fileNames), readRDS)

    # Get the main results
    out_main_list <- output[grepl("main", fileNames)]
    out_main <- do.call(rbind, out_main_list)

    # Get the time results
    out_time_list <- output[grepl("time", fileNames)]
    out_time <- do.call(rbind, out_time_list)

    # Get the errors results
    errors <- grep("ERROR", fileNames)
    if(length(errors) > 0){
      out_errors <- output[errors] # check that these are all trivial
      out_errors <- do.call(rbind, out_errors)
    } else {
      out_errors <- NULL
    }

    # Store results -----------------------------------------------------------

    # Main
    saveRDS(out_main,
            paste0(res_dir,
                   gsub(".tar.gz", "_main.rds", tar_names[i])
            )
    )

    # Time
    saveRDS(out_time,
            paste0(res_dir,
                   gsub(".tar.gz", "_time.rds", tar_names[i])
            )
    )

    # Errors
    saveRDS(out_errors, paste0(res_dir,
                             gsub(".tar.gz", "_ERROR.rds", tar_names[i])
    )
    )

    # Delete Temp Folder
    system(command = paste0("rm -rf ", temp_dir))

    # Delete large objects to save same memory
    rm(output)

    # Update progres bar
    setTxtProgressBar(pb, i)
  }

  close(pb)

# Combine results from the multiple tar.gz sources ------------------------

  # Read main results
  rds_main_names <- grep("main", list.files(res_dir), value = TRUE)
  rds_mains <- lapply(paste0(res_dir, rds_main_names), readRDS)
  rds_main <- do.call(rbind, rds_mains)

  # Read time results
  rds_time_names <- grep("time", list.files(res_dir), value = TRUE)
  rds_times <- lapply(paste0(res_dir, rds_time_names), readRDS)
  rds_time <- do.call(rbind, rds_times)

  # Read error results
  rds_error_names <- grep("ERROR", list.files(res_dir), value = TRUE)
  rds_errors <- lapply(paste0(res_dir, rds_error_names), readRDS)
  rds_error <- do.call(rbind, rds_errors)

  # Read the session info object
  grep(".rds", list.files(input_dir), value = TRUE)
  sInfo <- readRDS(paste0(input_dir,
                          grep(".rds", list.files(input_dir), value = TRUE))
  )

  # Save output
  saveRDS(list(main = rds_main,
               time = rds_time,
               error = rds_error,
               sInfo = sInfo),
          paste0(res_dir, idJob, "_unzipped.rds"))