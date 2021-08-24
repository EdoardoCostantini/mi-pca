# Project:   mipca_compare
# Objective: function to read compressed output
# Author:    Edoardo Costantini
# Created:   2021-08-24
# Modified:  2021-08-24

readTarGz <- function(tar_name){
  # Description:
  # Given the name of a tar.gz folder in the "output" project folder
  # it unzips it, reads the content, and deletes the unziepped folder

  # Move to Output folder
  setwd("../output/")

  # Unzip folder
  untar_command <- paste0("tar -xvf ", tar_name)
  system(untar_command)

  # Unzipped folder name
  name_run <- str_replace(tar_name, ".tar.gz", "")

  # Session info
  sInfo <- readRDS(paste0(name_run, "/sInfo.rds"))

  # Read .rds
  rds_names <- grep(".rds",
                  list.files(name_run),
                  value = TRUE)
  rds_keep <- rds_names[!grepl("sInfo", rds_names)]
  out <- lapply(paste0(name_run, "/", rds_keep), readRDS)

  # Delete Folder
  system(command = paste0("rm -rf ", name_run))

  # Revert WD to code folder
  setwd("../code/")

  # Return outputs
  return(list(out = out,
              name_run = name_run,
              file_names = rds_keep,
              sInfo = sInfo))
}