# Project:   mipca_compare
# Objective: Creates a fs (file system) object file system management
# Author:    Edoardo Costantini
# Created:   2021-08-24
# Modified:  2021-08-24

fs <- list()
fs$start_time <- format(Sys.time(), "%Y%m%d_%H%M%S")
fs$outDir <- paste0("../output/", fs$start_time, "/")
fs$fileName_res <- fs$start_time
fs$fileName_prog <- fs$start_time