### Title:    Testing Script
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-25
### Modified: 2021-06-21

# Set up ------------------------------------------------------------------

  # Packages
  library(testthat)
  
  # Environment
  rm(list=ls())
  source("./init.R")
  
  # Paths
  test_folder <- "../tests/testthat/"
  test_files <- list.files(test_folder)

# Perform Tests -----------------------------------------------------------

  test_result <- lapply(test_files, function(x){
    test_file(paste0(test_folder, x),
              reporter = TapReporter)
  })
