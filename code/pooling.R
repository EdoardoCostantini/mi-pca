# Project:   mipca_compare
# Objective: pooling simulation results (not imputations!)
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-09-29

  ## Make sure we have a clean environment:
  rm(list = ls())

  ## Support Functions
  source("./init.R")

# Load Results ----------------------------------------------------------

  inDir <- "../output/"
  files <- grep("tar", list.files(inDir), value = TRUE)
  target_tar <- files[length(files)]
  output <- readTarGz(target_tar)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

  # Give unique name to all objects
  # names(output$out) <- output$file_names

  # Put together main results
  out_main <- output$out[grepl("main", output$file_names)]
  out <- do.call(rbind, out_main)

  # out <- out[, -which(colnames(out) == "tag")]
  gg_shape <- reshape2::melt(out, id.var = colnames(out)[1:8])

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_pooled",
                        ".rds")
  )

  # Put together CPVE results from VBV method
  out_CPVE_list <- output$out[grepl("CPVE", output$file_names)]
  out_CPVE <- do.call(rbind, out_CPVE_list)