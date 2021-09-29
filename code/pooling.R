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

  out <- do.call(rbind, output$out)
  # out <- out[, -which(colnames(out) == "tag")]
  gg_shape <- reshape2::melt(out, id.var = colnames(out)[1:8])

  # Save
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_pooled",
                        ".rds")
  )