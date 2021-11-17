# Project:   mipca_compare
# Objective: Put results together
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
  idJob <- "8405014"
  
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

  # Read the session info object
  sInfo <- readRDS(paste0(resDir, "20211116_183620.rds"))
  sInfo$parms
  sInfo$conds
  sInfo$session_info

# Check for errors --------------------------------------------------------
  # Were there any errors?
  errors <- grep("ERROR", fileNames)
  if(length(errors) > 0){
    out_errors <- output[errors] # check that these are all trivial
    out_errors <- do.call(rbind, out_errors)
    sapply(unique(out_errors$tag), function (x){
      sum(out_errors$tag %in% x)/length(out_errors$tag)
    })*100
  }

# Extract time results ----------------------------------------------------

  # Check out the time to impute
  out_time <- output[grepl("time", fileNames)]
  res_time <- do.call(rbind, out_time)

  # Transform npc = max to appropriate number
  res_time$npc[res_time$npc == "max" & res_time$method == "all"] <- 56
  res_time$npc[res_time$npc == "max" & res_time$method == "aux"] <- 52
  res_time$npc[res_time$npc == "max" & res_time$method == "vbv"] <- 55

  # Cast experimental factors to ordered factors
  res_time$npc <- as.numeric(res_time$npc)
  res_time$K <- factor(res_time$K, levels = rev(unique(res_time$K)), ordered = TRUE)
  res_time$tag <- factor(res_time$tag, levels = unique(res_time$tag), ordered = TRUE)
  res_time$method <- factor(res_time$method,
                       levels = unique(sInfo$conds$method),
                       ordered = TRUE)
  res_time$lv <- factor(res_time$lv)

  # Average time per condition
  comp_grouping <- c("K", "D", "interval", "pj", "npc", "method", "lv")
  time_avg <- data.frame(res_time %>%
                           group_by_at(comp_grouping) %>%
                           dplyr::summarize(mean = mean(time))
  )

  ref_time <- time_avg %>%
    filter(method == "MIOR")

  merge_cols <- comp_grouping <- c("K", "D", "interval", "pj")

  time_avg <- base::merge(x = time_avg, y = ref_time,
                          by = merge_cols,
                          suffixes = c("",".ref"))

  res_time <- time_avg %>%
    mutate(relative = mean / mean.ref) %>%
    select(-grep("ref", colnames(time_avg)))

# Main Results ------------------------------------------------------------

  out_main <- output[grepl("main", fileNames)]
  out <- do.call(rbind, out_main)

  results <- evaPerf(out, output)
  gg_shape <- results