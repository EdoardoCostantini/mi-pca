# Project:   mipca_compare
# Objective: combine results from simulation study
# Author:    Edoardo Costantini
# Created:   2021-11-25
# Modified:  2021-11-25
# Note:      Works for both Lisa and PC results

# Prep environment --------------------------------------------------------

  rm(list = ls()) # to clean up
  source("./init.R") # only for support functions

# Load Results ------------------------------------------------------------

  loaction <- "../output/"
  out <- readRDS(paste0(loaction, "20211125_150443_pc_unzipped.rds")) # pc
  # out <- readRDS(paste0(loaction, "20211117_110718_lisa_unzipped.rds")) # lisa
  output <- out$output
  fileNames <- out$fileNames
  sInfo <- out$sInfo

# Extract Results ---------------------------------------------------------

# Were there any errors?
  errors <- grep("ERROR", fileNames)
  errors_pr <- length(errors)/length(fileNames) # proportion of errors
  if(errors_pr > 0){
    # Combine errors in a data.frame
    out_errors <- output[errors] # check that these are all trivial
    out_errors <- do.call(rbind, out_errors)

    # Check the first few errors
    head(out_errors)

    # Proportion of errors
    sapply(unique(out_errors$tag), function (x){
      sum(out_errors$tag %in% x)/length(out_errors$tag)
    })*100
  }

# Put together main results
  out_main_list <- output[grepl("main", fileNames)]
  out_main <- do.call(rbind, out_main_list)

# Put together CPVE results from VBV method
  out_CPVE_list <- output[grepl("CPVE", fileNames)]
  out_CPVE <- do.call(rbind, out_CPVE_list)

# Check out the time to impute
  out_time <- output[grepl("time", fileNames)]
  res_time <- do.call(rbind, out_time)

# Restructure Results -----------------------------------------------------

# > Main results

  # Reshape
  gg_shape <- evaPerf(results = out_main,
                     sInfo = sInfo)

  # Store
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_res",
                        ".rds")
  )

# > Time results

  # Transform npc = max to appropriate number
  res_time$npc <- as.numeric(res_time$npc)
  res_time$npc[is.na(res_time$npc) & res_time$method == "all"] <- 56
  res_time$npc[is.na(res_time$npc) & res_time$method == "aux"] <- 52
  res_time$npc[is.na(res_time$npc) & res_time$method == "vbv"] <- 55

  # Cast experimental factors to ordered factors
  res_time$K <- factor(res_time$K, levels = rev(unique(res_time$K)))
  res_time$tag <- factor(res_time$tag, levels = unique(res_time$tag))

  # Average time per condition
  comp_grouping <- c("K", "D", "interval", "pj", "npc", "method")
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

  # Store
  saveRDS(res_time,
          file = paste0("../output/",
                        output$name_run,
                        "_time",
                        ".rds")
  )