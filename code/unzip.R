# Project:   mipca_compare
# Objective: unzips the results file and puts together objects for analysis
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-16

## Make sure we have a clean environment:
rm(list = ls())

## Support Functions
source("./init.R")

# Load Results ----------------------------------------------------------

inDir <- "../output/"
files <- grep("tar", list.files(inDir), value = TRUE)
target_tar <- "20211116_120623.tar.gz" # trial with subfolders
output <- readTarGz(tar_name = target_tar, subfolders = TRUE)

# Restructure Results -----------------------------------------------------
# list of conditions containing results for every repetition

# Summary
  output$sInfo$conds
  output$sInfo$parms

# Were there any errors?
  errors <- grep("ERROR", output$file_names)
  out_errors <- output$out[errors] # check that these are all trivial
  out_errors <- do.call(rbind, out_errors)

  if(length(errors) > 0){
    sapply(unique(out_errors$tag), function (x){
      sum(out_errors$tag %in% x)/length(out_errors$tag)
    })*100
  }

# Put together main results
  out_main <- output$out[grepl("main", output$file_names)]
  out <- do.call(rbind, out_main)

# Put together CPVE results from VBV method
  out_CPVE_list <- output$out[grepl("CPVE", output$file_names)]
  out_CPVE <- do.call(rbind, out_CPVE_list)

# Check out the time to impute
  out_time <- output$out[grepl("time", output$file_names)]
  res_time <- do.call(rbind, out_time)

  # Transform npc = max to appropriate number
  res_time$npc[res_time$npc == "max" & res_time$method == "all"] <- 50
  res_time$npc[res_time$npc == "max" & res_time$method == "aux"] <- 46
  res_time$npc[res_time$npc == "max" & res_time$method == "vbv"] <- 49

  # Cast experimental factors to ordered factors
  res_time$npc <- as.numeric(res_time$npc)
  res_time$K <- factor(res_time$K, levels = rev(unique(res_time$K)), ordered = TRUE)
  res_time$tag <- factor(res_time$tag, levels = unique(res_time$tag), ordered = TRUE)
  res_time$method <- factor(res_time$method,
                       levels = unique(output$sInfo$conds$method),
                       ordered = TRUE)

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

# Extract Results ----------------------------------------------------------

  results <- evaPerf(out, output)
  gg_shape <- results

# Store Results
  saveRDS(gg_shape,
          file = paste0("../output/",
                        output$name_run,
                        "_res",
                        ".rds")
  )

# Store Results
  saveRDS(res_time,
          file = paste0("../output/",
                        output$name_run,
                        "_time",
                        ".rds")
  )