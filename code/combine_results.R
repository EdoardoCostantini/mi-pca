# Project:   mipca_compare
# Objective: combine results from simulation study
# Author:    Edoardo Costantini
# Created:   2021-11-25
# Modified:  2022-01-11
# Note:      Works for both Lisa and PC results

# Prep environment --------------------------------------------------------

  rm(list = ls()) # to clean up
  source("./init.R") # only for support functions

# Load Results ------------------------------------------------------------

  loaction <- "../output/"
  run_name <- "20220111_165620_pc_unzipped" # toy run on pc
  run_name <- "8447019_unzipped" # good run on lisa
  run_name <- "8469421_unzipped" # final run on lisa
  out <- readRDS(paste0(loaction, run_name, ".rds"))

  sInfo <- out$sInfo

# Restructure Results -----------------------------------------------------

# > Main results

  # Reshape
  perf_out <- evaPerf(results = out$main,
                      sInfo = out$sInfo)

  # Store
  saveRDS(perf_out$res,
          file = paste0("../output/",
                        gsub("unzipped",
                             "main_gg_shape",
                             run_name),
                        ".rds")
  )

  saveRDS(perf_out$full_res,
          file = paste0("../output/",
                        gsub("unzipped",
                             "ciw_raw_gg_shape",
                             run_name),
                        ".rds")
  )

# > Time results

  res_time <- out$time

  # Transform npc = max to appropriate number
  npc_temp <- as.character(res_time$npc)
  max_position <- npc_temp %in% "max"
  npc_temp[max_position & res_time$method == "all"] <- 56
  npc_temp[max_position & res_time$method == "all_oracle"] <- 56
  npc_temp[max_position & res_time$method == "aux"] <- 52
  npc_temp[max_position & res_time$method == "vbv"] <- 55
  res_time$npc <- factor(npc_temp, sort(as.numeric(unique(npc_temp))))

  # Cast experimental factors to ordered factors
  res_time$K <- factor(res_time$K, levels = rev(unique(res_time$K)))
  res_time$tag <- factor(res_time$tag, levels = unique(res_time$tag))

  # Average time per condition
  comp_grouping <- c("K", "D", "interval", "pj", "npc", "method", "lv")

  time_avg <- data.frame(res_time %>%
                           group_by_at(comp_grouping) %>%
                           dplyr::summarize(mean = mean(time))
  )

  ref_time <- time_avg %>%
    filter(method == "MIOR")

  merge_cols <- comp_grouping <- c("K", "D", "interval", "pj", "lv")

  time_avg <- base::merge(x = time_avg, y = ref_time,
                          by = merge_cols,
                          suffixes = c("",".ref"))

  res_time <- time_avg %>%
    mutate(relative = mean / mean.ref) %>%
    select(-grep("ref", colnames(time_avg)))

  # Store
  saveRDS(res_time,
          file = paste0("../output/",
                        gsub("unzipped",
                             "time_gg_shape",
                             run_name),
                        ".rds")
  )