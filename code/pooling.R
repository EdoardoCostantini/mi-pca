# Project:   mipca_compare
# Objective: pooling simulation results (not imputations!)
# Author:    Edoardo Costantini
# Created:   2021-09-29
# Modified:  2021-11-11

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

output$sInfo$conds
output$sInfo$parms

# Were there any errors?
errors <- grep("ERROR", output$file_names)
out_errors <- output$out[errors] # check that these are all trivial
out_errors <- do.call(rbind, out_errors)

sapply(unique(out_errors$tag), function (x){
  sum(out_errors$tag %in% x)/length(out_errors$tag)
})*100

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

# Tables ------------------------------------------------------------------

  library(kableExtra)
  # Paramters
  dat = gg_shape
  filters = list(npc = c(1, 5, 46, 49, 50),
                 K = c("7", "2"))

  # Subset data
  conds_columns <- c("pj", "K", "npc", "method")
  value_columns <- c("bias", "CIC", "CIW", "PC_exp")
  table_columns <- c(conds_columns, value_columns)
  dat_list <- list()
  for (i in seq_along(target_par)){
    dat_sub <- dat %>%
      filter(par == target_par[i]) %>%
      filter(method %in% sel_meths) %>%
      arrange(pj, K, npc, method)

    # Apply extra filters
    for (f in seq_along(filters)){
      filter_factor <- names(filters)[f]
      filter_lvels <- filters[[f]]
      dat_sub <- dat_sub %>%
        filter(!!as.symbol(filter_factor) %in% filter_lvels)
    }
    dat_list[[i]] <- dat_sub[, value_columns]
  }

  # Combine columns of data for different paramters
  dat_sub <- cbind(dat_sub[, conds_columns], do.call(cbind, dat_list))

  # Round
  dat_sub[, -(1:length(conds_columns))] <- round(dat_sub[, -(1:length(conds_columns))], 3)

  # Get rid of number of rows
  rownames(dat_sub) <- c()

  # Create header grouping vector
  header <- c(length(conds_columns),
              rep(length(value_columns), length(target_par)))
  names(header) <- c(" ", names(target_par))

  # Make table
  kbl(dat_sub,
      align = "c",
      format = "latex") %>%
    kable_styling(font_size = 6) %>%
    add_header_above(header) %>% # to name parameter columns
    row_spec(0, bold = T, background = "#D3D3D3") %>%
    column_spec(1:4, bold = T)  %>%
    collapse_rows(c(1, 2, 3), latex_hline = "full") # to combine factor levels
