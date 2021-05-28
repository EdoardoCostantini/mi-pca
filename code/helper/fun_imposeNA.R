### Title:    Data Generation Function
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-28

imposeNA <- function(dat_in, parms, cond, plot = FALSE){
  ## Description
  # Adds Matrix Design missingness to the MAR
  ## Example Inputs
  # cond <- conds[1, ]
  # dat_in <- genData(parms = parms, cond = cond)
  # plot = TRUE
  
# MAR ---------------------------------------------------------------------
  
  dat_out <- dat_in$dat_ob
  
  # Number of variables receiving MAR
  MAR_ta_n <- length(parms$varMap$ta) * parms$J
  
  # Impose MAR
  for (i in 1:MAR_ta_n) {
    nR <- simMissingness(pm    = cond$pm,
                         data  = dat_in$dat_lv,
                         preds = parms$varMap$mp,
                         type  = "center",
                         beta  = rep(1, length(parms$varMap$mp)))
    
    # Fill in NAs
    dat_out[nR, i] <- NA
  }
  
# MCAR --------------------------------------------------------------------
  
  # Planned Missing Blocks
  blocks <- LETTERS[1:4]
  
  # Assign variables to blocks
  groups <- combn(x = blocks, m = 2, simplify = FALSE)
  
  # Define in which block observations are
  memb_ids <- sort(rep(paste0("g", 1:length(groups)), 
                       length.out = parms$N))
  memb_ids_uni <- unique(memb_ids)
  
  # Define in which block variables are
  memb_vars <- sort(rep(blocks, parms$P/length(blocks)))
  
  # Impose planned missingness
  for (i in 1:length(groups)) {
    # Select units in the group
    row_filter <- memb_ids %in% memb_ids_uni[i]
    
    # Select variables in the block
    col_filter <- memb_vars %in% groups[[i]]
      
    # Impose Missing on those
    dat_out[row_filter, col_filter] <- NA
  }
  
  # Visual Check
  if(plot == TRUE){
    plot(as.matrix(!is.na(dat_out)), # requires "plot.matrix" pack
         border = NA,
         breaks = c(TRUE, FALSE),
         col = c("black", "white"),
         main = "Missing data pattern",
         cex.lab = 1, # text size for axis labels
         cex.axis = .5, # text size for ticks labels
         las = 2, # orientation of ticks
         y = "Observation ID",
         xlab = "Variable ID")
  }
  
  # Result
  return( dat_out )
}