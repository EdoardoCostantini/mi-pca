### Title:    Defining Simulation Conditions
### Project:  MI-PCA study
### Author:   Edoardo Costantini
### Created:  2021-05-19

# Load fixed parameters from init file
source("./init.R")

# Define Experimental Factor Values
n_ord <- c(0, 1/3, 2/3, 1)
n_bin <- c(0, 1/3, 2/3, 1)
p_junk <- c(0, 1/3, 2/3, 1)

# Make Conditions
conds <- expand.grid(N  = parms$N,
                     L  = parms$L,
                     J  = parms$J,
                     P  = parms$L * parms$J,
                     pm = parms$pm, 
                     fl = parms$fl,
                     p_junk = p_junk,
                     n_ord = n_ord,
                     n_bin = n_bin,
                     stringsAsFactors = FALSE)
conds
# Get rid of impossible combinations of n_ord and n_bin
conds[rowSums(conds[, c("n_ord", "n_bin")]) <= 1, ]

# Parallel Experiments: for the continuous and attenuated relationship
# Take to the extreme
# Directly find second der at each point and select max