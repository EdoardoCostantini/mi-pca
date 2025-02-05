# Project:   mipca_compare
# Objective: Install packages required for running the simulation
# Author:    Edoardo Costantini
# Created:   2021-05-12
# Modified:  2023-06-12
# Notes:     Assuming working directory ./code/

# Load init script to lists the names of the packages required
source("init.R")

# 1. Install all packages you can
install.packages(cran_list)

# 2. Install some packages that you need but are not listed
install.packages("glmnet")
install.packages("pls")

# 3. Install local mice.sim.pcr
install.packages(
    "../input/mice.pcr.sim_3.13.9.tar.gz",
    repos = NULL,
    type = "source"
)