# Project:   mipca_compare
# Objective: briefly explore dimensionality of the WVS
# Author:    Edoardo Costantini
# Created:   2022-04-11
# Modified:  2022-04-11

# Load data from CSV

WVS <- read.csv2("../../input/WVS_Cross-National_Wave_7_csv_v3_0.csv",
                header = TRUE)

# Check it

WVS[1:10, 1:10]

# Count how many "Q"uestions in the survey

sum(grepl("^[Q][0-9]", colnames(WVS)))