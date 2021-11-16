#!/bin/bash

# Project:   mipca_compare
# Objective: Prepare the stopos pool
# Author:    Edoardo Costantini
# Created:   2021-11-16
# Modified:  2021-11-16

# USAGE on LISA:
#   ./pool_script PATH_TO_POOL_LINES
#
# ARGS:
#   PATH_TO_POOL_LINES = Path to the text file with the number of lines to use for
#			  the stopos pool

# Create Stopos Pool
stopos create -p pool	# to create an empty pool of parameters
stopos -p pool add "$1"	# to put the parameters as lines in the pool
stopos status		# print a description of the resulting pool

