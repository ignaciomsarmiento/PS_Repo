##########################################################
# Master script
#
# Running this file reproduces all results in the repository.
#
# To reproduce all results, run:
# from an interactive R session: source("01_code/00_rundirectory.R")   
# or from the command line: R CMD BATCH 01_code/00_rundirectory.R
#
# Authors:
# - Ignacio Sarmiento-Barbieri
# - Gustavo Castillo Alvarez
##########################################################
# NOTE:
# - This file is provided as an EXAMPLE
# - This script should only call other scripts.
##########################################################

# Step 1: Download and construct the raw dataset
source("01_code/01_get_data.R")

# Step 2: Filters data 
# The filtered dataset is saved to a temporary folder for downstream use
source("01_code/02_filters_data.R")



