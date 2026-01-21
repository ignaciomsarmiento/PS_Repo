
##########################################################
# Replicates results for XXX
# authors: 
# Instructions: To reproduce all results, run: source("01_code/00_rundirectory.R")
##########################################################


# This script gets the data, there are no restrictions to getting the data
source("01_code/01_get_data.R")

# This script imputes missing values and saves a db in a temporary folder 03_temp called data_with_imputed_Values.Rds
source("01_code/02_clean_data.R")

