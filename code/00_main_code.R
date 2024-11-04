
# Description - This code reproduces all tables and figures from the paper

# TO DO
## Run sum stats
# Initial Commands --------------------------------------------------------

rm(list = ls(all.names = TRUE)) # clear objects
gc() # free up memory
set.seed(1234)

# Downloads required libraries --------------------------------------------

renv::restore()

# Libraries ---------------------------------------------------------------
library("tidyverse")    # to handle data
library("estimatr")
library("modelsummary") # to create tables
library('geobr')        # to create maps
library('skimr')        # to create sumstats
library('rdrobust')     # to run rdd
library('patchwork')    # to create figures with plots together

# Setting -----------------------------------------------------------------
work_dir                       = "C:/Users/wb633398/Documents/GitHub/Personal/rep_package_when_science_strikes_back"
output_dir                     = paste0(work_dir,"/outputs")

data_dir                       = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/output"
mayors_data_dir                = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/3_create_education_data/output/data"
baseline_data_dir              = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/5_create_baseline_data/output/data"
covid_data_dir                 = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/4_create_covid_data/output/data"
tenure_data_dir                = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/input"


# Parameters --------------------------------------------------------------

data = "rdd_data_main_broad_definition" # Machado's STEM classification


# Running scripts ---------------------------------------------------------

source("code/01_create_dataset.R")
