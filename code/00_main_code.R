
# Description -------------------------------------------------------------

## This code reproduces all tables and figures from the paper


# Initial Commands --------------------------------------------------------

rm(list = ls(all.names = TRUE)) # clear objects
gc() # free up memory


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
work_dir                       = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/sum_stats"
output_dir                     = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/sum_stats/output"
data_dir                       = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/output"

# Running scripts ---------------------------------------------------------


