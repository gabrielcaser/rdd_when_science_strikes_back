# Description - This code reproduces all tables and figures from the paper

# TO DO
## Versions - Create a pdf for each data


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
library("gt")           # better tables
library('geobr')        # to create maps
library('skimr')        # to create sumstats
library('rdrobust')     # to run rdd
library('patchwork')    # to create figures with plots together
library('knitr')        # render presentations
library('forcats')      # better figures
library('plm')          # regressions with fixed effects

# Setting -----------------------------------------------------------------

output_dir                     = paste0(getwd(),"/outputs")
data_dir                       = paste0(getwd(),"/data")

mayors_data_dir                = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/3_create_education_data/output/data"
baseline_data_dir              = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/5_create_baseline_data/output/data"
covid_data_dir                 = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/4_create_covid_data/output/data"
tenure_data_dir                = "C:/Users/wb633398/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/input"


# Parameters --------------------------------------------------------------
stem_definition        = "broad" # "broad" or "strict"
non_stem_college       = "college_mayors_only"   # "college_mayors_only" OR "all"
cohort_filter          = "" # "2016_" or ""

data   = paste0("rdd_data_", non_stem_college,"_", cohort_filter, stem_definition, "_definition.Rds") # Machado's STEM classification, both 2016 and 2020 cohorts and only considering municipalities where de NON-STEM mayor had college education

poli                   = 1                      # Functional form
janela                 = 0.05                   # Defining margin of victory for robustness tests
k                      = "triangular"           # Kernel  
deaths_and_hosp_in_log = "no"                  # Hospita. and Deaths as log(outcome + 1)

#covariates = # definir
# Running scripts ---------------------------------------------------------

#source("code/01_create_dataset.R")
#source("code/02_sum_stats.R")
#source("code/03_regressions_main.R")
#source("code/04_regressions_moderation.R")
