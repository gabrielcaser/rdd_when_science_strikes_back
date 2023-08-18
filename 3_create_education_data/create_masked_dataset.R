# Program - This program creates the dataset for the machine learning methods 

# TODO
## Quando terminar de rodar os códigos em 2, tenho que rodar aqui de novo


# Libs --------------------------------------------------------------------

library(skimr)
library(pROC)
library(tree)
library(randomForest)
library(fastAdaboost)
library(naniar)
library(fastDummies)
library(tidyverse)
library(janitor)

## Directories
work_dir                   = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/3_create_education_data"
output_dir                 = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/3_create_education_data/output"
create_electoral_data_dir  = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/1_create_electoral_data/output/data"
create_occupation_data_dir = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/2_create_occupation_data/output/data"

set.seed(1234) # making it reproducible

# Data --------------------------------------------------------------------
# Creating training dataset

## hand coded data
df_education <- read.csv(paste0(work_dir, "/input/230216_stem_eleitos_formacao.csv"), nrows = 302)[ , c("cpf", "graduacao", "graduacao_stem")] # This dataset was created manually. We searched for mayors' undergraduate degree on municipalities' website, social media and specialized websites (Escavador.com.br)

### completing IDs with leading 0 until they achieve 11 characters
df_education$cpf <- str_pad(df_education$cpf, 11, pad = "0")

##### electoral data
###df_electoral <- read.csv(paste0(create_electoral_data_dir, "/df_candidatos_2020_clean.csv"), sep = ";"###)[ , c("id_municipio", "sigla_uf", "cpf", "nome", "resultado", "sigla_partido", "instrucao", ###"ocupacao", "genero", "raca", "idade", "ano")]
###
###### completing IDs with leading 0 until they achieve 11 characters
###df_electoral$cpf <- str_pad(df_electoral$cpf, 11, pad = "0")


## Occupation and electoral dataset

df_occ_ele_2020 <- read.csv(paste0(create_occupation_data_dir, "/base_rdd_covid_stem_2020.csv"), sep = ",")[ , c("id_municipio", "sigla_uf", "cpf", "nome", "resultado", "sigla_partido", "instrucao", "ocupacao", "genero", "raca", "idade", "tenure", "tenure_rais", "hours", "cbo_2002", "stem_job", "ano")]

df_occ_ele_2016 <- read.csv(paste0(create_occupation_data_dir, "/base_rdd_covid_stem_2016.csv"), sep = ",")[ , c("id_municipio", "sigla_uf", "cpf", "nome", "resultado", "sigla_partido", "instrucao", "ocupacao", "genero", "raca", "idade", "tenure", "tenure_rais", "hours", "cbo_2002", "stem_job", "ano")]

### Appending years datasets

df_occ_ele <- rbind(df_occ_ele_2016, df_occ_ele_2020)

### completing IDs with leading 0 until they achieve 11 characters
df_occ_ele$cpf <- str_pad(df_occ_ele$cpf, 11, pad = "0")

### keeping only candidates with stem occupations

df_occ_ele <- df_occ_ele %>% 
  filter(stem_job == 1)

## Merging with hand coded (education) data
df <- left_join(df_occ_ele, df_education, by = 'cpf') # 5 are not merging 

# Removing datasets
rm(df_education, df_occ_ele, df_occ_ele_2016, df_occ_ele_2020)

# Masking -----------------------------------------------------------------

df$id_masked <- as.numeric(as.factor(df$cpf))

## completing IDs with leading 0 until they achieve 11 characters
df$id_masked <- str_pad(df$id_masked, 11, pad = "0")

# Treating data  -----------------------------------------

df <- df %>% 
  summarise(id_masked,
            tenure,
            tenure_rais,
            hours,
            cbo_2002,
            graduacao, 
            graduacao_stem,
            id_municipio,
            sigla_uf,
            resultado,
            sigla_partido,
            instrucao,
            ocupacao,
            genero,
            raca,
            idade,
            ano)


df <- df %>%  # renaming
  rename(state = sigla_uf)

## declaring as missing the education variable for candidates that we didn't found 
df <- df %>%
  replace_with_na(replace = list(graduacao_stem = c("?","-")))

## changing data types

df$state <- as.factor(df$state)
df$sigla_partido <- as.factor(df$sigla_partido)
df$instrucao <- as.factor(df$instrucao)
df$cbo_2002 <- as.factor(df$cbo_2002)
df$graduacao <- as.factor(df$graduacao)
df$ocupacao <- as.factor(df$ocupacao)
df$graduacao_stem <- as.factor(df$graduacao_stem)

## creating agreggate data for occupations
df <- df %>% 
  mutate(cbo_agregado = as.factor(substring(cbo_2002, 0,4)))

## Creating dummy columns for categorical variables (useful for running the algorithms)
df <- dummy_cols(df, select_columns = c("instrucao","ocupacao","sigla_partido","state", "cbo_agregado", "cbo_2002"))

df <- clean_names(df) # cleaning variable names so there are no duplicates


# Saving ------------------------------------------------------------------

saveRDS(df, file = paste(output_dir,"/data/230817_masked_ml_dataset.Rds", sep = ""))

