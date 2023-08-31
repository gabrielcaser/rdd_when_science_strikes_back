# Program - This program creates the dataset for the machine learning methods 

# TODO
## Estou excluindo os Non-STEM na linha 52. Mas tenho que arranjar algum jeito de manter esses caras depois eu acho. Talvez eu possa já criar a variável de educação do Non-STEM aqui
### Isso me faz perceber que eu estava fazendo errado no primeiro código ao comparar as variáveis a nível do candidato. Eu achei que estava comparando a idade, genero, etc dos candidatos STEM vs. candidatos Non-STEM, mas na real eu estava comparando essas características entre STEM eleitos vs. STEM não eleitos.
### Enfim, tenho que decidir o que fazer, mas definitivamente não posso jogar todo mundo fora que não é STEM ainda, a não ser que eu já deixe a penas uma linha por municipio aqui e crie colunas idade_eleito genero_eleito etc. Se eu criar variáveis vou ter que excluir na hora do machine learning.

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


## Occupation and electoral dataset

df_occ_ele_2020 <- read.csv(paste0(create_occupation_data_dir, "/base_rdd_covid_stem_2020.csv"), sep = ",")[ , c("id_municipio", "sigla_uf", "cpf", "nome", "resultado", "sigla_partido", "instrucao", "ocupacao", "genero", "raca", "idade", "tenure", "tenure_rais", "hours", "cbo_2002", "stem_job", "ano", "n_stem_candidates", "X2_lugar", "X3_lugar", "dif_votos_2_lugar", "dif_votos_3_lugar")]

df_occ_ele_2016 <- read.csv(paste0(create_occupation_data_dir, "/base_rdd_covid_stem_2016.csv"), sep = ",")[ , c("id_municipio", "sigla_uf", "cpf", "nome", "resultado", "sigla_partido", "instrucao", "ocupacao", "genero", "raca", "idade", "tenure", "tenure_rais", "hours", "cbo_2002", "stem_job", "ano", "n_stem_candidates", "X2_lugar", "X3_lugar", "dif_votos_2_lugar", "dif_votos_3_lugar")]

### Appending years datasets

df_occ_ele <- rbind(df_occ_ele_2016, df_occ_ele_2020)

### completing IDs with leading 0 until they achieve 11 characters
df_occ_ele$cpf <- str_pad(df_occ_ele$cpf, 11, pad = "0")

## Adding ownership data

df_bens <- read.csv(paste0(work_dir, "/input/220928_bens_candidatos.csv"), sep = ",")[ , c("cpf", "coorte", "bens_candidato")]


### completing IDs with leading 0 until they achieve 11 characters
df_bens$cpf <- str_pad(df_bens$cpf, 11, pad = "0")

df_bens <- df_bens %>% 
  summarise(cpf, ano = coorte, bens_candidato)

### Merging 

df_occ_ele <- left_join(df_occ_ele, df_bens, by = c("cpf", "ano"))

#### keeping only candidates with stem occupations
#
#df_occ_ele <- df_occ_ele %>% 
#  filter(stem_job == 1)

## Merging with hand coded (education) data
df <- left_join(df_occ_ele, df_education, by = 'cpf') # 5 are not merging 

# Removing datasets
rm(df_education, df_occ_ele_2016, df_occ_ele_2020, df_bens)

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
            stem_job,
            id_municipio,
            sigla_uf,
            resultado,
            sigla_partido,
            instrucao,
            ocupacao,
            bens_candidato,
            genero,
            raca,
            idade,
            ano,
            n_stem_candidates,
            X2_lugar,
            X3_lugar,
            dif_votos_2_lugar,
            dif_votos_3_lugar)


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

# Creating Non-STEM | STEM datasets

df_non_stem <-  df %>% 
  filter(stem_job != 1)

df <- df %>% 
 filter(stem_job == 1)

## Creating dummy columns for categorical variables (useful for running the algorithms)
df <- dummy_cols(df, select_columns = c("instrucao","ocupacao","sigla_partido","state", "cbo_agregado", "cbo_2002"))

df <- clean_names(df) # cleaning variable names so there are no duplicates
df_non_stem <- clean_names(df_non_stem) 

# Saving ------------------------------------------------------------------

saveRDS(df, file = paste(output_dir,"/data/230829_masked_ml_dataset_stem.Rds", sep = ""))

saveRDS(df_non_stem, file = paste(output_dir,"/data/230829_masked_ml_dataset_non_stem.Rds", sep = ""))
