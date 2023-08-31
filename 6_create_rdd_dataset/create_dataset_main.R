# Program - This program merges all data to create the rdd dataset 


# TO DO -------------------------------------------------------------------

## VOLTAR NA RUNNING  VARIABLE


# Initial commands

rm(list = ls(all.names = TRUE)) # clear objects
gc() # free up memory

# Libs --------------------------------------------------------------------

library(tidyverse)
library(skimr)
library(readxl)

# Directories

work_dir                   = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset"
output_dir                 = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/output"
baseline_data_dir          = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/5_create_baseline_data/output/data"
covid_data_dir             = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/4_create_covid_data/output/data"
mayors_data_dir            = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/3_create_education_data/output/data"

set.seed(1234) # making it reproducible

# Mayors Data ----------------------------------------------------

df_mayors <- readRDS(paste0(mayors_data_dir, "/candidates_dataset.Rds"))

df_mayors$id_municipio <- as.character(df_mayors$id_municipio) # changing data type
df_mayors$id_municipio <- substr(df_mayors$id_municipio,1,6) # keeping only 6 first digits

# Identify factor columns
factor_columns <- sapply(df_mayors, is.factor)

# Convert factor columns to character
df_mayors[factor_columns] <- lapply(df_mayors[factor_columns], as.character)


## Choosing the definition of STEM background (treatment) -----------------------------------------

df_mayors <- df_mayors %>%
  mutate(stem_background = as.numeric((stem_job == 1 & curso_stem == 1)),
         dif_votos_segundo = dif_votos_2_lugar,
         dif_votos_terceiro = dif_votos_3_lugar) # 616 candidates with stem background

## Turn into wide format

df_mayors$resultado <- ifelse(df_mayors$x2_lugar == 'True', 'segundo', df_mayors$resultado) # there is 1 second place more than elected
df_mayors$resultado <- ifelse(df_mayors$x3_lugar == 'True', 'terceiro', df_mayors$resultado)

## removing municipalities with more than one 2 or 3 place (I need to investigate why this is happening. Probably they achieved the same number of votes)

df_mayors <- df_mayors %>% 
  filter(id_municipio != '311920' & id_municipio != '410380')


df_mayors <- pivot_wider(df_mayors, id_cols = c('id_municipio', 'coorte', 'dif_votos_segundo', 'dif_votos_terceiro'), names_from =  'resultado', values_from = c('stem_background','tenure', 'hours', 'tenure_rais', 'cbo_2002', 'cbo_agregado', 'sigla_partido', 'instrucao', 'ocupacao', 'genero', 'raca', 'idade', 'stem_job', 'curso_stem') )

## Keeping only municipalities where at least 1 candidate has a STEM background

df_mayors <- df_mayors %>%
  group_by(id_municipio, coorte) %>% 
  mutate(n_stem_background = sum(stem_background_eleito, stem_background_segundo, stem_background_terceiro, na.rm = TRUE)) %>% 
  ungroup()

df_mayors <- df_mayors %>% 
  filter(n_stem_background >= 1)

## Defining cities to use third most voted candidate 
df_mayors$use_third <- ifelse(df_mayors$stem_background_eleito != 1 & df_mayors$stem_background_segundo != 1 & df_mayors$stem_background_terceiro == 1, 1, 0)
df_mayors$use_third <- ifelse(df_mayors$stem_background_eleito == 1 & df_mayors$stem_background_segundo == 1 & df_mayors$stem_background_terceiro != 1, 1, df_mayors$use_third)



## Replace values in variables ending with "_segundo" if use_third == 1

#df_mayors$stem_background_naoeleito <- ifelse(df_mayors$use_third != 1, df_mayors$stem_background_segundo, df_mayors$stem_background_terceiro)
#df_mayors$stem_background_naoeleito <- ifelse(df_mayors$use_third != 1, df_mayors$stem_background_segundo, df_mayors$stem_background_terceiro)

# Get a list of variable names ending with "_segundo"
segundo_columns <- colnames(df_mayors)[endsWith(colnames(df_mayors), "_segundo")]

# Iterate through the "_segundo" columns and replace values based on the condition
for (col_name in segundo_columns) {
  df_mayors[[col_name]] <- ifelse(df_mayors$use_third != 1, df_mayors[[col_name]], 
                                  df_mayors[[sub("_segundo", "_terceiro", col_name)]])
}

# Iterate through the "_segundo" columns and replace their names
for (col_name in segundo_columns) {
  new_col_name <- sub("_segundo", "_naoeleito", col_name)
  df_mayors <- df_mayors %>%
    rename(!!new_col_name := !!col_name)
}

# Drop variables ending with "_terceiro"
df_mayors <- df_mayors %>%
  select(-ends_with("_terceiro"))

# renaming variable
df_mayors <- df_mayors %>%
  rename(dif_votos = dif_votos_naoeleito)

# droping variables

df_mayors <- df_mayors %>%
  select(-use_third, -n_stem_background)

# changing data type

df_mayors$stem_background_eleito <- as.factor(df_mayors$stem_background_eleito)

# droping municipalities where 1 and 2 were STEM and had no 3 candidate

df_mayors <- df_mayors %>% 
  filter(!is.na(dif_votos))

# Baseline and NPI Data -----------------------------------------------------------

df_health <- readRDS(paste0(baseline_data_dir, "/health_data.Rds"))

df_ideology <- readRDS(paste0(baseline_data_dir, "/ideology_data.Rds"))

df_ideology <- df_ideology %>% 
  filter(coorte == 2016 | coorte == 2020) # keeping election coorte years

df_density <- readRDS(paste0(baseline_data_dir, "/density_data.Rds"))

df_political <- readRDS(paste0(baseline_data_dir, "/political_data.Rds"))

df_npi <- readRDS(paste0(baseline_data_dir, "/npi_data.Rds"))


# Covid Data --------------------------------------------------------------

df_covid <- readRDS(paste0(covid_data_dir, "/covid_data.Rds"))

df_covid <- df_covid %>% 
  ungroup()

# Merging datasets --------------------------------------------------------

df <- left_join(df_mayors, df_npi, by = c("id_municipio")) # 29% of municipalities with missing data. That is expected since not everyone responded the survey

df <- left_join(df, df_covid, by = c("id_municipio", "coorte")) # 4 municipalities with missing data

df <- left_join(df, df_health, by = c("id_municipio"))

df <- left_join(df, df_density, by = c("id_municipio")) # 2 municipalities with missing data

df <- left_join(df, df_ideology, by = c("id_municipio", "coorte"))


df <- df %>%
  rename(sigla_partido = sigla_partido_eleito)

df <- left_join(df, df_political, by = c("sigla_partido", "coorte"))

df <- df %>%
  rename(sigla_partido_eleito = sigla_partido,
         ideology_party_eleito = ideology_party)

df <- df %>%
  rename(sigla_partido = sigla_partido_naoeleito)

df <- left_join(df, df_political, by = c("sigla_partido", "coorte"))

df <- df %>%
  rename(sigla_partido_naoeleito = sigla_partido,
         ideology_party_naoeleito = ideology_party)

df$coorte <- as.factor(df$coorte)

rm(df_covid, df_density, df_health, df_ideology, df_mayors, df_npi, df_political) # removing dataset

### Creating "variable" of non_stem_candidate

df$he_non_stem_cdt = ifelse(df$stem_background_eleito == 1 & str_detect(df$instrucao_naoeleito, "ensino superior completo"), 1, 0)
df$he_non_stem_cdt = ifelse(df$stem_background_naoeleito == 1 & str_detect(df$instrucao_eleito, "ensino superior completo"), 1, df$he_non_stem_cdt)

df$sch_non_stem_cdt <= as.factor(df$he_non_stem_cdt)

### Dropping non-elected variables

# Drop variables ending with "_terceiro"
df <- df %>%
  select(-ends_with("_naoeleito"))

# Changing variable names

# Get a list of variable names ending with "_eleito"
eleito_columns <- colnames(df)[endsWith(colnames(df), "_eleito")]

# Iterate through the "_eleito" columns and replace their names
for (col_name in eleito_columns) {
  new_col_name <- sub("_eleito", "", col_name)
  df <- df %>%
    rename(!!new_col_name := !!col_name)
}

# Droping municipalties with null outcome variables

df <- df %>% 
  filter(!is.na(hosp_per_100k_inhabitants) | !is.na(deaths_sivep_per_100k_inhabitants))



### VOLTAR

# Creating running variable

df$dif_votos = ifelse(df$stem_job_4 == 1, df$dif_votos, -df$dif_votos)

# Creating candidate level variables

df <- df %>%
  mutate(reeleito = ocupacao == "prefeito")
df <- df %>%
  mutate(mulher = genero == "feminino")


df$ens_sup = ifelse(str_detect(df$instrucao, "ensino superior completo"),1,0)


#### crating confirmed cases previous year

df <- df %>%
  mutate(confirmed_per_100k_inhabitants_previous_year = confirmed_per_100k_inhabitants_total - confirmed_per_100k_inhabitants)


#### creating agregated occupation code 'cbo'

df <- df %>% 
  mutate(cbo_agregado_nome = dplyr::recode(cbo_agregado,
                                           "1223" = "Directors of works operations in construction",
                                           "1425" = "IT managers",
                                           "2124" = "IT analysts",
                                           "2142" = "Civil engineers",
                                           "2143" = "Electrical engineers",
                                           "2221" = "Agroforestry engineers",
                                           "2344" = "Uni. prof. of biological sciences",
                                           "3171" = "Systems and application technicias",
                                           "3172" = "Computer technicians",
                                           "2341" = "Uni. prof. of math, statistics and TI",
                                           .default = "Others"))




## Creating variables X e Y E T

df$X = df$dif_votos

df$Y_deaths = df$deaths_per_100k_inhabitants

df$Y_deaths_sivep = df$deaths_sivep_per_100k_inhabitants

df$Y_cases = df$confirmed_per_100k_inhabitants

df$Y_hosp = df$hosp_per_100k_inhabitants


## Creating dummy = 1 if STEM candidate won

df$T = ifelse(df$X >= 0, 1, 0)

df$T_X = df$X * df$T

# Cleaning the data -------------------------------------------------------

#df<- df %>% 
#  filter(coorte == 2016 & sch_non_stem_cdt == 1)

df <- df %>% 
  summarise(coorte,
            sigla_uf,
            city,
            id_municipio,
            resultado,
            situacao,
            nome,
            nome_urna,
            cpf,
            instrucao,
            ocupacao,
            graduacao_stem,
            cbo_2002,
            cbo_agregado,
            cbo_agregado_nome,
            stem_job_4,
            medico,
            tenure,
            sigla_partido,
            idade,
            genero,
            estimated_population,
            populacao_2010,
            densidade,
            taxa_analfabetismo_18_mais,
            indice_gini,
            idhm,
            renda_pc,
            per_populacao_urbana,
            per_populacao_homens,
            total_vacinados,
            per_vacinados,
            tx_med_ch,
            tx_med,
            tx_enf_ch,
            tx_enf,
            pct_desp_recp_saude_mun,
            cob_esf,
            tx_leito_sus,
            ideology_party,
            ideology_municipality,
            reeleito,
            mulher,
            ens_sup,
            sch_non_stem_cdt,
            X,
            Y_deaths_sivep,
            Y_cases,
            Y_hosp,
            barreiras_sanitarias,
            mascaras,
            restricao_atv_nao_essenciais,
            restricao_circulacao,
            restricao_transporte_publico,
            total_nfi)

df <- df %>% 
  dplyr::filter(situacao != "cassado com recurso" & situacao != "indeferido")

#df %>% 
#dplyr::group_by(id_municipio, coorte) %>% 
# count() %>% 
# arrange(desc(n))

df$pop_maior_200k = ifelse(df$populacao_2010 >= 200000, 1, 0)

unique(df$instrucao)

df <- df %>%
  mutate(instrucao = dplyr::recode(instrucao,
                                   "ensino superior completo" = 7,
                                   "ensino superior incompleto" = 6,
                                   "ensino medio completo" = 5,
                                   "ensino medio incompleto" = 4,
                                   "ensino fundamental completo" = 3,
                                   "ensino fundamental incompleto" = 2,
                                   "le e escreve" = 1))

library(vtable)
st(df)

# Treating null values ----------------------------------------------------



##df <- df %>%
##  group_by(coorte, stem_job_4) %>% 
##  mutate(mascaras = replace_na(mascaras, mean(mascaras, na.rm = TRUE)),
##         restricao_atv_nao_essenciais = replace_na(restricao_atv_nao_essenciais, mean(restricao_atv_nao_essenciais, na.rm = TRUE)),
##         restricao_circulacao = replace_na(restricao_circulacao, mean(restricao_circulacao, na.rm = TRUE)),
##         restricao_transporte_publico = replace_na(restricao_transporte_publico, mean(restricao_transporte_publico, na.rm = TRUE)),
##         barreiras_sanitarias = replace_na(barreiras_sanitarias, mean(barreiras_sanitarias, na.rm = TRUE)),
##         Y_deaths_sivep = replace_na(Y_deaths_sivep, mean(Y_deaths_sivep, na.rm = TRUE)),
##         Y_hosp = replace_na(Y_hosp, mean(Y_hosp, na.rm = TRUE))) %>% 
##    ungroup()
##
##
##df <- df %>%
##  group_by(id_municipio, coorte) %>% 
##  mutate(total_nfi = sum(barreiras_sanitarias, mascaras, restricao_atv_nao_essenciais, restricao_circulacao, restricao_transporte_publico)) %>% 
##  ungroup()
##
##df <- df %>% 
## filter(!is.na(Y_hosp))


df <- df %>%
  dplyr::group_by(id_municipio, coorte) %>% 
  dplyr::mutate(total_nfi = sum(barreiras_sanitarias, mascaras, restricao_atv_nao_essenciais, restricao_circulacao, restricao_transporte_publico, na.rm = FALSE)) %>% 
  dplyr::ungroup()


# Resampling --------------------------------------------------------------



df <- df %>% 
  filter(coorte == 2016 & sch_non_stem_cdt == 1 & ens_sup == 1)
amostra <- cbind(df$coorte == 2016, df$sch_non_stem_cdt == 1, df$ens_sup == 1)
poli = 1
covsZ = cbind(state.d)



# Saving ------------------------------------------------------------------



saveRDS(df, file = paste(work_dir, "/output/data/rdd_data_main.rds", sep = ""))

