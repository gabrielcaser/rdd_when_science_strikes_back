# Program - This program merges all data to create the rdd dataset 


# TO DO -------------------------------------------------------------------

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



# Loop to run code with different definitions of "stem_background"
for (definition in c("strict", "broad")) {
  
  # Mayors Data ----------------------------------------------------
  
  df_mayors <- readRDS(paste0(mayors_data_dir, "/candidates_dataset.Rds"))
  
  df_mayors$id_municipio <- as.character(df_mayors$id_municipio) # changing data type
  df_mayors$id_municipio <- substr(df_mayors$id_municipio,1,6) # keeping only 6 first digits
  
  # Identify factor columns
  factor_columns <- sapply(df_mayors, is.factor)
  
  # Convert factor columns to character
  df_mayors[factor_columns] <- lapply(df_mayors[factor_columns], as.character)
  
  # Modify "stem_background" definition based on the loop iteration
  if (definition == "strict") {
    df_mayors <- df_mayors %>%
      dplyr::mutate(stem_background = as.numeric((stem_job == 1 & curso_stem == 1)),
                    dif_votos_segundo = dif_votos_2_lugar,
                    dif_votos_terceiro = dif_votos_3_lugar) # 616 candidates with stem background
  } else {
    df_mayors <- df_mayors %>%
      dplyr::mutate(stem_background = as.numeric((stem_job == 1)),
                    dif_votos_segundo = dif_votos_2_lugar,
                    dif_votos_terceiro = dif_votos_3_lugar) # 616 candidates with stem background
  }
  
  ## Choosing the definition of STEM background (treatment) -----------------------------------------
  

  
  ## Turn into wide format
  
  df_mayors$resultado <- ifelse(df_mayors$x2_lugar == 'True', 'segundo', df_mayors$resultado) # there is 1 second place more than elected
  df_mayors$resultado <- ifelse(df_mayors$x3_lugar == 'True', 'terceiro', df_mayors$resultado)
  
  ## removing municipalities with more than one 2 or 3 place (I need to investigate why this is happening. Probably they achieved the same number of votes)
  
  df_mayors <- df_mayors %>% 
    dplyr::filter(id_municipio != '311920' & id_municipio != '410380')
  
  
  df_mayors <- pivot_wider(df_mayors, id_cols = c('id_municipio', 'coorte', 'dif_votos_segundo', 'dif_votos_terceiro'), names_from =  'resultado', values_from = c('stem_background','tenure', 'hours', 'tenure_rais', 'cbo_2002', 'cbo_agregado', 'sigla_partido', 'instrucao', 'ocupacao', 'genero', 'raca', 'idade', 'stem_job', 'curso_stem') )
  
  ## Keeping only municipalities where at least 1 candidate has a STEM background
  
  df_mayors <- df_mayors %>%
    dplyr::group_by(id_municipio, coorte) %>% 
    dplyr::mutate(n_stem_background = sum(stem_background_eleito, stem_background_segundo, stem_background_terceiro, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  df_mayors <- df_mayors %>% 
    dplyr::filter(n_stem_background >= 1)
  
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
    colnames(df_mayors)[colnames(df_mayors) == col_name] <- new_col_name
  }
  
  
  # Drop variables ending with "_terceiro"
  df_mayors <- df_mayors %>%
    dplyr::select(-ends_with("_terceiro"))
  
  # renaming variable
  df_mayors <- df_mayors %>%
    dplyr::rename(dif_votos = dif_votos_naoeleito)
  
  # droping variables
  
  df_mayors <- df_mayors %>%
    dplyr::select(-use_third, -n_stem_background)
  
  # changing data type
  
  df_mayors$stem_background_eleito <- as.factor(df_mayors$stem_background_eleito)
  
  # droping municipalities where 1 and 2 were STEM and had no 3 candidate
  
  df_mayors <- df_mayors %>% 
    dplyr::filter(!is.na(dif_votos))
  
  # Baseline and NPI Data -----------------------------------------------------------
  
  df_health <- readRDS(paste0(baseline_data_dir, "/health_data.Rds"))
  
  df_ideology <- readRDS(paste0(baseline_data_dir, "/ideology_data.Rds"))
  
  df_ideology <- df_ideology %>% 
    dplyr::filter(coorte == 2016 | coorte == 2020) # keeping election coorte years
  
  df_density <- readRDS(paste0(baseline_data_dir, "/density_data.Rds"))
  
  df_political <- readRDS(paste0(baseline_data_dir, "/political_data.Rds"))
  
  df_npi <- readRDS(paste0(baseline_data_dir, "/npi_data.Rds"))
  
  df_stem_eleito <- readRDS(paste0(baseline_data_dir, "/stem_classification_eleito.Rds"))
  
  df_stem_naoeleito <- readRDS(paste0(baseline_data_dir, "/stem_classification_naoeleito.Rds"))
  
  # Covid Data --------------------------------------------------------------
  
  df_covid <- readRDS(paste0(covid_data_dir, "/covid_data.Rds"))
  
  df_covid <- df_covid %>% 
    dplyr::ungroup()
  
  
  # Tenure data -------------------------------------------------------------
  
  df_tenure <- readRDS(paste0(work_dir, "/input/tenure_data.Rds"))
  
  # Merging datasets --------------------------------------------------------
  
  df <- left_join(df_mayors, df_npi, by = c("id_municipio")) # 29% of municipalities with missing data. That is expected since not everyone responded the survey
  
  df <- left_join(df, df_covid, by = c("id_municipio", "coorte")) # 4 municipalities with missing data
  
  df <- left_join(df, df_health, by = c("id_municipio"))
  
  df <- left_join(df, df_density, by = c("id_municipio")) # 2 municipalities with missing data
  
  df <- left_join(df, df_ideology, by = c("id_municipio", "coorte"))
  
  df <- left_join(df, df_tenure, by = c('id_municipio', 'coorte'))
  
  df <- left_join(df, df_stem_eleito, by = c('cbo_agregado_eleito'))
  
  df <- left_join(df, df_stem_naoeleito, by = c('cbo_agregado_naoeleito'))
  
  df <- df %>%
    dplyr::rename(sigla_partido = sigla_partido_eleito)
  
  df <- left_join(df, df_political, by = c("sigla_partido", "coorte"))
  
  df <- df %>%
    dplyr::rename(sigla_partido_eleito = sigla_partido,
                  ideology_party_eleito = ideology_party)
  
  df <- df %>%
    dplyr::rename(sigla_partido = sigla_partido_naoeleito)
  
  df <- left_join(df, df_political, by = c("sigla_partido", "coorte"))
  
  df <- df %>%
    dplyr::rename(sigla_partido_naoeleito = sigla_partido,
                  ideology_party_naoeleito = ideology_party)
  
  df$coorte <- as.factor(df$coorte)
  
  rm(df_covid, df_density, df_health, df_ideology, df_mayors, df_npi, df_political, df_tenure) # removing dataset
  
  
  ### Creating "variable" of non_stem_candidate
  
  df$he_non_stem_cdt = ifelse(df$stem_background_eleito == 1 & str_detect(df$instrucao_naoeleito, "ensino superior completo"), 1, 0)
  df$he_non_stem_cdt = ifelse(df$stem_background_naoeleito == 1 & str_detect(df$instrucao_eleito, "ensino superior completo"), 1, df$he_non_stem_cdt)
  
  df$sch_non_stem_cdt <- as.logical(df$he_non_stem_cdt)
  
  ### Dropping non-elected variables
  
  # Drop variables ending with "_terceiro"
  df <- df %>%
    select(-ends_with("_naoeleito"))
  
  # Changing variable names
  
  # Get a list of variable names ending with "_eleito"
  eleito_columns <- colnames(df)[endsWith(colnames(df), "_eleito")]
  
  # Iterate through the "_eleito" columns and remove the suffix
  for (col_name in eleito_columns) {
    new_col_name <- sub("_eleito", "", col_name)
    colnames(df)[colnames(df) == col_name] <- new_col_name
  }
  
  
  # Droping municipalties with null outcome variables
  
  df <- df %>% 
    filter(!is.na(hosp_per_100k_inhabitants) | !is.na(deaths_sivep_per_100k_inhabitants))
  
  
  # Creating running variable
  
  df$dif_votos = ifelse(df$stem_background == 1, df$dif_votos, -df$dif_votos)
  
  # Creating candidate level variables
  
  df <- df %>%
    dplyr::mutate(reeleito = ocupacao == "prefeito")
  df <- df %>%
    dplyr::mutate(mulher = genero == "feminino")
  
  
  df$ens_sup = as.logical(ifelse(str_detect(df$instrucao, "ensino superior completo"),1,0))
  
  
  #### creating agregated occupation code 'cbo'
  
  df <- df %>% 
    dplyr::mutate(cbo_agregado_nome = dplyr::recode(cbo_agregado,
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
  
  df$T = ifelse(df$X >= 0, 1, 0)
  
  df$T_X = df$X * df$T
  
  
  # Cleaning the data -------------------------------------------------------
  
  #df<- df %>% 
  #  filter(coorte == 2016 & sch_non_stem_cdt == 1)
  
  df <- df %>% 
    dplyr::summarise(coorte,
                     sigla_uf,
                     id_municipio,
                     instrucao,
                     ocupacao,
                     curso_stem,
                     cbo_2002,
                     cbo_agregado,
                     cbo_agregado_nome,
                     cbo_agregado_nome_caser.laverde.rothwell,
                     stem_background,
                     #medico,
                     tenure,
                     tenure_rais,
                     tenure_old,
                     situacao,
                     sigla_partido,
                     idade,
                     genero,
                     populacao,
                     densidade,
                     #taxa_analfabetismo_18_mais,
                     #indice_gini,
                     idhm,
                     renda_pc,
                     per_populacao_urbana,
                     per_populacao_homens,
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
                     Y_hosp,
                     T,
                     T_X,
                     barreiras_sanitarias,
                     mascaras,
                     restricao_atv_nao_essenciais,
                     restricao_circulacao,
                     restricao_transporte_publico,
                     total_nfi)
  
  
  df <- df %>%
    dplyr::mutate(instrucao = dplyr::recode(instrucao,
                                            "ensino superior completo" = 7,
                                            "ensino superior incompleto" = 6,
                                            "ensino medio completo" = 5,
                                            "ensino medio incompleto" = 4,
                                            "ensino fundamental completo" = 3,
                                            "ensino fundamental incompleto" = 2,
                                            "le e escreve" = 1))
  
  # Defining the sample --------------------------------------------------------------
  
  ## For now I'm onlu comparing municipalities whose
  ### STEM and Non-STEM candidates had a college degree
  
  df_moderation <- df
  
  df <- df %>% 
    dplyr::filter(sch_non_stem_cdt == 1 & coorte == 2016)
  
  
  
  # Saving ------------------------------------------------------------------
  
  
  saveRDS(df, file = paste(work_dir, "/output/data/rdd_data_main_", definition, "definition.rds", sep = ""))
  saveRDS(df_moderation, file = paste(work_dir, "/output/data/rdd_data_moderation_", definition, "definition.rds", sep = ""))
  
  write.csv(df_moderation, file = paste0(work_dir, "/output/data/rdd_data_moderation_", definition, "definition.csv"), row.names = FALSE, na = ".")
  
}

