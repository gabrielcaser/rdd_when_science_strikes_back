# Program - This program merges all data to create the rdd dataset 


# TO DO -------------------------------------------------------------------

## Ver por que não tenho mais a variável STEMJOB e só STEMJOB_ML


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

df_mayors <- df_mayors %>% 
  ungroup()

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

df <- left_join(df, df_political, by = c("sigla_partido", "coorte"))

df$coorte <- as.factor(df$coorte)

# Choosing the definition of STEM -----------------------------------------

df <- df %>%
  mutate(stem_job_2 = stem_job == 1 | ocupacao == "medico" | ocupacao == "engenheiro" | ocupacao == "biomedico"| ocupacao == "quimico" | ocupacao == "biologo" | ocupacao == "estatistico" | ocupacao == "tecnico em informatica") %>% 
  mutate(stem_job_3 = stem_job_2 == 1 & ocupacao != "medico") %>% 
  mutate(medico = ocupacao == "medico") %>% 
  mutate(stem_job_4 = (stem_job == 1  & tenure > 0 & curso_stem_ml == 1))



df <- df %>% 
  #dplyr::filter((resultado == "eleito" | X2_lugar == 'True')) %>%  # escolhendo só o primeiro e o segundo colocado
  dplyr::group_by(id_municipio, coorte) %>% 
  dplyr::mutate(rdd1 = ifelse(sum(stem_job == 1) == 1, 1, 0),
                rdd2 = ifelse(sum(stem_job_2 == 1) == 1, 1, 0),
                rdd3 = ifelse(sum(stem_job_3 == 1) == 1, 1, 0),
                rdd4 = ifelse(sum(stem_job_4 == 1) == 1, 1, 0))

df %>% 
  dplyr::filter(rdd4 == 1)


### Creating "variables"


df$he_non_stem_cdt = ifelse(str_detect(df$instrucao, "ensino superior completo") & df$stem_job_4 == 0 & df$rdd4 == 1, 1, 0)


df <- df %>% 
  dplyr::group_by(id_municipio, coorte) %>%
  dplyr::mutate(stem_cdt_tenure = sum(tenure),
                deaths_per_100k_inhabitants = (deaths / estimated_population) * 100000,
                hosp_per_100k_inhabitants = (hosp_sivep / estimated_population) * 100000,
                deaths_sivep_per_100k_inhabitants = (deaths_sivep / estimated_population) * 100000,
                sch_non_stem_cdt = sum(he_non_stem_cdt, na.rm = TRUE),
                #delta_hosp_per_100k_inhabitants = (delta_hosp_sivep / estimated_population) * 100000,
                #deaths_per_100k_inhabitants = (delta_deaths_sivep / estimated_population) * 100000,
  )



df2 <- df %>%
  dplyr::filter(resultado == "eleito")

df2 <- df2 %>% 
  mutate(cbo_agregado = substr(cbo_2002,1,4))

df2 <- df2 %>% 
  dplyr::filter(situacao != "cassado com recurso" & situacao != "indeferido")



df_stem <- df %>%
  dplyr::filter(rdd4 == 1)

df %>% 
  summarise(coorte,id_municipio,rdd,rdd1,rdd4) %>% 
  arrange(desc(rdd))

df <- df %>%
  dplyr::filter(rdd4 == 1)


## Criando tabela de candidatos stem

stem_eleitos <- df_stem %>% 
  dplyr::filter(stem_job_4 == 1) %>% 
  summarise(coorte,
            resultado,
            id_municipio,
            city,
            state,
            sigla_partido,
            cpf,
            nome,
            ocupacao,
            instrucao,
            cbo_2002,
            tenure,
            idade,
            genero)


#write.table(stem_eleitos, "C:\\Users\\GabrielCaserDosPasso\\Documents\\RAIS\\Dados\\Output\\220922_stem_eleitos.csv", sep = ',', row.names = FALSE, fileEncoding = "latin1")

# voltando

df <- df %>%
  dplyr::filter(resultado == "eleito")



df$dif_votos = ifelse(df$stem_job_4 == 1, df$dif_votos, -df$dif_votos)


df <- df %>%
  mutate(reeleito = ocupacao == "prefeito")
df <- df %>%
  mutate(mulher = genero == "feminino")

df <- df %>% 
  mutate(cbo_agregado = substr(cbo_2002,1,4))

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

