# Program - This program creates a dataset with SRAG(Covid) outcomes per day and classifies municipalities as STEM or not

# TODO

## entender como calcular a média de mortes por 100k habitantes por mês

# Initial commands

rm(list = ls(all.names = TRUE)) # clear objects
gc() # free up memory

# Libs --------------------------------------------------------------------

library(tidyverse)
library(skimr)
library(writexl)
library(haven) # read .dta files

# Directories

work_dir                            = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/sum_stats"
output_dir                          = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/sum_stats/output"
create_dataset_for_regressions_dir  = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/output"
create_covid_data_dir               = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/4_create_covid_data"
set.seed(1234) # making it reproducible



# Oppening data -----------------------------------------------------

## Covid data
data_list <- c()
df_covid <- readRDS(paste0(create_covid_data_dir, "/output/data/covid_day_data.rds"))
df_covid <- df_covid %>% 
  mutate(coorte = case_when(as.numeric(format(df_covid$DT_SIN_PRI, "%Y")) == 2020 ~ as.factor(2016),
                            as.numeric(format(df_covid$DT_SIN_PRI, "%Y")) == 2021 ~ as.factor(2020)),
         month = as.numeric(format(df_covid$DT_SIN_PRI, "%m")))

## Population

df_population <- read.csv2(paste0(create_covid_data_dir, "/input/populacao.csv"), sep = ",") # source: https://iepsdata.org.br/data-downloads
df_population <- df_population %>%
  mutate(coorte = recode(ano, '2020' = '2016', '2021' = '2020')) %>% 
  reframe(coorte = as.factor(coorte), populacao, id_municipio = as.character(id_municipio))

## Paper data

df_paper <- read_dta(paste0(work_dir,"/input/paper_data.dta"))

df_paper <- df_paper %>% 
  reframe(id_municipio = id_mun6,
          date,
          indice_isolamento,
          npi_intensity = dummy_intensity,
          DT_SIN_PRI = date)

## Runing loop

for (definition in c("strict", "broad")) {
  for (var in c("deaths", "hosp", "deaths_per100k", "hosp_per100k", "deaths_per100k_acum", "hosp_per100k_acum", "deaths_acum", "hosp_acum", "indice_isolamento", "npi_intensity")) {
    ## STEM background data
    df_stem <- readRDS(paste(create_dataset_for_regressions_dir, "/data/rdd_data_moderation_", definition, "definition.rds", sep = ""))
    
    df_stem <- df_stem %>% 
      reframe(id_municipio, coorte, stem_background, sch_non_stem_cdt, X)
    
    ## Merging
    
    df <- left_join(df_stem, df_covid, by = c("id_municipio","coorte"))
    df <- left_join(df, df_population, by = c("id_municipio","coorte"))
    df <- left_join(df, df_paper,      by = c("id_municipio","DT_SIN_PRI"))
    
    df <- df %>%
      mutate(rdd_window = case_when(
        abs(X) <= 0.03 ~ 3,
        abs(X) <= 0.05 ~ 5,
        abs(X) <= 0.07 ~ 7,
        abs(X) <= 0.09 ~ 9,
        abs(X) <= 0.11 ~ 11,
        abs(X) <= 0.13 ~ 13,
        abs(X) <= 0.15 ~ 15,
        abs(X) <= 1.00 ~ 100,
        TRUE ~ NA  # If X doesn't meet any condition, assign NA
      ))
    
    
    df_final <- df %>%
      group_by(id_municipio, populacao, coorte, stem_background, month, CLASSI_FIN, sch_non_stem_cdt, rdd_window) %>% 
      reframe(deaths = sum(EVOLUCAO == "Óbito"),
              hosp = sum(HOSPITAL == "Sim"),
              indice_isolamento = sum(indice_isolamento),
              npi_intensity = sum(npi_intensity)) %>% 
      mutate(deaths_per100k = (deaths / populacao) * 100000,
             hosp_per100k = (hosp / populacao) * 100000) %>% 
      arrange(month) %>%
      mutate(deaths_acum = ave(deaths, id_municipio, FUN = cumsum),
             hosp_acum = ave(hosp, id_municipio, FUN = cumsum),
             deaths_per100k_acum = ave(deaths_per100k, id_municipio, FUN = cumsum),
             hosp_per100k_acum = ave(hosp_per100k, id_municipio, FUN = cumsum)) %>% 
      reframe(value = !!sym(var), outcome = var, id_municipio, populacao, coorte, stem_background, month, CLASSI_FIN, sch_non_stem_cdt, rdd_window)
    
    df_final$stem_definition <- definition
    
    # Create a new variable based on the absolute value of X using case_when
    
    
    data_list <- rbind(data_list, df_final)
    
    
  }
}
# Saving
#saveRDS(df_covid, paste0(output_dir, "/data/covid_day_data.rds"))
write_xlsx(data_list, path = paste0(output_dir, "/data/covid_day_data.xlsx"))
