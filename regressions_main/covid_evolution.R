

# Gráfico de evolução de COVID --------------------------------------------


#library("basedosdados")
library("readstata13")
library("stargazer")
#library("gt")
library("modelsummary")
library("rdrobust")
library("rddtools")
library("plyr")
library("tidyverse")


## Setting -----------------------------------------------------------------

setwd("C:/Users/GabrielCaserDosPasso/Documents/RAIS")


## Creating Hospitalization and Deaths Database ----------------------------
## Based on: Escola de Dados -> https://escoladedados.org/coda2021/

formato_data <- "%d/%m/%Y"


### 2020 ------------------------------------------------------------------
### source: https://opendatasus.saude.gov.br/dataset/srag-2020

sivep_2020 <- read_csv2("Dados/input/220613_srag_base_oficial_2020.csv", col_types = cols(
  DT_NOTIFIC = col_date(format = formato_data),
  SEM_NOT = col_double(),
  DT_SIN_PRI = col_date(format = formato_data),
  SEM_PRI = col_double(),
  SG_UF_NOT = col_character(),
  ID_REGIONA = col_character(),
  CO_REGIONA = col_double(),
  ID_MUNICIP = col_character(),
  CO_MUN_NOT = col_double(),
  ID_UNIDADE = col_character(),
  CO_UNI_NOT = col_double(),
  CS_SEXO = col_character(),
  DT_NASC = col_date(format = formato_data),
  NU_IDADE_N = col_double(),
  TP_IDADE = col_double(),
  COD_IDADE = col_double(),
  CS_GESTANT = col_double(),
  CS_RACA = col_double(),
  CS_ETINIA = col_character(),
  CS_ESCOL_N = col_double(),
  ID_PAIS = col_character(),
  CO_PAIS = col_double(),
  SG_UF = col_character(),
  ID_RG_RESI = col_character(),
  CO_RG_RESI = col_double(),
  ID_MN_RESI = col_character(),
  CO_MUN_RES = col_double(),
  CS_ZONA = col_double(),
  SURTO_SG = col_double(),
  NOSOCOMIAL = col_double(),
  AVE_SUINO = col_double(),
  FEBRE = col_double(),
  TOSSE = col_double(),
  GARGANTA = col_double(),
  DISPNEIA = col_double(),
  DESC_RESP = col_double(),
  SATURACAO = col_double(),
  DIARREIA = col_double(),
  VOMITO = col_double(),
  OUTRO_SIN = col_double(),
  OUTRO_DES = col_character(),
  PUERPERA = col_double(),
  FATOR_RISC = col_character(),
  CARDIOPATI = col_double(),
  HEMATOLOGI = col_double(),
  SIND_DOWN = col_double(),
  HEPATICA = col_double(),
  ASMA = col_double(),
  DIABETES = col_double(),
  NEUROLOGIC = col_double(),
  PNEUMOPATI = col_double(),
  IMUNODEPRE = col_double(),
  RENAL = col_double(),
  OBESIDADE = col_double(),
  OBES_IMC = col_double(),
  OUT_MORBI = col_double(),
  MORB_DESC = col_character(),
  VACINA = col_double(),
  DT_UT_DOSE = col_date(format = formato_data),
  MAE_VAC = col_double(),
  DT_VAC_MAE = col_date(format = formato_data),
  M_AMAMENTA = col_double(),
  DT_DOSEUNI = col_date(format = formato_data),
  DT_1_DOSE = col_date(format = formato_data),
  DT_2_DOSE = col_date(format = formato_data),
  ANTIVIRAL = col_double(),
  TP_ANTIVIR = col_double(),
  OUT_ANTIV = col_character(),
  DT_ANTIVIR = col_date(format = formato_data),
  HOSPITAL = col_double(),
  DT_INTERNA = col_date(format = formato_data),
  SG_UF_INTE = col_character(),
  ID_RG_INTE = col_character(),
  CO_RG_INTE = col_double(),
  ID_MN_INTE = col_character(),
  CO_MU_INTE = col_double(),
  UTI = col_double(),
  DT_ENTUTI = col_date(format = formato_data),
  DT_SAIDUTI = col_date(format = formato_data),
  SUPORT_VEN = col_double(),
  RAIOX_RES = col_double(),
  RAIOX_OUT = col_character(),
  DT_RAIOX = col_date(format = formato_data),
  AMOSTRA = col_double(),
  DT_COLETA = col_date(format = formato_data),
  TP_AMOSTRA = col_double(),
  OUT_AMOST = col_character(),
  PCR_RESUL = col_double(),
  DT_PCR = col_date(format = formato_data),
  POS_PCRFLU = col_double(),
  TP_FLU_PCR = col_double(),
  PCR_FLUASU = col_double(),
  FLUASU_OUT = col_character(),
  PCR_FLUBLI = col_double(),
  FLUBLI_OUT = col_character(),
  POS_PCROUT = col_double(),
  PCR_VSR = col_double(),
  PCR_PARA1 = col_double(),
  PCR_PARA2 = col_character(),
  PCR_PARA3 = col_double(),
  PCR_PARA4 = col_character(),
  PCR_ADENO = col_double(),
  PCR_METAP = col_double(),
  PCR_BOCA = col_character(),
  PCR_RINO = col_double(),
  PCR_OUTRO = col_double(),
  DS_PCR_OUT = col_character(),
  CLASSI_FIN = col_double(),
  CLASSI_OUT = col_character(),
  CRITERIO = col_double(),
  EVOLUCAO = col_double(),
  DT_EVOLUCA = col_date(format = formato_data),
  DT_ENCERRA = col_date(format = formato_data),
  DT_DIGITA = col_date(format = formato_data),
  HISTO_VGM = col_double(),
  PAIS_VGM = col_character(),
  CO_PS_VGM = col_double(),
  LO_PS_VGM = col_character(),
  DT_VGM = col_date(format = formato_data),
  DT_RT_VGM = col_date(format = formato_data),
  PCR_SARS2 = col_double(),
  PAC_COCBO = col_character(),
  PAC_DSCBO = col_character(),
  OUT_ANIM = col_character(),
  DOR_ABD = col_double(),
  FADIGA = col_double(),
  PERD_OLFT = col_double(),
  PERD_PALA = col_double(),
  TOMO_RES = col_double(),
  TOMO_OUT = col_character(),
  DT_TOMO = col_date(format = formato_data),
  TP_TES_AN = col_double(),
  DT_RES_AN = col_date(format = formato_data),
  RES_AN = col_double(),
  POS_AN_FLU = col_double(),
  TP_FLU_AN = col_character(),
  POS_AN_OUT = col_double(),
  AN_SARS2 = col_double(),
  AN_VSR = col_character(),
  AN_PARA1 = col_character(),
  AN_PARA2 = col_character(),
  AN_PARA3 = col_character(),
  AN_ADENO = col_character(),
  AN_OUTRO = col_double(),
  DS_AN_OUT = col_character(),
  TP_AM_SOR = col_double(),
  SOR_OUT = col_character(),
  DT_CO_SOR = col_date(format = formato_data),
  TP_SOR = col_double(),
  OUT_SOR = col_character(),
  DT_RES = col_date(format = formato_data),
  RES_IGG = col_character(),
  RES_IGM = col_character(),
  RES_IGA = col_character()
))

head(sivep_2020)

### 2021 --------------------------------------------------------------------
### source: https://opendatasus.saude.gov.br/dataset/srag-2021-e-2022

sivep_2021 <- read_csv2("Dados/input/220613_srag_base_oficial_2021.csv", col_types = cols(
  DT_NOTIFIC = col_date(format = formato_data),
  SEM_NOT = col_double(),
  DT_SIN_PRI = col_date(format = formato_data),
  SEM_PRI = col_double(),
  SG_UF_NOT = col_character(),
  ID_REGIONA = col_character(),
  CO_REGIONA = col_double(),
  ID_MUNICIP = col_character(),
  CO_MUN_NOT = col_double(),
  ID_UNIDADE = col_character(),
  CO_UNI_NOT = col_double(),
  CS_SEXO = col_character(),
  DT_NASC = col_date(format = formato_data),
  NU_IDADE_N = col_double(),
  TP_IDADE = col_double(),
  COD_IDADE = col_double(),
  CS_GESTANT = col_double(),
  CS_RACA = col_double(),
  CS_ETINIA = col_character(),
  CS_ESCOL_N = col_double(),
  ID_PAIS = col_character(),
  CO_PAIS = col_double(),
  SG_UF = col_character(),
  ID_RG_RESI = col_character(),
  CO_RG_RESI = col_double(),
  ID_MN_RESI = col_character(),
  CO_MUN_RES = col_double(),
  CS_ZONA = col_double(),
  SURTO_SG = col_double(),
  NOSOCOMIAL = col_double(),
  AVE_SUINO = col_double(),
  FEBRE = col_double(),
  TOSSE = col_double(),
  GARGANTA = col_double(),
  DISPNEIA = col_double(),
  DESC_RESP = col_double(),
  SATURACAO = col_double(),
  DIARREIA = col_double(),
  VOMITO = col_double(),
  OUTRO_SIN = col_double(),
  OUTRO_DES = col_character(),
  PUERPERA = col_double(),
  FATOR_RISC = col_character(),
  CARDIOPATI = col_double(),
  HEMATOLOGI = col_double(),
  SIND_DOWN = col_double(),
  HEPATICA = col_double(),
  ASMA = col_double(),
  DIABETES = col_double(),
  NEUROLOGIC = col_double(),
  PNEUMOPATI = col_double(),
  IMUNODEPRE = col_double(),
  RENAL = col_double(),
  OBESIDADE = col_double(),
  OBES_IMC = col_double(),
  OUT_MORBI = col_double(),
  MORB_DESC = col_character(),
  VACINA = col_double(),
  DT_UT_DOSE = col_date(format = formato_data),
  MAE_VAC = col_double(),
  DT_VAC_MAE = col_date(format = formato_data),
  M_AMAMENTA = col_double(),
  DT_DOSEUNI = col_date(format = formato_data),
  DT_1_DOSE = col_date(format = formato_data),
  DT_2_DOSE = col_date(format = formato_data),
  ANTIVIRAL = col_double(),
  TP_ANTIVIR = col_double(),
  OUT_ANTIV = col_character(),
  DT_ANTIVIR = col_date(format = formato_data),
  HOSPITAL = col_double(),
  DT_INTERNA = col_date(format = formato_data),
  SG_UF_INTE = col_character(),
  ID_RG_INTE = col_character(),
  CO_RG_INTE = col_double(),
  ID_MN_INTE = col_character(),
  CO_MU_INTE = col_double(),
  UTI = col_double(),
  DT_ENTUTI = col_date(format = formato_data),
  DT_SAIDUTI = col_date(format = formato_data),
  SUPORT_VEN = col_double(),
  RAIOX_RES = col_double(),
  RAIOX_OUT = col_character(),
  DT_RAIOX = col_date(format = formato_data),
  AMOSTRA = col_double(),
  DT_COLETA = col_date(format = formato_data),
  TP_AMOSTRA = col_double(),
  OUT_AMOST = col_character(),
  PCR_RESUL = col_double(),
  DT_PCR = col_date(format = formato_data),
  POS_PCRFLU = col_double(),
  TP_FLU_PCR = col_double(),
  PCR_FLUASU = col_double(),
  FLUASU_OUT = col_character(),
  PCR_FLUBLI = col_double(),
  FLUBLI_OUT = col_character(),
  POS_PCROUT = col_double(),
  PCR_VSR = col_double(),
  PCR_PARA1 = col_double(),
  PCR_PARA2 = col_character(),
  PCR_PARA3 = col_double(),
  PCR_PARA4 = col_character(),
  PCR_ADENO = col_double(),
  PCR_METAP = col_double(),
  PCR_BOCA = col_character(),
  PCR_RINO = col_double(),
  PCR_OUTRO = col_double(),
  DS_PCR_OUT = col_character(),
  CLASSI_FIN = col_double(),
  CLASSI_OUT = col_character(),
  CRITERIO = col_double(),
  EVOLUCAO = col_double(),
  DT_EVOLUCA = col_date(format = formato_data),
  DT_ENCERRA = col_date(format = formato_data),
  DT_DIGITA = col_date(format = formato_data),
  HISTO_VGM = col_double(),
  PAIS_VGM = col_character(),
  CO_PS_VGM = col_double(),
  LO_PS_VGM = col_character(),
  DT_VGM = col_date(format = formato_data),
  DT_RT_VGM = col_date(format = formato_data),
  PCR_SARS2 = col_double(),
  PAC_COCBO = col_character(),
  PAC_DSCBO = col_character(),
  OUT_ANIM = col_character(),
  DOR_ABD = col_double(),
  FADIGA = col_double(),
  PERD_OLFT = col_double(),
  PERD_PALA = col_double(),
  TOMO_RES = col_double(),
  TOMO_OUT = col_character(),
  DT_TOMO = col_date(format = formato_data),
  TP_TES_AN = col_double(),
  DT_RES_AN = col_date(format = formato_data),
  RES_AN = col_double(),
  POS_AN_FLU = col_double(),
  TP_FLU_AN = col_character(),
  POS_AN_OUT = col_double(),
  AN_SARS2 = col_double(),
  AN_VSR = col_character(),
  AN_PARA1 = col_character(),
  AN_PARA2 = col_character(),
  AN_PARA3 = col_character(),
  AN_ADENO = col_character(),
  AN_OUTRO = col_double(),
  DS_AN_OUT = col_character(),
  TP_AM_SOR = col_double(),
  SOR_OUT = col_character(),
  DT_CO_SOR = col_date(format = formato_data),
  TP_SOR = col_double(),
  OUT_SOR = col_character(),
  DT_RES = col_date(format = formato_data),
  RES_IGG = col_character(),
  RES_IGM = col_character(),
  RES_IGA = col_character()
))

head(sivep_2021)



### Merging ----------------------------------------------------------------


sivep_full <- bind_rows(sivep_2020, sivep_2021)
rm(sivep_2020, sivep_2021)

## Garbage collector
gc()

dim(sivep_full)


### Recoding variables ------------------------------------------------------
### based on: https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/pdfs/dicionario_de_dados_srag_hosp_17_02_2022.pdf

sivep_full <- sivep_full %>%
  mutate(CS_SEXO = recode(CS_SEXO,
                          "F" = "Feminino",
                          "M" = "Masculino",
                          "I" = "Ignorado",
                          .default = NA_character_)) %>%
  mutate(CS_SEXO = replace_na(CS_SEXO, "Não preenchido")) %>%
  mutate(TP_IDADE = recode(TP_IDADE,
                           "1" = "Dia",
                           "2" = "Mês",
                           "3" = "Ano",
                           .default = NA_character_)) %>%
  mutate(CS_GESTANT = recode(CS_GESTANT,
                             "1" = "1o trimestre",
                             "2" = "2o trimestre",
                             "3" = "3o trimestre",
                             "4" = "Idade gestacional ignorada",
                             "5" = "Não",
                             "6" = "Não se aplica",
                             "9" = "Ignorado",
                             "0" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(CS_GESTANT = replace_na(CS_GESTANT, "Não preenchido")) %>%
  mutate(CS_RACA = recode(CS_RACA,
                          "1" = "Branca",
                          "2" = "Preta",
                          "3" = "Amarela",
                          "4" = "Parda",
                          "5" = "Indígena",
                          "9" = "Ignorado",
                          .default = "Ignorado")) %>%
  mutate(CS_RACA = replace_na(CS_RACA, "Não preenchido")) %>%
  mutate(CS_ESCOL_N = recode(CS_ESCOL_N,
                             "0" = "Sem escolaridade/Analfabeto",
                             "1" = "Fundamental 1o ciclo (1a a 5a série)",
                             "2" = "Fundamental 2o ciclo (6a a 9a série",
                             "3" = "Médio (1o ao 3o ano)",
                             "4" = "Superior",
                             "5" = "Não se aplica",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(CS_ESCOL_N = replace_na(CS_ESCOL_N, "Não preenchido")) %>%
  mutate(CS_ZONA = recode(CS_ZONA, 
                          "1" = "Urbana",
                          "2" = "Rural",
                          "3" = "Periurbana",
                          "9" = "Ignorado",
                          .default = "Ignorado")) %>%
  mutate(CS_ZONA = replace_na(CS_ZONA, "Não preenchido")) %>%
  mutate(HISTO_VGM = recode(HISTO_VGM,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado",
                            "0" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(SURTO_SG = recode(SURTO_SG,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(NOSOCOMIAL = recode(NOSOCOMIAL,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(NOSOCOMIAL = replace_na(NOSOCOMIAL, "Não preenchido")) %>%
  mutate(AVE_SUINO = recode(AVE_SUINO,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(FEBRE = recode(FEBRE,
                        "1" = "Sim",
                        "2" = "Não",
                        "9" = "Ignorado",
                        .default = "Ignorado")) %>%
  mutate(TOSSE = recode(TOSSE,
                        "1" = "Sim",
                        "2" = "Não",
                        "9" = "Ignorado",
                        .default = "Ignorado")) %>%
  mutate(GARGANTA = recode(GARGANTA,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(DISPNEIA = recode(DISPNEIA,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(DESC_RESP = recode(DESC_RESP,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(SATURACAO = recode(SATURACAO,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(DIARREIA = recode(DIARREIA,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(VOMITO = recode(VOMITO,
                         "1" = "Sim",
                         "2" = "Não",
                         "9" = "Ignorado",
                         .default = "Ignorado")) %>%
  mutate(OUTRO_SIN = recode(OUTRO_SIN,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(PUERPERA = recode(PUERPERA,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(CARDIOPATI = recode(CARDIOPATI,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(HEMATOLOGI = recode(HEMATOLOGI,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(SIND_DOWN = recode(SIND_DOWN,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(HEPATICA = recode(HEPATICA,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(ASMA = recode(ASMA,
                       "1" = "Sim",
                       "2" = "Não",
                       "9" = "Ignorado",
                       .default = "Ignorado")) %>%
  mutate(DIABETES = recode(DIABETES,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(NEUROLOGIC = recode(NEUROLOGIC,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(PNEUMOPATI = recode(PNEUMOPATI,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(IMUNODEPRE = recode(IMUNODEPRE,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(RENAL = recode(RENAL,
                        "1" = "Sim",
                        "2" = "Não",
                        "9" = "Ignorado",
                        .default = "Ignorado")) %>%
  mutate(OBESIDADE = recode(OBESIDADE,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(OUT_MORBI = recode(OUT_MORBI,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(VACINA = recode(VACINA,
                         "1" = "Sim",
                         "2" = "Não",
                         "9" = "Ignorado",
                         .default = "Ignorado")) %>%
  mutate(MAE_VAC = recode(MAE_VAC,
                          "1" = "Sim",
                          "2" = "Não",
                          "9" = "Ignorado",
                          .default = "Ignorado")) %>%
  mutate(M_AMAMENTA = recode(M_AMAMENTA,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(ANTIVIRAL = recode(ANTIVIRAL,
                            "1" = "Sim",
                            "2" = "Não",
                            "9" = "Ignorado")) %>%
  mutate(ANTIVIRAL = replace_na(ANTIVIRAL, "Não preenchido")) %>%
  mutate(TP_ANTIVIR = recode(TP_ANTIVIR,
                             "1" = "Oseltamivir",
                             "2" = "Zanamivir",
                             "3" = "Outro",
                             .default = "Ignorado")) %>%
  mutate(HOSPITAL = recode(HOSPITAL,
                           "1" = "Sim",
                           "2" = "Não",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(UTI = recode(UTI,
                      "1" = "Sim",
                      "2" = "Não",
                      "9" = "Ignorado",
                      .default = "Ignorado")) %>%
  mutate(SUPORT_VEN = recode(SUPORT_VEN,
                             "1" = "Sim, invasivo",
                             "2" = "Sim, não invasivo",
                             "3" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(RAIOX_RES = recode(RAIOX_RES,
                            "1" = "Normal",
                            "2" = "Infiltrado intersticial",
                            "3" = "Consolidação",
                            "4" = "Misto",
                            "5" = "Outro",
                            "6" = "Não realizado",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(AMOSTRA = recode(AMOSTRA,
                          "1" = "Sim",
                          "2" = "Não",
                          "9" = "Ignorado",
                          .default = "Ignorado")) %>%
  mutate(TP_AMOSTRA = recode(TP_AMOSTRA,
                             "1" = "Secreção de naso-orofaringe",
                             "2" = "Lavado broco-alveolar",
                             "3" = "Tecido post mortem",
                             "4" = "Outra",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(PCR_RESUL = recode(PCR_RESUL,
                            "1" = "Detectável",
                            "2" = "Não detectável",
                            "3" = "Inconclusivo",
                            "4" = "Não realizado",
                            "5" = "Aguardando resultado",
                            "9" = "Ignorado",
                            .default = "Ignorado")) %>%
  mutate(TP_FLU_PCR = recode(TP_FLU_PCR,
                             "1" = "Influenza A",
                             "2" = "Influenza B")) %>%
  mutate(PCR_FLUASU = recode(PCR_FLUASU,
                             "1" = "Influenza A (H1N1)",
                             "2" = "Influenza A (H3N2)",
                             "3" = "Influenza A (Não subtipado)",
                             "4" = "Influenza A (Não subtipável)",
                             "5" = "Inconclusivo",
                             "6" = "Outro",
                             .default = "Ignorado")) %>%
  mutate(PCR_FLUBLI = recode(PCR_FLUBLI,
                             "1" = "Victoria",
                             "2" = "Yamagatha",
                             "3" = "Não realizado",
                             "4" = "Inconclusivo",
                             "5" = "Outro",
                             .default = "Ignorado")) %>%
  mutate(POS_PCROUT = recode(POS_PCROUT,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado",
                             .default = "Ignorado")) %>%
  mutate(CLASSI_FIN = recode(CLASSI_FIN,
                             "1" = "SRAG por influenza",
                             "2" = "SRAG por outro vírus respiratório",
                             "3" = "SRAG por outro agente etiológico",
                             "4" = "SRAG não especificado",
                             "5" = "SRAG COVID-19",
                             .default = "Ignorado")) %>%
  mutate(CRITERIO = recode(CRITERIO,
                           "1" = "Laboratorial",
                           "2" = "Vínculo epidemiológico",
                           "3" = "Clínico",
                           .default = "Ignorado")) %>%
  mutate(EVOLUCAO = recode(EVOLUCAO,
                           "1" = "Cura",
                           "2" = "Óbito",
                           "3" = "Óbito por outras causas",
                           "9" = "Ignorado",
                           .default = "Ignorado")) %>%
  mutate(VACINA_COV = recode(VACINA_COV,
                             "1" = "Sim",
                             "2" = "Não",
                             "9" = "Ignorado")) %>%                
  mutate(VACINA_COV = replace_na(VACINA_COV, "Não preenchido")) %>%                                  
  mutate(PAC_DSCBO = replace_na(PAC_DSCBO, "Não preenchido")) %>%
  mutate(ID_UNIDADE = replace_na(ID_UNIDADE, "Unidade não informada")) %>%
  mutate(EVOLUCAO = replace_na(EVOLUCAO, "Em andamento")) %>%
  mutate(idade = if_else(TP_IDADE == "Ano", NU_IDADE_N, 0)) %>%
  mutate(fx_etaria = cut(idade, 
                         breaks = c(-Inf, 2, 4, 9, 19, 29, 39, 49, 59, +Inf),
                         labels = c("< 2 anos", "2-4 anos", "5-9 anos", "10-19 anos", "20-29 anos", "30-39 anos", "40-49 anos", "50-59 anos", "60+ anos")
  )
  ) %>%
  select(-idade)




# Criando gráfico 

sivep_full$month = format(sivep_full$DT_SIN_PRI, "%m")

sivep_2020 <- sivep_full %>%
  filter((CLASSI_FIN == "SRAG COVID-19" | CLASSI_FIN == "SRAG não especificado") & DT_SIN_PRI >= "2020-02-01" & DT_SIN_PRI <= "2020-11-30") %>%
  group_by(CO_MUN_RES, month) %>%
  summarise(deaths = sum(EVOLUCAO == "Óbito", na.rm = TRUE),
            hosp = sum(HOSPITAL == "Sim", na.rm = TRUE),
            coorte = 2016) %>%
  arrange(desc(deaths)) 

#readRDS(sivep, "Dados/Output/230221_covid_data.rds")

sivep_2020$grupo = ifelse(sivep_2020$CO_MUN_RES %in% subset(df, stem_background == 1 & coorte == 2016 )$id_municipio, "STEM",
                          ifelse(sivep_2020$CO_MUN_RES %in% subset(df, stem_background == 0 & coorte == 2016 )$id_municipio, "Non-STEM", NaN)) 


deaths_group_2020 <- sivep_2020 %>% 
  filter(grupo == "STEM" | grupo == "Non-STEM") %>% 
  group_by(CO_MUN_RES) %>%
  arrange(month) %>% 
  mutate(deaths_acum = cumsum(deaths),
         hosp_acum = cumsum(hosp)) 

deaths_group_2020 <- deaths_group_2020 %>%
  group_by(grupo, month) %>% 
  summarise(death_mean = mean(deaths),
            hosp_mean = mean(hosp),
            hosp_ci_min = t.test(hosp, conf.level = 0.90)$conf.int[1],
            hosp_ci_max = t.test(hosp, conf.level = 0.90)$conf.int[2],
            death_ci_min = t.test(deaths, conf.level = 0.90)$conf.int[1],
            death_ci_max = t.test(deaths, conf.level = 0.90)$conf.int[2],
            coorte = 2016,
            deaths_acum_mean = mean(deaths_acum),
            hosp_acum_mean = mean(hosp_acum),
            hosp_acum_ci_min = t.test(hosp_acum, conf.level = 0.90)$conf.int[1],
            hosp_acum_ci_max = t.test(hosp_acum, conf.level = 0.90)$conf.int[2],
            death_acum_ci_min = t.test(deaths_acum, conf.level = 0.90)$conf.int[1],
            death_acum_ci_max = t.test(deaths_acum, conf.level = 0.90)$conf.int[2]
            
  ) %>%
  arrange(desc(death_mean)) 

teste <- sivep_2020 %>%
  filter(grupo != "NaN") %>% 
  summarise(CO_MUN_RES, grupo)

length(unique(teste$CO_MUN_RES))

hosp_2020_acum <- ggplot(data = deaths_group_2020, aes(x = month, y = hosp_acum_mean, group = grupo, color = grupo)) +
  geom_line() +
  geom_point()
#geom_ribbon(aes(x = month, ymin = hosp_acum_ci_min, ymax = hosp_acum_ci_max, fill = grupo), alpha = 0.2)

death_2020_acum <- ggplot(data = deaths_group_2020, aes(x = month, y = deaths_acum_mean, group = grupo, color = grupo)) +
  geom_line() +
  geom_point() 
#geom_ribbon(aes(x = month, ymin = death_acum_ci_min, ymax = death_acum_ci_max, fill = grupo), alpha = 0.2)


death_2020_acum  / hosp_2020_acum


hosp_2020 <- ggplot(data = deaths_group_2020, aes(x = month, y = hosp_mean, group = grupo, color = grupo)) +
  geom_line() +
  geom_point()
#geom_ribbon(aes(x = month, ymin = hosp_ci_min, ymax = hosp_ci_max, fill = grupo), alpha = 0.2)

death_2020 <- ggplot(data = deaths_group_2020, aes(x = month, y = death_mean, group = grupo, color = grupo)) +
  geom_line() +
  geom_point()
#geom_ribbon(aes(x = month, ymin = death_ci_min, ymax = death_ci_max, fill = grupo), alpha = 0.2)


plots = death_2020  / hosp_2020

plots

ggsave("Dados/output/230224_mapa_stem_SP.png", plots,
       width = 5.5,
       height = 5,
       units = "in")
