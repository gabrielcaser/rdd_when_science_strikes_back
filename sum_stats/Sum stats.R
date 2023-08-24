
# This program creates sum stats

# Setting -----------------------------------------------------------------

setwd("C:/Users/GabrielCaserDosPasso/Documents/RAIS")

output_dir = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/Dados/output"
input_dir = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/Dados/input"


# Oppening ----------------------------------------------------------------

df <- readRDS(paste(input_dir,"/rdd_data.RDS", sep = ""))

# Sum stats ---------------------------------------------------------------
library("estimatr")

dat <- df[ c("stem_job_4",
             "tenure",
             "X",
             "medico",
             "mulher",
             "idade",
             "instrucao",
             "reeleito",
             "ideology_party",
             "populacao_2010",
             "densidade",
             "taxa_analfabetismo_18_mais",
             "indice_gini",
             "idhm",
             "renda_pc",
             "per_populacao_urbana",
             "per_populacao_homens",
             "tx_med",
             "pct_desp_recp_saude_mun",
             "cob_esf",
             "tx_leito_sus",
             "ideology_municipality",
             "Y_deaths_sivep",
             "Y_hosp",
             "Y_cases",
             "barreiras_sanitarias",
             "mascaras",
             "restricao_atv_nao_essenciais",
             "restricao_circulacao",
             "restricao_transporte_publico",
             "total_nfi")]

dat <- dat %>% 
  summarise(tenure_stem_job = tenure,
            #physician = as.numeric(medico),
            female = as.numeric(mulher),
            age = idade,
            education = instrucao,
            incumbent_when_elected = as.numeric(reeleito),
            party_ideology = ideology_party,
            stem_win_margin = X,
            deaths_per_100k = Y_deaths_sivep,
            hospitalizations_per_100k = Y_hosp,
            cases_per_100k = Y_cases,
            cordon_sanitaire = barreiras_sanitarias,
            face_covering_required = mascaras,
            closure_of_non_essential = restricao_atv_nao_essenciais,
            gathering_prohibition = restricao_circulacao,
            public_transport_restriction = restricao_transporte_publico,
            number_of_npi = total_nfi,
            population_2010 = populacao_2010,
            illiteracy_rate = taxa_analfabetismo_18_mais,
            gini = indice_gini,
            hdi = idhm,
            pc_income = renda_pc,
            density = densidade,
            urban_pop_rate = per_populacao_urbana,
            men_pop_rate = per_populacao_homens,
            physician_per_1k = tx_med,
            health_municipal_spending_rate = pct_desp_recp_saude_mun,
            community_health_agency_rate = cob_esf,
            hosp_beds_per_100k_pop = tx_leito_sus,
            stem_job = stem_job_4)

tab <- datasummary(All(data.frame(dat)) ~ N  + Min + Mean + Max + SD, data = dat, fmt = 2)

tab

datasummary(All(data.frame(dat)) ~ N  + Min + Mean + Max + SD, data = dat, fmt = 2, output = "Dados/output/230224_table_sum_stats.tex")


#datasummary_skim(All(dat) ~ NUnique + min, dat)

datasummary_balance( ~ stem_job, dinm_statistic = "p.value", data = dat, fmt = 2)
datasummary_balance( ~ stem_job, dinm_statistic = "p.value", data = dat, fmt = 2, output = "Dados/output/230224_table_sum_stats_groups.tex")

#datasummary_crosstab(tenure ~ stem_job_4, data = dat)

## deaths and hospitalizations ---------------------------------------------

#sivep %>% 
#  group_by(coorte) %>% 
#  count() # tenho que descobrir porque estão aparecendo municípios a mais -> aparentemente são algumas classificações diferentes (Brasília - Asa Norte, Brasília - Asa Sul, dentre outros typos)


## stem candidates ---------------------------------------------------------


profi2 <- df %>%
  group_by(coorte) %>% 
  dplyr::summarise(per_stem_candidates_elected = (sum(stem_job_4 == 1)), #/ sum(stem_job != 99)) * 100,
                   total_stem_candidates = (sum(stem_job_4 == 1)))


q <- ggplot(profi2, aes(x = as.character(coorte), y = per_stem_candidates_elected)) +
  geom_bar(stat = "identity") +
  # theme_minimal() + 
  #ggtitle("Number of elected STEM candidates") +
  xlab("cohort") +
  ylab("")

q

#ggsave("Dados/output/221201_barplot_stem_candidates.png", q,
#       width = 5.50,
#       height = 5.00,
#       units = "in")


profi <- df %>% 
  filter(stem_job_4 == 1) %>%
  group_by(coorte, cbo_agregado_nome) %>%
  dplyr::summarise(number = n()) %>%
  ungroup() %>% 
  group_by(coorte) %>%
  slice_max(order_by = number, n = 10) %>%
  arrange(desc(number))


p <- ggplot(profi, aes(x = as.character(coorte), y = number, fill = as.character(cbo_agregado_nome))) +
  geom_bar(pattern = "occupation", color = "black", stat = "identity") +
  theme_minimal(base_size = 16) +
  #theme_bw() +
  #ggtitle("STEM mayors' occupations") +
  xlab("cohort") +
  ylab("number of STEM mayors") 

p <- p + scale_fill_discrete(name = "occuppation")
p

ggsave("Dados/output/230224_barplot_stem_cbos.png", height = 5.0, width = 9.5)

#df2 <- df2 %>% 
#  filter(df$sch_non_stem_cdt == 1)



#profi_RO <- df2 %>% 
#  filter(stem_job_4 == 1 & state == "AC") %>%
#  group_by(coorte, cbo_2002) %>%
#  dplyr::summarise(number = n()) %>%
#  ungroup() %>% 
#  group_by(coorte) %>%
#  slice_max(order_by = number, n = 5) %>%
#  arrange(desc(number))


#p_RO <- ggplot(profi_RO, aes(x = as.character(coorte), y = number, fill = as.character(cbo_2002))) +
#  geom_bar(stat = "identity") +
#  theme_minimal() + 
#  #ggtitle("Top 5 STEM candidate's occupations (TSE)") +
#  xlab("cohort") +
#  ylab("number of STEM mayors")
#
#p_RO <- p_RO + scale_fill_discrete(name = "cbo_2002")
#p_RO


## tenure

box <- ggplot(subset(df, stem_job_4 == 1)) + 
  geom_boxplot(aes(y=tenure, fill = as.factor(coorte))) +
  xlab("") +
  scale_x_discrete() +
  scale_fill_discrete(name = "cohort")

box 

ggsave("Dados/output/230223_boxplot_tenure.png", height = 5.0, width = 5.5)


#df<- df %>% 
#filter(estimated_population < 200000)




## % of STEM candidates per state

states <- df2 %>%
  filter(coorte == 2016) %>% 
  group_by(state, coorte) %>% 
  dplyr::summarise(perc_stem = sum(stem_job_4 == 1, na.rm = TRUE) / sum(stem_job_4 == 1 | stem_job_4 == 0, na.rm = TRUE)) %>% 
  arrange(desc(perc_stem))

box2 <- ggplot(states, aes(y = perc_stem * 100, group = coorte, x = as.character(coorte))) + 
  geom_boxplot() + 
  #theme_minimal() + 
  xlab("cohort") +
  ylab("% of STEM mayors")

box2 # bem estranho esses outliers, tenho que investigar porque RO e AC estão com tantos prefeitos STEM

ggsave("Dados/output/230223_sumstats_boxplot.png", height = 5, width = 5.5)

mean(states$perc_stem[states$coorte == 2016])
mean(states$perc_stem[states$coorte == 2020])



### Investigando outliers

MG <- df2 %>% ## o estranho aqui é o tanto de stem com cbo '317110' que é 'Programador de sistemas de informação'
  filter(state == 'MG' & coorte == 2020) %>%
  summarise(id_municipio, city, cpf, nome, sigla_partido, stem_job_4, tenure, ocupacao, cbo_2002, instrucao, confirmed_per_100k_inhabitants, deaths_per_100k_inhabitants)


df2 %>% 
  filter(coorte == 2020) %>% 
  group_by(id_municipio) %>% 
  dplyr::count() %>% 
  arrange(desc(n)) # está repetindo apenas um município na amostra, tenho que apagá-lo




# Criando mapa ---------------------------------------------------------

## estados
#utils::remove.packages('geobr')
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
#library('geobr')
##dados_mapa <- read_state(year=2019, showProgress = FALSE, simplified = FALSE)

dados_mapa <- readRDS("Dados/input/220811_geo_dados_estados_2019.rds")

sf2 <- states %>% 
  filter(coorte == 2016)

sf2 <- sf2 %>% 
  rename(sigla_uf = state)

dados_mapa <- dados_mapa %>% 
  rename(sigla_uf = abbrev_state)


sf2 <- left_join(dados_mapa, sf2, by = c("sigla_uf"))
dim(df)

sf2$perc_stem = sf2$perc_stem * 100


sf2 %>%
  filter(sigla_uf != "AC") %>% 
  ggplot() +
  geom_sf(aes(fill = perc_stem), alpha = 0.9, color = NA) +
  labs(#title="Percentage of STEM mayors per state (2016)",
    caption='Source: Author', size = 8) +
  viridis::scale_fill_viridis(
    direction = 1,
    name="% of STEM mayors",
    na.value = "white"
  ) +
  theme_minimal(base_size = 16) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("Dados/output/230224_mapa_stem_estados_2016.png", height = 5.5, width = 10)


## municipios

mun <- readRDS("Dados/input/220811_geo_dados_municipios_2019.rds")

mun <- mun %>% 
  rename(id_municipio = code_muni)

mun$id_municipio = substr(mun$id_municipio,1,6)

sf3 <- df %>% 
  filter(coorte == 2016)

sf3 <- left_join(mun, sf3, by = c("id_municipio"))

sf3 %>%
  #filter(code_region == 3) %>%  # para ver o mapa inteiro é só tirar o code_region
  # filter(sigla_uf != "AC") %>% 
  ggplot() +
  geom_sf(data = subset(dados_mapa)) +
  geom_sf(aes(fill = stem_job_4), alpha = .7, color = NA) +
  labs(#title="Municipalities where a STEM canditate was among the top 2 voted (2016)",
    caption='Source: Author', size = 8) +
  scale_fill_manual(values = c("red", "blue"),
                    name = "STEM candidate",
                    na.value = "grey90",
                    labels = c("Lost","Won", "Not in top 2")) +
  theme_minimal(base_size = 16) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("Dados/output/230224_mapa_stem_municipios_2016.png", height = 5.5, width = 10)



sf3 %>%
  filter(code_state == 35) %>%  # para ver o mapa inteiro é só tirar o code_region
  ggplot() +
  geom_sf(data = subset(dados_mapa, code_state == 35)) +
  geom_sf(aes(fill = stem_job_4), alpha = .7, color = NA) +
  labs(#title="Municipalities where a STEM canditate was among the top 2 voted in SP (2016)",
    caption='Source: Author', size = 8) +
  scale_fill_manual(values = c("red", "blue"),
                    name = "STEM candidate",
                    na.value = "grey90",
                    labels = c("Lost","Won", "Not in top 2")) +
  theme_minimal(base_size = 16) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("Dados/output/230224_mapa_stem_SP.png", height = 5.5, width = 7.5)



# Gráficos - Discontinuidade ----------------------------------------------




## outcomes

### hosp 

amostra <- cbind()

poli = 1
covsZ = cbind(state.d, df$idade)
#covsZ = df$tenure


hosp <- rdplot(df$Y_hosp , df$X,
               covs = covsZ,
               p = poli,
               x.lim = c(-0.10, 0.10),
               y.lim = c(100, 550),
               #shade = TRUE,
               subset = amostra,
               h = r4$bws[1],
               scale = 3,
               #ci = 95,
               binselect = "qsmv",
               kernel = 'triangular',
               x.label = "STEM candidate's margin of victory",
               y.label = "",
               title = "(a) COVID-19 hospitalizations")


### deaths


death <- rdplot(df$Y_deaths_sivep , df$X,
                covs = covsZ,
                p = poli,
                x.lim = c(-0.10, 0.10),
                y.lim = c(00, 100),
                #shade = TRUE,
                subset = amostra,
                h = r5$bws[1],
                scale = 6,
                #ci = 90,
                binselect = "qsmv",
                kernel = 'triangular',
                x.label = "STEM candidate's margin of victory",
                y.label = "",
                title = "(b) COVID-19 deaths")

death <- death$rdplot



death <- death + 
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 10, face = "plain"),
        title = element_text(size = 12)) 

death

hosp <- hosp$rdplot  

hosp <- hosp +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 10, face = "plain"),
        title = element_text(size = 12))

plots <- hosp / death

plots

ggsave("Dados/output/230224_bigsample_plots_outcomes.png", plots,
       width = 5.5,
       height = 5,
       units = "in")


### NFI

rdplot(df$total_nfi , df$X,
       covs = covsZ,
       p = poli,
       x.lim = c(-0.0776, 0.0776),
       #y.lim = c(100, 700),
       #shade = TRUE,
       subset = amostra,
       h = 0.0776,
       scale = 4,
       #ci = 90,
       binselect = "qsmv",
       kernel = 'uniform',
       x.label = "STEM candidate winning margin",
       y.label = "Nº of NFI",
       title = "STEM candidate elected in 2016 and NFI in 2020")



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

sivep_2020$grupo = ifelse(sivep_2020$CO_MUN_RES %in% subset(df, stem_job_4 == 1 & coorte == 2016 )$id_municipio, "STEM",
                          ifelse(sivep_2020$CO_MUN_RES %in% subset(df, stem_job_4 == 0 & coorte == 2016 )$id_municipio, "Non-STEM", NaN)) 


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
