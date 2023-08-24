

# Libraries ***************************************************** ---------------------------------------------------------------

#library("basedosdados")
library("readstata13")
library("stargazer")
library("gt")
library("modelsummary")
library("rdrobust")
library("rddtools")
library("plyr")
library("sf")
library("rio")
library("readr")
library("stringr")
library(tidyverse)
#library(dplyr)
library("readxl")
library("patchwork")


theme_set(theme_minimal(base_size = 16))

# Setting -----------------------------------------------------------------

setwd("C:/Users/GabrielCaserDosPasso/Documents/RAIS")
create_dataset_for_regressions = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/create_dataset_for_regressions/output/data"
output_dir                     = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/regressions_tsedefiniton/output"
# Oppening Covid and RDD Data ----------------------------------------------------

df <- readRDS(paste(create_dataset_for_regressions, "/rdd_data_tsedefinition.rds", sep = ""))


# Creating vector of baseline controls ------------------------------------

Z_variables = cbind(df$taxa_analfabetismo_18_mais,
                    df$indice_gini,
                    df$renda_pc,
                    df$populacao_2010,
                    df$per_populacao_homens,
                    df$per_populacao_urbana,
                    df$pct_desp_recp_saude_mun,
                    df$tx_med,
                    df$cob_esf,
                    df$tx_leito_sus,
                    df$ideology_municipality,
                    df$idhm)


# Criando funções -----------------------------------

## Criando função tabela de resultados

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[1, 1],
    std.error = model$se[1, 1],
    p.value = model$pv[, 1],
    conf.low = model$ci[3,1],
    conf.high = model$ci[3,2]
    
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    "Eff number obs." = as.character(model$N_h[1] + model$N_h[2]),
    "Bandwidth" = as.character(round(model$bws[1,1] * 100, 2)),
    "State FE" = "PREENCHER",
    "Election FE" = "PREENCHER",
    "Gender" = "PREENCHER"
  )
  ret
}

## Criando função tabela de robustez


robust_check <- function(outcome, poli, covsZ, k) {
  
  df_robs <- data.frame()
  
  
  for (i in seq(0.02, 0.2, by = 0.01)) {
    
    prov = rdrobust(y = outcome, df$X, p = poli, level = 90, kernel = k, h = i, covs = covsZ) #rodando rdd

    df_robs = rbind(df_robs, c(i, prov[["coef"]][1], prov[["coef"]][3], prov[["ci"]][3,1], prov[["ci"]][3,2], prov[["ci"]][1,1], prov[["ci"]][1,2], prov[["z"]][3], prov[["N_h"]][1] + prov[["N_h"]][2])) #salvando colunas
    
  }
  
  colnames(df_robs) <- c("bw", "coef_conv", "coef_robs", "ci_lower_rob", "ci_higher_rob",  "ci_lower_conv", "ci_higher_conv", "z", "eff_n")
  
  
  return(df_robs)
  
  
}


# Creating state dummies for fixed effects --------------------------------



state.f = factor(df$sigla_uf)

state.d = model.matrix(~state.f+0)


# Main Results -------------------------------------------------------------


covsZ = cbind(state.d)
poli = 1
k = "triangular"


taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
indice_gini <- rdrobust(df$indice_gini, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
renda_pc <- rdrobust(df$renda_pc, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
populacao_2010 <- rdrobust(log(df$populacao_2010), df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
idhm <- rdrobust(df$idhm, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
per_populacao_urbana <- rdrobust(log(df$per_populacao_urbana + 1), df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
per_populacao_homens <- rdrobust(log(df$per_populacao_homens + 1), df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
tx_med_ch <- rdrobust(df$tx_med, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
cob_esf <- rdrobust(df$cob_esf, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
tx_leito_sus <- rdrobust(df$tx_leito_sus, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
ideology_municipality <- rdrobust(df$ideology_municipality, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)


models <- list(
  "Gini" = indice_gini,
  "PC income" = renda_pc,
  "Population" = populacao_2010,
  "Illiteracy" = taxa_analfabetismo_18_mais,
  "HDI" = idhm,
  "% Masc. Pop" = per_populacao_homens,
  "% Health municipal spending" = pct_desp_recp_saude_mun,
  "Doctors per 1k pop." = tx_med_ch,
  "Community health agents program " = cob_esf,
  "Hosp. beds per 100k pop." = tx_leito_sus,
  "Mun. ideology index" = ideology_municipality)


baseline_table <- modelsummary(models,
                               estimate = "{estimate}",
                               statistic = c("[{std.error}]","{p.value}{stars}"
                               ),
                               coef_rename = c("Robust" = "RD estimator"),
                               stars = c('*'=.1, '**'=.05, '***'=.01),
                               fmt = 2, # decimal places
                               output = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/regressions_tsedefinition/bigsample_baseline.tex",
                               #output = "gt",
                               title = "Baseline Characteristis - RD estimates",
                               coef_omit = "Corrected|Conventional")

baseline_table

## estimates

covsZ = cbind(state.d , df$ideology_municipality)
poli = 1


r1 = rdrobust(df$Y_hosp, df$X,  p = poli, kernel = k, subset = amostra, covs = covsZ)
r2 = rdrobust(df$Y_deaths_sivep, df$X,  p = poli, kernel = k, subset = amostra, covs = covsZ)


covsZ = cbind(state.d, as.numeric(df$mulher),  df$ideology_party, df$ideology_municipality)
poli = 1

r3 = rdrobust(df$Y_hosp, df$X,  p = poli, kernel = k, subset = amostra, covs = covsZ)
r4 = rdrobust(df$Y_deaths_sivep, df$X,  p = poli, kernel = k, subset = amostra, covs = covsZ)


models <- list(
  "Hospitalizations" = r1,
  "Deaths" = r2,
  "Hospitalizations" = r3,
  "Deaths" = r4)


teste <- modelsummary(models,
                      estimate = c("{estimate}"),
                      statistic = c("[{std.error}]","{p.value}{stars}"
                      ),
                      coef_rename = c("Robust" = "RD estimator"),
                      stars = c('*'=.1, '**'=.05, '***'=.01),
                      fmt = 2, # decimal places
                      output = "gt",
                      title = "Impact of STEM Leadership on Epidemiological Outcomes — RD estimates",
                      coef_omit = "Corrected|Conventional") %>%
  tab_spanner(label = "(1)", columns = 2:3) %>% 
  tab_spanner(label = "(2)", columns = 4:5)


teste
#gt::gtsave(teste, filename =  "Dados/output/221104(2)_bigsample_estimates.tex")



# Personal charact


covsZ = cbind(state.d)
poli = 1

mulher <- rdrobust(as.numeric(df$mulher),  df$X, p = poli, kernel = k,  subset = amostra, covs = covsZ)
summary(mulher)

reeleito <- rdrobust(df$reeleito,  df$X, p = poli, kernel = k,  subset = amostra, covs = covsZ)
summary(reeleito)

idade <- rdrobust(df$idade,  df$X, p = poli, kernel = k, subset = amostra, covs = covsZ)
summary(idade)

#education <- rdrobust(df$instrucao,  df$X, p = poli, kernel = k, subset = amostra, covs = covsZ)
#summary(education)

ideology <- rdrobust(df$ideology_party,  df$X, p = poli, kernel = k, subset = amostra, covs = covsZ)
summary(ideology)



models <- list(
  "Women" = mulher,
  "Incumbent" = reeleito,
  "Age" = idade,
  #"Years of education" = education,
  "Mayors' party ideology" = ideology)


teste_chr <- modelsummary(models,
                          estimate = "{estimate}",
                          statistic = c("[{std.error}]","{p.value}{stars}"
                          ),
                          coef_rename = c("Robust" = "RD estimator"),
                          stars = c('*'=.1, '**'=.05, '***'=.01),
                          fmt = 2, # decimal places
                          #output = "Dados/output/221103_bigsample_personal_charac.tex",
                          output = "gt",
                          title = "STEM candidates' personal characteristics — RD estimates",
                          coef_omit = "Corrected|Conventional")




teste_chr
#gt::gtsave(teste_chr, filename =  "Dados/output/221103_personal_char.tex")


#mecanismo


covsZ = cbind(state.d)
poli = 1


r1 = rdrobust(df$total_nfi,  df$X, p = poli, kernel = k,  subset = amostra, covs = covsZ)
r2 = rdrobust(df$mascaras, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)
r3 = rdrobust(df$restricao_atv_nao_essenciais, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)
r4 = rdrobust(df$restricao_circulacao, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)
r5 = rdrobust(df$restricao_transporte_publico, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)
r6 = rdrobust(df$barreiras_sanitarias, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)

covsZ = cbind(state.d, as.numeric(df$mulher),  df$ideology_party, df$ideology_municipality)

r12 = rdrobust(df$total_nfi, df$X, p = poli, kernel = k,  subset = amostra, covs = covsZ)
r22 = rdrobust(df$mascaras, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)
r32 = rdrobust(df$restricao_atv_nao_essenciais, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)
r42 = rdrobust(df$restricao_circulacao, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)
r52 = rdrobust(df$restricao_transporte_publico, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)
r62 = rdrobust(df$barreiras_sanitarias, df$X,   p = poli, kernel = k,  subset = amostra, covs = covsZ)


models <- list("Total NFI" = r1,
               "Masks" = r2,
               "Restrictions atv." = r3,
               "Restrictions circu." = r4,
               "Restrictions transp." = r5,
               "Sani barriers" = r6,
               "Total NFI" = r12,
               "Masks" = r22,
               "Restrictions atv." = r32,
               "Restrictions circu." = r42,
               "Restrictions transp." = r52,
               "Sani barriers" = r62)

mr3 <- modelsummary(models,
                    estimate = "{estimate}",
                    statistic = c("[{std.error}]","{p.value}{stars}"),
                    stars = c('*'=.1, '**'=.05, '***'=.01),
                    fmt = 2, # decimal places,
                    output = "gt",
                    title = "Impact of STEM Candidate Elected in 2016 on COVID-19 Epidemiological Outcomes per 100k inhabitants in 2021 — RD estimates without controls",
                    coef_omit = "Bias-Corrected|Conventional")


mr3 %>%
  tab_spanner(label = "(1)", columns = 2:7) %>% 
  tab_spanner(label = "(2)", columns = 8:13)


## controlando mecanismo


covsZ = cbind(as.numeric(df$mulher), as.numeric(df$reeleito), df$total_nfi)

poli = 1

r1 = rdrobust(df$Y_hosp, df$X,  p = poli, kernel = 'uniform', subset = amostra, covs = covsZ)
r2 = rdrobust(df$Y_deaths_sivep, df$X,  p = poli, kernel = 'uniform', subset = amostra, covs = covsZ)

poli = 2

r3 = rdrobust(df$Y_hosp, df$X,  p = poli, kernel = 'uniform', subset = amostra, covs = covsZ)
r4 = rdrobust(df$Y_deaths_sivep, df$X,  p = poli, kernel = 'uniform', subset = amostra, covs = covsZ)

models <- list("Hospitalizations" = r1,
               "Deaths" = r2)
#"Hospitalizations" = r3,
# "Deaths" = r4)


mr4 <- modelsummary(models,
                    estimate = "{estimate}",
                    statistic = c("{p.value}{stars}","[{conf.low}, {conf.high}]"),
                    coef_rename = c("Robust" = "RD estimator"),
                    stars = c('*'=.1, '**'=.05, '***'=.01),
                    fmt = 2, # decimal places
                    output = "gt",
                    title = "Impact of STEM Candidate controlling for Non-Farmaceutical Intervations (NFIs) — RD estimates with fixed effects (optimal bandwidth)",
                    coef_omit = "Bias-Corrected|Conventional")

mr4# %>%
# tab_spanner(label = "Linear", columns = 2:3) %>% 
# tab_spanner(label = "Quadratic", columns = 4:5)


# Robustness --------------------------------------------------------------


## Different windows -----------------------------------------------------

CovsZ = cbind(year.d, state.d, df$mulher, df$restricao_transporte_publico) # main results
CovsZ = cbind(state.d, as.numeric(df$mulher),  df$ideology_party, df$ideology_municipality)

k = 'triangular'

  
df_robs_hosp <- robust_check(df$Y_hosp, 1, CovsZ, k)
df_robs_deaths <- robust_check(df$Y_deaths_sivep, 1, CovsZ, k)
df_robs_nfi <- robust_check(df$total_nfi, 1, CovsZ, k)
df_robs_masks <- robust_check(df$mascaras, 1, CovsZ, k)
df_robs_trans_pub <- robust_check(df$restricao_transporte_publico, 1, CovsZ, k)
df_robs_circu <- robust_check(df$restricao_circulacao, 1, CovsZ, k)
df_robs_atv <- robust_check(df$restricao_atv_nao_essenciais, 1, CovsZ, k)
df_robs_sani <- robust_check(df$barreiras_sanitarias, 1, CovsZ, k)





plot_hosp_robs <-  ggplot(df_robs_hosp, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.2) +
  #ylim(-750, 200) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12)) +
  ggtitle("(b) COVID-19 hospitalizations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_hosp_robs 


plot_deaths_robs <-  ggplot(df_robs_deaths, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.2) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12)) +
  ggtitle("(a) COVID-19 deaths") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_deaths_robs


plot_deaths_robs / plot_hosp_robs

graficos_juntos <- plot_deaths_robs / plot_hosp_robs

#ggsave("Dados/output/221201_mechanism_robust_outcomes.png", graficos_juntos,
#       width = 5.50,
#       height = 5.00,
#       units = "in")
#

plot_nfi_robs <-  ggplot(df_robs_nfi, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.2) +
 # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(a) Total number of NPIs") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_nfi_robs



plot_nfi_masks <-  ggplot(df_robs_masks, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.2) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(b) Face covering restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_nfi_masks


plot_nfi_trans <-  ggplot(df_robs_trans_pub, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.2) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(c) Transportation restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_nfi_trans


plot_nfi_sani <-  ggplot(df_robs_sani, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.2) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(d) Cordon sanitaire restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_nfi_atv <-  ggplot(df_robs_atv, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.2) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(e) Non-essential activ. restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_nfi_circu <-  ggplot(df_robs_circu, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.2) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(f) Public gathering restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)




graf <- (plot_nfi_robs + plot_nfi_masks) / ( plot_nfi_sani + plot_nfi_trans) / (plot_nfi_atv + plot_nfi_circu)

graf

ggsave("Dados/output/221202_mechanism_npi_rob.png", graf,
       width = 10.00,
       height = 8.00,
       units = "in")


## Baseline --------------------------------------------------------------

# quadratic

amostra = cbind()
covsZ = cbind(state.d)
poli = 1
janela = cbind()
k = "triangular"

taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
indice_gini <- rdrobust(df$indice_gini,  df$X, p = poli, kernel = k, subset = amostra, covs = covsZ)
renda_pc <- rdrobust(df$renda_pc,  df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
populacao_2010 <- rdrobust(df$populacao_2010,  df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
idhm <- rdrobust(df$idhm,  df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
per_populacao_urbana <- rdrobust(df$per_populacao_urbana,  df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
per_populacao_homens <- rdrobust(df$per_populacao_homens,  df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun,  df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
tx_med_ch <- rdrobust(df$tx_med, df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
cob_esf <- rdrobust(df$cob_esf,  df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
tx_leito_sus <- rdrobust(df$tx_leito_sus, df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)
ideology_municipality <- rdrobust(df$ideology_municipality, df$X, p = poli, kernel = k, bwselect = "mserd", subset = amostra, covs = covsZ)



models <- list("Illiteracy" = taxa_analfabetismo_18_mais,
               "Gini" = indice_gini,
               "PC income" = renda_pc,
               "Population" = populacao_2010,
               "HDI" = idhm,
               "% Urb. Pop" = per_populacao_urbana,
               "% Masc. Pop" = per_populacao_homens,
               "% Health municipal spending" = pct_desp_recp_saude_mun,
               "Doctors per 1k pop." = tx_med_ch,
               "Community health agents program " = cob_esf,
               "Hosp. beds per 100k pop." = tx_leito_sus,
               "Mun. ideology index" = ideology_municipality)

baseline_table <- modelsummary(models,
                               estimate = "{estimate}",
                               statistic = c("{p.value}{stars}","[{conf.low}, {conf.high}]"
                               ),
                               coef_rename = c("Robust" = "RD estimator"),
                               stars = c('*'=.1, '**'=.05, '***'=.01),
                               fmt = 2, # decimal places
                               #output = "Dados/output/bigsample_baseline.tex",
                               output = "gt",
                               title = "Baseline Characteristis - RD estimates",
                               coef_omit = "Corrected|Conventional")%>%
  tab_spanner(label = "Demography", columns = 2:8) %>% 
  tab_spanner(label = "Health", columns = 9:12) %>%
  tab_spanner(label = "Ideology", columns = 13)

baseline_table

#gt::gtsave(baseline_table, filename =  "Dados/output/221201_bigsample_robust_baseline_quadratic.tex")



### metade

covsZ = cbind(state.d)
poli = 1
k = "triangular"


taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, h = taxa_analfabetismo_18_mais[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
indice_gini <- rdrobust(df$indice_gini, h = indice_gini[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
renda_pc <- rdrobust(df$renda_pc, h = renda_pc[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
populacao_2010 <- rdrobust(df$populacao_2010, h = populacao_2010[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
idhm <- rdrobust(df$idhm, h = idhm[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
per_populacao_urbana <- rdrobust(df$per_populacao_urbana, h = per_populacao_urbana[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
per_populacao_homens <- rdrobust(df$per_populacao_homens, h = per_populacao_homens[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun, h = pct_desp_recp_saude_mun[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
tx_med_ch <- rdrobust(df$tx_med, h = tx_med_ch[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
cob_esf <- rdrobust(df$cob_esf, h = cob_esf[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
tx_leito_sus <- rdrobust(df$tx_leito_sus, h = tx_leito_sus[["bws"]][1] / 2,  df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
ideology_municipality <- rdrobust(df$ideology_municipality, h = ideology_municipality[["bws"]][1] / 2, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)


models <- list("Illiteracy" = taxa_analfabetismo_18_mais,
               "Gini" = indice_gini,
               "PC income" = renda_pc,
               "Population" = populacao_2010,
               "HDI" = idhm,
               "% Urb. Pop" = per_populacao_urbana,
               "% Masc. Pop" = per_populacao_homens,
               "% Health municipal spending" = pct_desp_recp_saude_mun,
               "Doctors per 1k pop." = tx_med_ch,
               "Community health agents program " = cob_esf,
               "Hosp. beds per 100k pop." = tx_leito_sus,
               "Mun. ideology index" = ideology_municipality)

baseline_table <- modelsummary(models,
                               estimate = "{estimate}",
                               statistic = c("{p.value}{stars}","[{conf.low}, {conf.high}]"
                               ),
                               coef_rename = c("Robust" = "RD estimator"),
                               stars = c('*'=.1, '**'=.05, '***'=.01),
                               fmt = 2, # decimal places
                               #output = "Dados/output/bigsample_baseline.tex",
                               output = "gt",
                               title = "Baseline Characteristis - RD estimates",
                               coef_omit = "Corrected|Conventional")%>%
  tab_spanner(label = "Demography", columns = 2:8) %>% 
  tab_spanner(label = "Health", columns = 9:12) %>%
  tab_spanner(label = "Ideology", columns = 13)

baseline_table

gt::gtsave(baseline_table, filename =  "Dados/output/221201_bigsample_robust_baseline_halfband.tex")






### dobro

covsZ = cbind(state.d)
poli = 1
k = "triangular"


taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, h = taxa_analfabetismo_18_mais[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
indice_gini <- rdrobust(df$indice_gini, h = indice_gini[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
renda_pc <- rdrobust(df$renda_pc, h = renda_pc[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
populacao_2010 <- rdrobust(df$populacao_2010, h = populacao_2010[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
idhm <- rdrobust(df$idhm, h = idhm[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
per_populacao_urbana <- rdrobust(df$per_populacao_urbana, h = per_populacao_urbana[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
per_populacao_homens <- rdrobust(df$per_populacao_homens, h = per_populacao_homens[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun, h = pct_desp_recp_saude_mun[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
tx_med_ch <- rdrobust(df$tx_med_ch, h = tx_med_ch[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
cob_esf <- rdrobust(df$cob_esf, h = cob_esf[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
tx_leito_sus <- rdrobust(df$tx_leito_sus, h = tx_leito_sus[["bws"]][1] * 4,  df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)
ideology_municipality <- rdrobust(df$ideology_municipality, h = ideology_municipality[["bws"]][1] * 4, df$X, p = poli,  kernel = k, subset = amostra, covs = covsZ)


models <- list("Illiteracy" = taxa_analfabetismo_18_mais,
               "Gini" = indice_gini,
               "PC income" = renda_pc,
               "Population" = populacao_2010,
               "HDI" = idhm,
               "% Urb. Pop" = per_populacao_urbana,
               "% Masc. Pop" = per_populacao_homens,
               "% Health municipal spending" = pct_desp_recp_saude_mun,
               "Doctors per 1k pop." = tx_med_ch,
               "Community health agents program " = cob_esf,
               "Hosp. beds per 100k pop." = tx_leito_sus,
               "Mun. ideology index" = ideology_municipality)

baseline_table <- modelsummary(models,
                               estimate = "{estimate}",
                               statistic = c("{p.value}{stars}","[{conf.low}, {conf.high}]"
                               ),
                               coef_rename = c("Robust" = "RD estimator"),
                               stars = c('*'=.1, '**'=.05, '***'=.01),
                               fmt = 2, # decimal places
                               #output = "Dados/output/bigsample_baseline.tex",
                               output = "gt",
                               title = "Baseline Characteristis - RD estimates",
                               coef_omit = "Corrected|Conventional")%>%
  tab_spanner(label = "Demography", columns = 2:8) %>% 
  tab_spanner(label = "Health", columns = 9:12) %>%
  tab_spanner(label = "Ideology", columns = 13)

baseline_table

gt::gtsave(baseline_table, filename =  "Dados/output/221201_bigsample_robust_baseline_double.tex")








# Placebos ----------------------------------------------------------------



library("rddensity")

### hosp

hosp_rdd <- rdd_data(y = Y_hosp, x = X, covar = cbind(log(df$tenure + 1), state.d, year.d), cutpoint = 0, data = df)
plot(hosp_rdd)

bw_ik <- rdd_bw_ik(hosp_rdd)
reg_nonpara <- rdd_reg_lm(rdd_object = hosp_rdd, bw = 0.09, covariates =  NULL)
print(reg_nonpara)


teste <- plotSensi(reg_nonpara, from = 0.02, to = 0.15, by = 0.005, level = 0.90,
          output = "ggplot") 



plotPlacebo(reg_nonpara, level = 0.90)

dens_test(reg_nonpara)




# testando
#### deaths

deaths_rdd <- rdd_data(y = Y_deaths_sivep, x = X, cutpoint = 0, data = subset(df, !is.na(Y_deaths_sivep) & coorte == 2016 & sch_non_stem_cdt == 1))
plot(deaths_rdd)

bw_ik <- rdd_bw_ik(deaths_rdd)
reg_nonpara <- rdd_reg_np(rdd_object = deaths_rdd, bw = 0.05)
print(reg_nonpara)

plotSensi(reg_nonpara, from = 0.02, to = 0.1, by = 0.005, level = 0.90)

plotPlacebo(reg_nonpara,  level = 0.90)

dens_test(reg_nonpara)

#### total_nfi

nfi_rdd <- rdd_data(y = total_nfi, x = X, cutpoint = 0, data = subset(df, coorte == 2016 & sch_non_stem_cdt == 1  & !is.na(Y_deaths_sivep)))
plot(nfi_rdd)

bw_ik <- rdd_bw_ik(nfi_rdd)
reg_nonpara <- rdd_reg_np(rdd_object = nfi_rdd, bw = 0.04)
print(reg_nonpara)

plotSensi(reg_nonpara, from = 0.02, to = 0.15, by = 0.005, level = 0.90)

plotPlacebo(reg_nonpara,  level = 0.90)

dens_test(reg_nonpara)



# Criando tabela para entrevistar -----------------------------------------

df %>% 
  filter(stem_job_4 == 1 & coorte == 2016) %>% 
  summarise(sigla_uf, city, nome, nome_urna, ocupacao, tenure, Y_deaths_sivep,cbo_agregado_nome) %>% 
  arrange(Y_deaths_sivep) %>% 
  print(n = 100)




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


sivep_2020$grupo = ifelse(sivep_2020$CO_MUN_RES %in% subset(df, stem_job_4 == 1 & coorte == 2016)$id_municipio, "STEM",
                          ifelse(sivep_2020$CO_MUN_RES %in% subset(df, stem_job_4 == 0 & coorte == 2016)$id_municipio, "Non-STEM", NaN)) 


deaths_group_2020 <- sivep_2020 %>% 
  filter(grupo == "STEM" | grupo == "Non-STEM") %>% 
  group_by(grupo, month) %>% 
  summarise(death_mean = mean(deaths),
            hosp_mean = mean(hosp),
            hosp_ci_min = t.test(hosp, conf.level = 0.90)$conf.int[1],
            hosp_ci_max = t.test(hosp, conf.level = 0.90)$conf.int[2],
            death_ci_min = t.test(deaths, conf.level = 0.90)$conf.int[1],
            death_ci_max = t.test(deaths, conf.level = 0.90)$conf.int[2],
            coorte = 2016) %>%
  arrange(desc(death_mean)) 
  
  
hosp_2020 <- ggplot(data = deaths_group_2020, aes(x = month, y = hosp_mean, group = grupo, color = grupo)) +
              geom_line() +
              geom_point() 
              #geom_ribbon(aes(x = month, ymin = hosp_ci_min, ymax = hosp_ci_max, fill = grupo), alpha = 0.2)

death_2020 <- ggplot(data = deaths_group_2020, aes(x = month, y = death_mean, group = grupo, color = grupo)) +
  geom_line() +
  geom_point()
  #geom_ribbon(aes(x = month, ymin = death_ci_min, ymax = death_ci_max, fill = grupo), alpha = 0.2)


death_2020  / hosp_2020 

