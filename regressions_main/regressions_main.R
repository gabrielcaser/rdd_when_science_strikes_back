# Program - This program run main RDD regressions, including robustness, tables and pictures 


# Libraries ***************************************************** ---------------------------------------------------------------

#library("stargazer")
#library("gt")
#library("modelsummary")
#library("rdrobust")
#library("rddtools")
#library("readr")
#library("stringr")
#library("tidyverse")
#
#library("plyr")
#library("sf")
#library("rio")
#library('patchwork')

# Oppening Covid and RDD Data ----------------------------------------------------

df <- readRDS(paste(create_dataset_for_regressions, "/data/rdd_data_moderation_broaddefinition.rds", sep = ""))


# Creating functions -----------------------------------

## table of results

tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[1, 1],
    std.error = model$se[3, 1],
    p.value = model$pv[, 1],
    conf.low = model$ci[3,1],
    conf.high = model$ci[3,2])
  
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

## table of robustness


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

# Defining regressions' parameters

df <- df[df$sch_non_stem_cdt == 1, ]

state.f = factor(df$sigla_uf)

state.d = model.matrix(~state.f+0)


year.f = factor(df$coorte)

year.d = model.matrix(~year.f+0)





# Main Results -------------------------------------------------------------


# baseline


covsZ = cbind(state.d, year.d)
poli = 1
#janela = 0.05
janela = cbind()
k = "triangular"

#rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)

#taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
#indice_gini <- rdrobust(df$indice_gini,  df$X, p = poli, kernel = k, h = janela,  covs = covsZ)
renda_pc <- rdrobust(df$renda_pc,  df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
populacao <- rdrobust(log(df$populacao),  df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
idhm <- rdrobust(df$idhm,  df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
densidade <- rdrobust(df$densidade,  df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
per_populacao_homens <- rdrobust(df$per_populacao_homens,  df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun,  df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
tx_med_ch <- rdrobust(df$tx_med_ch, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
cob_esf <- rdrobust(df$cob_esf,  df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
tx_leito_sus <- rdrobust(df$tx_leito_sus, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
ideology_municipality <- rdrobust(df$ideology_municipality, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)



models <- list(
              # "Gini" = indice_gini,
               "PC income" = renda_pc,
               "Log Population" = populacao,
               #"Illiteracy" = taxa_analfabetismo_18_mais,
               "HDI" = idhm,
               "Density" = densidade,
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
                       #output = "Dados/output/bigsample_baseline.tex",
                       output = "gt",
                       title = "Baseline Characteristis - RD estimates",
                       coef_omit = "Corrected|Conventional")%>%
  tab_spanner(label = "Demography", columns = 2:6) %>% 
  tab_spanner(label = "Health", columns = 7:10) %>%
  tab_spanner(label = "Ideology", columns = 11)



baseline_table

gt::gtsave(baseline_table, filename =  "output/240803_bigsample_baseline.tex")


# estimates

covsZ = cbind(state.d, year.d) 
poli = 1
janela = cbind()

r2 = rdrobust(df$Y_hosp,  df$X, p = poli, kernel = k, h = janela,   bwselect = "mserd", covs = covsZ)
r3 = rdrobust(df$Y_deaths_sivep, df$X, p = poli, kernel = k,  h = janela,  bwselect = "mserd", covs = covsZ)

summary(r2)
summary(r3)

covsZ = cbind(state.d, year.d, df$renda_pc, log(df$populacao), df$idhm, log(df$densidade), df$per_populacao_homens, df$pct_desp_recp_saude_mun, df$tx_med_ch, df$cob_esf, df$tx_leito_sus, df$ideology_municipality, df$mulher, df$reeleito, df$ideology_party, df$idade)
poli = 1
janela = NULL
#k = "triangular"

r4 = rdrobust(df$Y_hosp,  df$X, p = poli, kernel = k,  h = janela, covs = covsZ)
r5 = rdrobust(df$Y_deaths_sivep, df$X, kernel = k, h = janela,    p = poli, covs = covsZ)

summary(r4)
summary(r5)

covsZ = cbind(state.d, year.d) 
poli = 1
janela = 0.05

r6 = rdrobust(df$Y_hosp, df$X, p = poli, kernel = k,  h = janela,  bwselect = "mserd",  covs = covsZ)
r7 = rdrobust(df$Y_deaths_sivep,  df$X, p = poli, kernel = k, h = janela,   bwselect = "mserd",   covs = covsZ)


covsZ = cbind(state.d, year.d, df$renda_pc, log(df$populacao), df$idhm, log(df$densidade), df$per_populacao_homens, df$pct_desp_recp_saude_mun, df$tx_med_ch, df$cob_esf, df$tx_leito_sus, df$ideology_municipality, df$mulher, df$reeleito, df$ideology_party, df$idade)
poli = 1

r8 = rdrobust(df$Y_hosp ,  df$X, p = poli, kernel = k, h = janela,   bwselect = "mserd",  covs = covsZ)
r9 = rdrobust(df$Y_deaths_sivep,  df$X, kernel = k, h = janela,   bwselect = "mserd", p = poli,   covs = covsZ)



models <- list("Panel A: Deaths" = list(r7,
                                        r9,
                                        r3,
                                        r5),
               "Panel B: Hospitalizations" = list(r6,
                                                  r8,
                                                  r2,
                                                  r4)
               )
  
teste <- modelsummary(models,
                      shape = "rbind",
                      estimate = "{estimate}",
                      statistic = c("[{std.error}]","{p.value}{stars}"
                      ),
                      coef_rename = c("Robust" = "RD estimator"),
                      stars = c('*'=.1, '**'=.05, '***'=.01),
                      fmt = 2, # decimal places
                      #output = paste(output_dir, "/bigsample_estimates.tex", sep = ""),
                      output = "gt",
                      title = "Impact of STEM Leadership on Epidemiological Outcomes — RD estimates",
                      coef_omit = "Corrected|Conventional") 


teste
gt::gtsave(teste, filename =  "output/240803_bigsample_estimates.tex")



# Personal charact

covsZ = cbind(state.d, year.d)
janela = cbind()
#janela = 0.05
poli = 1

mulher <- rdrobust(df$mulher, h = janela, df$X, p = poli, kernel = k,   covs = covsZ)
reeleito <- rdrobust(df$reeleito, h = janela, df$X, p = poli, kernel = k,   covs = covsZ)
idade <- rdrobust(df$idade, h = janela, df$X, p = poli, kernel = k,  covs = covsZ)
#ens.sup <- rdrobust(df$instrucao, h = janela, df$X, p = poli, kernel = k,  covs = covsZ)
ideology <- rdrobust(df$ideology_party, h = janela, df$X, p = poli, kernel = k,  covs = covsZ)



models <- list(
  "Women" = mulher,
  "Incumbent" = reeleito,
  "Age" = idade,
  #"Education" = ens.sup,
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
gt::gtsave(teste_chr, filename =  "output/240803_personal_char.tex")




### means

df %>%
  dplyr::filter(X>= -0.10 & X <= 0.10) %>% 
  dplyr::group_by(stem_background) %>% 
  dplyr::summarise(mean(Y_deaths_sivep, na.rm = TRUE), mean(Y_hosp, na.rm = TRUE), n())

2.19 / 3.92

53.68 / 144



# Mechanism  -----------------------

df2 <- df[df$coorte == 2016, ]
state.f2 = factor(df2$sigla_uf)
state.d2 = model.matrix(~state.f2+0)


covsZ = cbind(state.d2)
poli = 1
k = "triangular"
#janela = 0.08
janela = cbind()



r1 = rdrobust(df2$total_nfi,  df2$X, p = poli, kernel = k,   covs = covsZ, h = janela)
r2 = rdrobust(df2$mascaras, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)
r3 = rdrobust(df2$restricao_atv_nao_essenciais, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)
r4 = rdrobust(df2$restricao_circulacao, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)
r5 = rdrobust(df2$restricao_transporte_publico, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)
r6 = rdrobust(df2$barreiras_sanitarias, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)

covsZ = cbind(state.d2, df2$renda_pc, log(df2$populacao), df2$idhm, df2$densidade, df2$per_populacao_homens, df2$pct_desp_recp_saude_mun, df2$tx_med_ch, df2$cob_esf, df2$tx_leito_sus, df2$ideology_municipality, df2$mulher, df2$reeleito, df2$ideology_party, df2$idade)

poli = 1

r12 = rdrobust(df2$total_nfi, df2$X, p = poli, kernel = k,   covs = covsZ, h = janela)
r22 = rdrobust(df2$mascaras, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)
r32 = rdrobust(df2$restricao_atv_nao_essenciais, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)
r42 = rdrobust(df2$restricao_circulacao, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)
r52 = rdrobust(df2$restricao_transporte_publico, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)
r62 = rdrobust(df2$barreiras_sanitarias, df2$X,   p = poli, kernel = k,   covs = covsZ, h = janela)


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


mr3 <- mr3 %>%
  tab_spanner(label = "(1)", columns = 2:7) %>% 
  tab_spanner(label = "(2)", columns = 8:13)

gt::gtsave(mr3, filename =  "output/240803_mechanism.tex")


## controlling (testing) mechanism


covsZ = cbind(state.d2, df2$total_nfi)

poli = 1

r1 = rdrobust(df2$Y_hosp, df2$X,  p = poli, kernel = k,  covs = covsZ, h = janela)
r2 = rdrobust(df2$Y_deaths_sivep, df2$X,  p = poli, kernel = k,  covs = covsZ, h = janela)

poli = 2
covsZ = cbind(state.d2, df2$total_nfi)

r3 = rdrobust(df2$Y_hosp, df2$X,  p = poli, kernel = k,  covs = covsZ, h = janela)
r4 = rdrobust(df2$Y_deaths_sivep, df2$X,  p = poli, kernel = k,  covs = covsZ, h = janela)

models <- list("Hospitalizations" = r1,
               "Deaths" = r2,
               "Hospitalizations" = r3,
               "Deaths" = r4)


mr4 <- modelsummary(models,
                    estimate = "{estimate}",
                    statistic = c("{p.value}{stars}","[{conf.low}, {conf.high}]"),
                    coef_rename = c("Robust" = "RD estimator"),
                    stars = c('*'=.1, '**'=.05, '***'=.01),
                    fmt = 2, # decimal places
                    output = "gt",
                    title = "Impact of STEM Candidate controlling for Non-Pharmaceutical Intervations (NPIs) — RD estimates with fixed effects (optimal bandwidth)",
                    coef_omit = "Bias-Corrected|Conventional")

mr4 %>%
 tab_spanner(label = "Linear", columns = 2:3) %>% 
 tab_spanner(label = "Quadratic", columns = 4:5)


# Robustness --------------------------------------------------------------


## Different windows -----------------------------------------------------

CovsZ = cbind(state.d, year.d, df$mulher) # mechanisms

CovsZ_mechanism = cbind(state.d2, df2$mulher) # mechanisms

k = 'triangular'

  
df_robs_hosp <- robust_check(df$Y_hosp, 1, CovsZ, k)
df_robs_deaths <- robust_check(df$Y_deaths_sivep, 1, CovsZ, k)
df_robs_nfi <- robust_check(df2$total_nfi, 1, CovsZ_mechanism, k)
df_robs_masks <- robust_check(df2$mascaras, 1, CovsZ_mechanism, k)
df_robs_trans_pub <- robust_check(df2$restricao_transporte_publico, 1, CovsZ_mechanism, k)
df_robs_circu <- robust_check(df2$restricao_circulacao, 1, CovsZ_mechanism, k)
df_robs_atv <- robust_check(df2$restricao_atv_nao_essenciais, 1, CovsZ_mechanism, k)
df_robs_sani <- robust_check(df2$barreiras_sanitarias, 1, CovsZ_mechanism, k)





plot_hosp_robs <-  ggplot(df_robs_hosp, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.2) +
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
  xlim(0.00, 0.2) +
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

ggsave("output/240803_mechanism_robust_outcomes.png", graficos_juntos,
       width = 5.50,
       height = 5.00,
       units = "in")


plot_nfi_robs <-  ggplot(df_robs_nfi, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.2) +
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
  xlim(0.02, 0.2) +
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
  xlim(0.02, 0.2) +
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
  xlim(0.02, 0.2) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(d) Cordon sanitaire restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_nfi_atv <-  ggplot(df_robs_atv, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.2) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(e) Non-essential activ. restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)

plot_nfi_circu <-  ggplot(df_robs_circu, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.2) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(f) Public gathering restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2)




graf <- (plot_nfi_robs + plot_nfi_masks) / ( plot_nfi_sani + plot_nfi_trans) / (plot_nfi_atv + plot_nfi_circu)

graf

ggsave("output/240803_mechanism_npi_rob.png", graf,
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

taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
indice_gini <- rdrobust(df$indice_gini,  df$X, p = poli, kernel = k,  covs = covsZ)
renda_pc <- rdrobust(df$renda_pc,  df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
populacao_2010 <- rdrobust(df$populacao_2010,  df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
idhm <- rdrobust(df$idhm,  df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
per_populacao_urbana <- rdrobust(df$per_populacao_urbana,  df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
per_populacao_homens <- rdrobust(df$per_populacao_homens,  df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun,  df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
tx_med_ch <- rdrobust(df$tx_med, df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
cob_esf <- rdrobust(df$cob_esf,  df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
tx_leito_sus <- rdrobust(df$tx_leito_sus, df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)
ideology_municipality <- rdrobust(df$ideology_municipality, df$X, p = poli, kernel = k, bwselect = "mserd",  covs = covsZ)



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



### half

covsZ = cbind(state.d,year.d)
poli = 1
k = "uniform"


taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, h = taxa_analfabetismo_18_mais[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
indice_gini <- rdrobust(df$indice_gini, h = indice_gini[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
renda_pc <- rdrobust(df$renda_pc, h = renda_pc[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
populacao_2010 <- rdrobust(df$populacao_2010, h = populacao_2010[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
idhm <- rdrobust(df$idhm, h = idhm[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
per_populacao_urbana <- rdrobust(df$per_populacao_urbana, h = per_populacao_urbana[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
per_populacao_homens <- rdrobust(df$per_populacao_homens, h = per_populacao_homens[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun, h = pct_desp_recp_saude_mun[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
tx_med_ch <- rdrobust(df$tx_med, h = tx_med_ch[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
cob_esf <- rdrobust(df$cob_esf, h = cob_esf[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)
tx_leito_sus <- rdrobust(df$tx_leito_sus, h = tx_leito_sus[["bws"]][1] / 2,  df$X, p = poli,  kernel = k,  covs = covsZ)
ideology_municipality <- rdrobust(df$ideology_municipality, h = ideology_municipality[["bws"]][1] / 2, df$X, p = poli,  kernel = k,  covs = covsZ)


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

#gt::gtsave(baseline_table, filename =  "Dados/output/221201_bigsample_robust_baseline_halfband.tex")



### double

covsZ = cbind(state.d, year.d)
poli = 1
k = "uniform"


taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, h = taxa_analfabetismo_18_mais[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
indice_gini <- rdrobust(df$indice_gini, h = indice_gini[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
renda_pc <- rdrobust(df$renda_pc, h = renda_pc[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
populacao_2010 <- rdrobust(df$populacao_2010, h = populacao_2010[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
idhm <- rdrobust(df$idhm, h = idhm[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
per_populacao_urbana <- rdrobust(df$per_populacao_urbana, h = per_populacao_urbana[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
per_populacao_homens <- rdrobust(df$per_populacao_homens, h = per_populacao_homens[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun, h = pct_desp_recp_saude_mun[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
tx_med_ch <- rdrobust(df$tx_med, h = tx_med[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
cob_esf <- rdrobust(df$cob_esf, h = cob_esf[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)
tx_leito_sus <- rdrobust(df$tx_leito_sus, h = tx_leito_sus[["bws"]][1] * 4,  df$X, p = poli,  kernel = k,  covs = covsZ)
ideology_municipality <- rdrobust(df$ideology_municipality, h = ideology_municipality[["bws"]][1] * 4, df$X, p = poli,  kernel = k,  covs = covsZ)


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

#gt::gtsave(baseline_table, filename =  "Dados/output/221201_bigsample_robust_baseline_double.tex")








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


