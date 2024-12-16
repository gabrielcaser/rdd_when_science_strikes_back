# Program - This program run main RDD regressions, including robustness, tables and pictures 


# Oppening ----------------------------------------------------------------

df <- readRDS(paste(data_dir, "/final/", data, sep = ""))

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
    "Eff N obs." = as.character(model$N_h[1] + model$N_h[2]),
    "Bandwidth" = as.character(round(model$bws[1,1] * 100, 2))#,
    #"State FE" = "X",
    #"Election FE" = "X",
    #"Gender" = ifelse("mulher" %in% colnames(as.data.frame(covsZ)),"X"," ")
  )
  ret
}

## table of robustness


robust_check <- function(outcome, poli, covsZ, k, x_value) {
  
  df_robs <- data.frame()
  
  
  for (i in seq(0.02, 0.24, by = 0.01)) {
    
    prov = rdrobust(y = outcome, x_value, p = poli, level = 90, kernel = k, h = i, covs = covsZ) #rodando rdd

    df_robs = rbind(df_robs, c(i, prov[["coef"]][1], prov[["coef"]][3], prov[["ci"]][3,1], prov[["ci"]][3,2], prov[["ci"]][1,1], prov[["ci"]][1,2], prov[["z"]][3], prov[["N_h"]][1] + prov[["N_h"]][2])) #salvando colunas
    
  }
  
  colnames(df_robs) <- c("bw", "coef_conv", "coef_robs", "ci_lower_rob", "ci_higher_rob",  "ci_lower_conv", "ci_higher_conv", "z", "eff_n")
  
  
  return(df_robs)
  
  
}

# Creating state dummies for fixed effects --------------------------------

# Defining regressions' parameters


state.f = factor(df$sigla_uf)

state.d = model.matrix(~state.f+0)


year.f = factor(df$coorte)

if (cohort_filter == "") {
  year.d = model.matrix(~year.f+0)
}
if (cohort_filter == "2016_") {
  year.d = 1
}



# Main Results -------------------------------------------------------------


# baseline


covsZ = cbind(state.d, year.d)
#rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)

#taxa_analfabetismo_18_mais <- rdrobust(df$taxa_analfabetismo_18_mais, df$X, p = poli, kernel = k, h = janela, bwselect = "mserd",  covs = covsZ)
#indice_gini <- rdrobust(df$indice_gini,  df$X, p = poli, kernel = k, h = janela,  covs = covsZ)
renda_pc                <- rdrobust(df$renda_pc,  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
populacao               <- rdrobust(log(df$populacao),  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
idhm                    <- rdrobust(df$idhm,  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
densidade               <- rdrobust(df$densidade,  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
per_populacao_homens    <- rdrobust(df$per_populacao_homens,  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
pct_desp_recp_saude_mun <- rdrobust(df$pct_desp_recp_saude_mun,  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
tx_med_ch               <- rdrobust(df$tx_med_ch, df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
cob_esf                 <- rdrobust(df$cob_esf,  df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
tx_leito_sus            <- rdrobust(df$tx_leito_sus, df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)
ideology_municipality   <- rdrobust(df$ideology_municipality, df$X, p = poli, kernel = k,  bwselect = "mserd",  covs = covsZ)


# Models for Panel 1 (Demography)
models_1 <- list(
  "PC income" = renda_pc,
  "Log Population" = populacao,
  "HDI" = idhm,
  "Density" = densidade,
  "% Masc. Pop" = per_populacao_homens
)

# Models for Panel 2 (Health and Ideology)
models_2 <- list(
  "% Health municipal spending" = pct_desp_recp_saude_mun,
  "Doctors per 1k pop." = tx_med_ch,
  "Community health agents program" = cob_esf,
  "Hosp. beds per 100k pop." = tx_leito_sus,
  "Mun. ideology index" = ideology_municipality
)

# Create the first table (Demography)
baseline_table_1 <- modelsummary(
  models_1,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  output = "outputs/tables/baseline_table_panel1.md",
  title = "Baseline Characteristics - RD Estimates (Demography)",
  coef_omit = "Corrected|Conventional"#,
  #align = paste(rep("c", length(models_1) + 1), collapse = "")
)

# Create the second table (Health and Ideology)
baseline_table_2 <- modelsummary(
  models_2,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  output = "outputs/tables/baseline_table_panel2.md",
  title = "Baseline Characteristics - RD Estimates (Health and Ideology)",
  coef_omit = "Corrected|Conventional"#,
  #align = paste(rep("c", length(models_2) + 1), collapse = "")
)

baseline_table_1
baseline_table_2

# estimates

covsZ = cbind(state.d, year.d) 

r2 = rdrobust(df$Y_hosp,  df$X, p = poli, kernel = k, bwselect = "mserd", covs = covsZ)
r3 = rdrobust(df$Y_deaths_sivep, df$X, p = poli, kernel = k, bwselect = "mserd", covs = covsZ)

summary(r2)
summary(r3)

covsZ = cbind(state.d, year.d, df$mulher, df$ideology_party, df$instrucao, df$reeleito)

r4 = rdrobust(df$Y_hosp,  df$X, p = poli, kernel = k,  covs = covsZ)
r5 = rdrobust(df$Y_deaths_sivep, df$X, kernel = k, p = poli, covs = covsZ)

summary(r4)
summary(r5)

covsZ = cbind(state.d, year.d) 

r6 = rdrobust(df$Y_hosp, df$X, p = poli, kernel = k,  h = janela,  bwselect = "mserd",  covs = covsZ)
r7 = rdrobust(df$Y_deaths_sivep,  df$X, p = poli, kernel = k, h = janela,   bwselect = "mserd",   covs = covsZ)


covsZ = cbind(state.d, year.d, df$mulher, df$instrucao, df$ideology_party, df$reeleito)

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

modelsummary(
  models,
  shape = "rbind",
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  coef_rename = c("Robust" = "RD estimator"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2,
  # decimal places
  #output = paste(output_dir, "/bigsample_estimates.tex", sep = ""),
  output = "outputs/tables/estimates.md",
  title = "Impact of STEM Leadership on Epidemiological Outcomes — RD estimates",
  coef_omit = "Corrected|Conventional"#,
  #align = paste(rep("c", length(models) + 1), collapse = "")
)



# Personal charact

covsZ = cbind(state.d, year.d)
poli = 1

mulher <- rdrobust(df$mulher,  df$X, p = poli, kernel = k,   covs = covsZ)
reeleito <- rdrobust(df$reeleito,  df$X, p = poli, kernel = k,   covs = covsZ)
idade <- rdrobust(df$idade,  df$X, p = poli, kernel = k,  covs = covsZ)
#ens.sup <- rdrobust(df$instrucao,  df$X, p = poli, kernel = k,  covs = covsZ)
ideology <- rdrobust(df$ideology_party,  df$X, p = poli, kernel = k,  covs = covsZ)



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
             output = "outputs/tables/personal_char.md",
             title = "STEM candidates' personal characteristics — RD estimates",
             coef_omit = "Corrected|Conventional")#,
            # align = paste(rep("c", length(models) + 1), collapse = ""))

#teste_chr
#gt::gtsave(teste_chr, filename =  "outputs/tables/personal_char.tex")




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

r1 = rdrobust(df2$total_nfi,  df2$X, p = poli, kernel = k,   covs = covsZ)
r2 = rdrobust(df2$mascaras, df2$X,   p = poli, kernel = k,   covs = covsZ)
r3 = rdrobust(df2$restricao_atv_nao_essenciais, df2$X,   p = poli, kernel = k,   covs = covsZ)
r4 = rdrobust(df2$restricao_circulacao, df2$X,   p = poli, kernel = k,   covs = covsZ)
r5 = rdrobust(df2$restricao_transporte_publico, df2$X,   p = poli, kernel = k,   covs = covsZ)
r6 = rdrobust(df2$barreiras_sanitarias, df2$X,   p = poli, kernel = k,   covs = covsZ)

covsZ = cbind(state.d2, df2$mulher, df2$ideology_party)

r12 = rdrobust(df2$total_nfi, df2$X, p = poli, kernel = k,   covs = covsZ)
r22 = rdrobust(df2$mascaras, df2$X,   p = poli, kernel = k,   covs = covsZ)
r32 = rdrobust(df2$restricao_atv_nao_essenciais, df2$X,   p = poli, kernel = k,   covs = covsZ)
r42 = rdrobust(df2$restricao_circulacao, df2$X,   p = poli, kernel = k,   covs = covsZ)
r52 = rdrobust(df2$restricao_transporte_publico, df2$X,   p = poli, kernel = k,   covs = covsZ)
r62 = rdrobust(df2$barreiras_sanitarias, df2$X,   p = poli, kernel = k,   covs = covsZ)


models <- list(#"Total NFI" = r1,
               #"Masks" = r2,
               #"Restrictions atv." = r3,
               #"Restrictions circu." = r4,
               #"Restrictions transp." = r5,
               #"Sani barriers" = r6,
               "Total NFI" = r12,
               "Masks" = r22,
               "Restrictions atv." = r32,
               "Restrictions circu." = r42,
               "Restrictions transp." = r52,
               "Sani barriers" = r62)


# Create a gt object
mr3 <- modelsummary(
  models,
  estimate = "{estimate}",
  statistic = c("[{std.error}]", "{p.value}{stars}"),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  fmt = 2, # decimal places
  output = "outputs/tables/mechanism.md", # Output as gt table
  title = "Impact of STEM Candidate Elected in 2016 on Non-Pharmaceutical Interventions in 2020",
  coef_omit = "Bias-Corrected|Conventional",
  align = paste(rep("r", length(models) + 1), collapse = "") # Create alignment string
)

# Add tab spanners
#mr3 <- mr3 %>%
#  tab_spanner(label = "(1)", columns = 2:7) %>% 
#  tab_spanner(label = "(2)", columns = 8:13)
#
## Convert gt table to raw HTML and then to Markdown
#markdown_output <- gt::as_raw_html(mr3)
#
## Save Markdown content to a file
#writeLines(markdown_output, con = "outputs/tables/mechanism.md")



# Robustness --------------------------------------------------------------


## Different windows -----------------------------------------------------

CovsZ = cbind(state.d, year.d, df$mulher, df$reeleito, df$instrucao, df$ideology_party) # mechanisms
CovsZ_mechanism = cbind(state.d2, df2$mulher, df2$ideology_party) # mechanisms

  
df_robs_hosp      <- robust_check(df$Y_hosp, 1, CovsZ, k, df$X)
df_robs_deaths    <- robust_check(df$Y_deaths_sivep, 1, CovsZ, k, df$X)
df_robs_nfi       <- robust_check(df2$total_nfi, 1, CovsZ_mechanism, k, df2$X)
df_robs_masks     <- robust_check(df2$mascaras, 1, CovsZ_mechanism, k, df2$X)
df_robs_trans_pub <- robust_check(df2$restricao_transporte_publico, 1, CovsZ_mechanism, k, df2$X)
df_robs_circu     <- robust_check(df2$restricao_circulacao, 1, CovsZ_mechanism, k, df2$X)
df_robs_atv       <- robust_check(df2$restricao_atv_nao_essenciais, 1, CovsZ_mechanism, k, df2$X)
df_robs_sani      <- robust_check(df2$barreiras_sanitarias, 1, CovsZ_mechanism, k, df2$X)

theme_clean <- theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(color = "black")
)

plot_hosp_robs <-  ggplot(df_robs_hosp, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.24) +
  #ylim(-750, 200) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12)) +
  ggtitle("(b) COVID-19 hospitalizations") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2) + theme_clean

plot_hosp_robs 


plot_deaths_robs <-  ggplot(df_robs_deaths, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.00, 0.24) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 12)) +
  ggtitle("(a) COVID-19 deaths") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2) + theme_clean

plot_deaths_robs


plot_deaths_robs / plot_hosp_robs

graficos_juntos <- plot_deaths_robs / plot_hosp_robs

ggsave("outputs/figures/robust_outcomes.png", graficos_juntos,
       width = 5.50,
       height = 5.00,
       units = "in")


plot_nfi_robs <-  ggplot(df_robs_nfi, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
 # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(a) Total number of NPIs") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2) + theme_clean

plot_nfi_robs



plot_nfi_masks <-  ggplot(df_robs_masks, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(b) Face covering restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2) + theme_clean

plot_nfi_masks


plot_nfi_trans <-  ggplot(df_robs_trans_pub, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(c) Transportation restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2) + theme_clean

plot_nfi_trans


plot_nfi_sani <-  ggplot(df_robs_sani, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(d) Cordon sanitaire restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2) + theme_clean

plot_nfi_atv <-  ggplot(df_robs_atv, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(e) Non-essential activ. restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2) + theme_clean

plot_nfi_circu <-  ggplot(df_robs_circu, aes(x = bw, y = coef_robs)) +
  geom_point(na.rm = TRUE) +
  xlim(0.02, 0.24) +
  # ylim(-5,10) +
  ylab("") +
  xlab("bandwidth") +
  theme(axis.title = element_text(size = 10)) +
  ggtitle("(f) Public gathering restrictions") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  geom_ribbon(aes(ymin = ci_lower_rob, ymax = ci_higher_rob), alpha = 0.2) + theme_clean




graf <- (plot_nfi_robs + plot_nfi_masks) / ( plot_nfi_sani + plot_nfi_trans) / (plot_nfi_atv + plot_nfi_circu)

graf

ggsave("outputs/figures/npi_rob.png", graf,
       width = 10.00,
       height = 8.00,
       units = "in")

