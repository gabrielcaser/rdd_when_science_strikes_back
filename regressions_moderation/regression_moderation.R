# This program run moderation analysis

# TO-DO
 ## Acho que só tenho o tenure dos caras que ganharam. Seria importante ter o dos que perderam.Ou talvez não!

# Setting

## Libraries


library(dplyr)
library(plm)
library(readxl)
library(stargazer)

## Directories
work_dir = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/regressions_moderation"
output_dir  = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/regressions_moderation/output"
rdd_dir = "C:/Users/GabrielCaserDosPasso/Documents/RAIS"
create_dataset_for_regressions = "C:/Users/GabrielCaserDosPasso/Documents/RAIS/create_dataset_for_regressions/output/data"

# Oppening Covid and RDD Data ----------------------------------------------------

df <- readRDS(paste(create_dataset_for_regressions, "/rdd_data_moderation.rds", sep = ""))

# Merging with Revenue data

data_revenue <- read_excel(paste(work_dir,"/input/ipea_receitas_municipais.xlsx", sep = ""), sheet = "Receitas Totais")

data_revenue <- data_revenue %>% 
  summarise(id_municipio = Cod.IBGE,
            receita_2015 = `2015` / 10000000)

df <- merge(df, data_revenue, by = "id_municipio")

rm(data_revenue)

# Regression OLS with moderator

## Creating interaction term

df$tenure <- df$tenure / 12

df_subset <- subset(df, X >= -0.05 & X <= 0.05 & coorte == 2016)

inter_receita_stem <- df_subset$receita_2015 * df_subset$stem_job_4

inter_tenure_stem <- df_subset$tenure * df_subset$stem_job_4

inter_ideo_stem <- df_subset$ideology_party * df_subset$stem_job_4

inter_X_stem <- df_subset$X * df_subset$stem_job_4

#reg_hosp <- lm(Y_hosp ~ Y_hosp + renda_pc + tenure + X + inter_renda_stem + inter_tenure_stem + inter_X_stem, data = df)
#summary(reg_hosp)


## Regression with vote shares



df_subset$T = ifelse(df_subset$X >= 0, 1, 0)

df_subset$T_X = df_subset$X * df_subset$T

# OLS

## most simple regression without revenue
out = lm(Y_hosp ~ X + T + T_X, data = df_subset)
summary(out)
out = lm(Y_deaths_sivep ~ X + T + T_X , data = df_subset)
summary(out)

## adding mayors characteristcs without revenue
out = lm(Y_hosp ~ X + T + T_X + instrucao + mulher + idade + reeleito + ideology_party + tenure, data = df_subset)
summary(out)
out = lm(Y_deaths_sivep ~ X + T + T_X + instrucao + mulher + idade + reeleito + ideology_party + tenure, data = df_subset)
summary(out)


## most simple regression with revenue
out = lm(Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem  , data = df_subset)
summary(out)
out = lm(Y_deaths_sivep ~ X + T + T_X + receita_2015 + inter_receita_stem  , data = df_subset)
summary(out)

## adding mayor characteristics
out = lm(Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem + ens_sup + mulher + idade , data = df_subset)
summary(out)
out = lm(Y_deaths_sivep ~ X + T + T_X + receita_2015 + inter_receita_stem + ens_sup + mulher + idade  , data = df_subset)
summary(out)


# FE
pdata <- pdata.frame(df_subset, c("sigla_uf"))

## most simple regression without revenue but with state FE 
out <- plm(Y_hosp ~ X + T + T_X, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out <- plm(Y_deaths_sivep ~ X + T + T_X, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

## adding mayors characteristics without revenue but with state FE 
out <- plm(Y_hosp ~ X + T + T_X + instrucao + mulher, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out <- plm(Y_deaths_sivep ~ X + T + T_X + instrucao + mulher, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)


## most simple regression with state FE 
out <- plm(Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out <- plm(Y_deaths_sivep ~ X + T + T_X + receita_2015 + inter_receita_stem, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

## adding mayor characteristics
out1 <- plm(Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out2 <- plm(Y_deaths_sivep ~ X + T + T_X + receita_2015 + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out3 <- plm(total_nfi ~ X + T + T_X + receita_2015 + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

stargazer(out1, out2, out3,
          type = "text",
          #covariate.labels = c("X - Share of votes", "T - STEM Background", "Int. T X", "2015 Revenue", "Rev Moder. Eff.", "College", "Female", "Party Ideology", "Age", "Incubent"),
          dep.var.labels = c("Hospitalizations", "Deaths", "NFI"))

## adding tenure
out1 <- plm(Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem + ens_sup + mulher + ideology_party + idade + reeleito + tenure, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out2 <- plm(Y_deaths_sivep ~ X + T + T_X + receita_2015 + inter_receita_stem + ens_sup + mulher + ideology_party + idade + reeleito + tenure, data = pdata, index = c("sigla_uf", "coorte"), model = "within")
summary(out)
out3 <- plm(total_nfi ~ X + T + T_X + receita_2015 + inter_receita_stem + ens_sup + mulher + ideology_party + idade + reeleito + tenure, data = pdata, index = c("sigla_uf", "coorte"), model = "within")
summary(out)

stargazer(out1, out2, out3,
           type = "text",
           covariate.labels = c("X - Share of votes", "T - STEM Background", "Int. T X", "2015 Revenue", "Rev Moder. Eff.", "College", "Female", "Party Ideology", "Age", "Incubent", "Tenure"),
           dep.var.labels = c("Hospitalizations", "Deaths", "NFI"))


##pdata$tenure <- ifelse(pdata$tenure == 0, NaN, pdata$tenure)

## adding tenure interaction
out1 <- plm(Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito + tenure  + inter_tenure_stem, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out2 <- plm(Y_deaths_sivep ~ X + T + T_X + receita_2015  + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito + tenure + inter_tenure_stem , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out3 <- plm(total_nfi ~ X + T + T_X + receita_2015 + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito + tenure + inter_tenure_stem , data = pdata, index = c("sigla_uf", "coorte"), model = "within")
summary(out)

stargazer(out1, out2, out3,
          type = "text",
          covariate.labels = c("STEM Background", "2015 Revenue", "Rev Modereration Effect", "Tenure", "Tenure Moderation Effect"),
          dep.var.labels = c("Hospitalizations", "Deaths", "NFI"),
          out = paste(output_dir, "/tables/moderation_both.tex", sep = ""),
          omit = c("X", "T_X", "instrucao", "mulher", "idade", "ideology_party", "reeleito"))

## removing revenue 

out1 <- plm(Y_hosp ~ X + T + T_X  + instrucao + mulher + ideology_party + idade + reeleito + tenure  + inter_tenure_stem, data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out2 <- plm(Y_deaths_sivep ~ X + T + T_X  + instrucao + mulher + ideology_party + idade + reeleito + tenure + inter_tenure_stem , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out3 <- plm(total_nfi ~ X + T + T_X + instrucao + mulher + ideology_party + idade + reeleito + tenure + inter_tenure_stem , data = pdata, index = c("sigla_uf", "coorte"), model = "within")
summary(out)

table <- stargazer(out1, out2, out3,
                   type = "text",
                   #covariate.labels = c("Share of votes", "STEM Background", "Interaction Votes x STEM", "2015 Revenue", "Rev Moder. Eff.", "Incomplete H. education", "Complete H. education", "Complete College", "Incomplete College","Reads and writes","Female", "Party Ideology", "Age", "Incubent","Tenure Moder. Eff."),
                   dep.var.labels = c("Hospitalizations", "Deaths", "NFI"))

# Final table

out1 <- plm(Y_hosp ~ X + T + T_X + tenure  + inter_tenure_stem + instrucao + mulher + ideology_party + idade + reeleito , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out2 <- plm(Y_deaths_sivep ~ X + T + T_X + tenure + inter_tenure_stem + instrucao + mulher + ideology_party + idade + reeleito , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out3 <- plm(total_nfi ~ X + T + T_X + tenure + inter_tenure_stem + instrucao + mulher + ideology_party + idade + reeleito , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

stargazer(out2, out1, out3,
          type = "text",
          covariate.labels = c("STEM Background",  "Tenure", "Tenure Moderation Effect"),
          dep.var.labels = c("Deaths", "Hospitalizations","NFI", "Deaths", "Hospitalizations", "NFI"),
          out = paste(output_dir, "/tables/moderation_tenure.txt", sep = ""),
          omit = c("X", "T_X", "instrucao", "mulher", "idade", "ideology_party", "reeleito"))


out4 <- plm(Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out5 <- plm(Y_deaths_sivep ~ X + T + T_X + receita_2015  + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
out6 <- plm(total_nfi ~ X + T + T_X + receita_2015 + inter_receita_stem + instrucao + mulher + ideology_party + idade + reeleito , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

stargazer(out2, out1, out3, out5, out4, out6,
          type = "text",
          covariate.labels = c("STEM Background",  "Tenure", "Tenure Moderation Effect", "2015 Revenue", "Revenue Modereration Effect"),
          dep.var.labels = c("Deaths", "Hospitalizations","NFI", "Deaths", "Hospitalizations", "NFI"),
          out = paste(output_dir, "/tables/moderation_revenue.txt", sep = ""),
          omit = c("X", "T_X", "instrucao", "mulher", "idade", "ideology_party", "reeleito"))


## baseline char

out <- plm(receita_2015 ~ X + T + T_X , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

out <- plm(renda_pc ~ X + T + T_X , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

out <- plm(taxa_analfabetismo_18_mais ~ X + T + T_X , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)


out <- plm(densidade ~ X + T + T_X , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

out <- plm(idhm ~ X + T + T_X , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

out <- plm(tx_med_ch ~ X + T + T_X , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)

out <- plm(ideology_municipality ~ X + T + T_X , data = pdata, index = c("sigla_uf"), model = "within")
summary(out)
