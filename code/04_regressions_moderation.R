# This program run moderation analysis


# Oppening ----------------------------------------------------------------

df <- readRDS(paste(data_dir, "/final/", data, sep = ""))

# Merging with Revenue data

data_revenue <- readxl::read_excel(paste(data_dir,"/raw/ipea_receitas_municipais.xlsx", sep = ""), sheet = "Receitas Totais")

data_revenue <- data_revenue %>% 
  summarise(id_municipio = Cod.IBGE,
            receita_2015 = `2015` / 10000000)

df <- merge(df, data_revenue, by = "id_municipio")

rm(data_revenue)

# Regression OLS with moderator

## Creating interaction term

df$tenure <- df$tenure / 12

df_subset <- subset(df, X >= -1 * janela & X <= janela)

df_subset$inter_receita_stem <- df_subset$receita_2015 * (as.double(df_subset$stem_background) - 1)
df_subset$log_tenure         <- log(df_subset$tenure + 1)
df_subset$inter_tenure_stem  <- df_subset$tenure  * (as.double(df_subset$stem_background) - 1)
df_subset$inter_ideo_stem    <- df_subset$ideology_party * (as.double(df_subset$stem_background) - 1)
df_subset$inter_X_stem       <- df_subset$X * (as.double(df_subset$stem_background) - 1)


# FE
pdata <- pdata.frame(df_subset, c("sigla_uf","coorte"))

# Controles

covsZ = cbind(pdata$mulher,
              pdata$ideology_party,
              pdata$instrucao,
              pdata$reeleito)

# Final table
out1 <- plm(
  Y_hosp ~ X + T + T_X + inter_tenure_stem + tenure + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within" ,
  effect = "twoways"
)
out2 <- plm(
  Y_deaths_sivep ~ X + T + T_X + inter_tenure_stem + tenure + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"  ,
  effect = "twoways"
)
out3 <- plm(
  total_nfi ~ X + T + T_X + inter_tenure_stem + tenure + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within" ,
  effect = "twoways"
)


moderation_tenure <- stargazer::stargazer(
  out2,
  out1,
  out3,
  type = "latex",
  covariate.labels = c("STEM Background", "Tenure Moderation Effect", "Woman"),
  dep.var.labels = c("Hospitalizations", "Deaths", "NFI"),
  out = paste(output_dir, "/tables/moderation_tenure.tex", sep = ""),
  title = "Moderating effects of scientific intensity on the impact of STEM background",
  omit = c("X", "T_X", "covsZ2", "covsZ3", "covsZ4"),
  notes = NULL
)

writeLines(moderation_tenure, con = "outputs/tables/moderation_tenure.md")

out4 <- plm(
  Y_hosp ~ X + T + T_X + receita_2015 + inter_receita_stem + covsZ ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)
out5 <- plm(
  Y_deaths_sivep ~ X + T + T_X + receita_2015  + inter_receita_stem + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)
out6 <- plm(
  total_nfi ~ X + T + T_X + receita_2015 + inter_receita_stem + covsZ,
  data = pdata,
  index = c("sigla_uf","coorte"),
  model = "within"
)

moderation_revenue <- stargazer::stargazer(
  out5,
  out4,
  out6,
  type = "latex",
  covariate.labels = c(
    "STEM Background",
    "2015 Revenue",
    "Revenue Modereration Effect",
    "Woman"
  ),
  dep.var.labels = c("Hospitalizations", "Deaths", "NFI"),
  title = "Moderating effects of cities’ development on the impact of STEM background",
  out = paste(output_dir, "/tables/moderation_revenue.tex", sep = ""),
  omit = c("X", "T_X", "covsZ2", "covsZ3", "covsZ4"),
  notes = NULL
)

writeLines(moderation_revenue, con = "outputs/tables/moderation_revenue.md")
