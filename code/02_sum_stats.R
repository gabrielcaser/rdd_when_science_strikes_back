

# This program creates sum stats


# Oppening ----------------------------------------------------------------

df <- readRDS(paste(data_dir, "/final/", data, sep = ""))

df <- df %>% # Removing NPI data from municipalities in 2020 chort (since this data only regards mayors elected in 2016)
  mutate(
    total_nfi = ifelse(coorte == 2020, NA, total_nfi),
    barreiras_sanitarias = ifelse(coorte == 2020, NA, barreiras_sanitarias),
    mascaras = ifelse(coorte == 2020, NA, mascaras),
    restricao_atv_nao_essenciais = ifelse(coorte == 2020, NA, restricao_atv_nao_essenciais),
    restricao_circulacao = ifelse(coorte == 2020, NA, restricao_circulacao),
    restricao_transporte_publico = ifelse(coorte == 2020, NA, restricao_transporte_publico)
  )

# Sum stats ---------------------------------------------------------------



dat <- df[c(
  "stem_background",
  # keeping only relevant variables
  "tenure",
  "X",
  "mulher",
  "idade",
  "instrucao",
  "reeleito",
  "ideology_party",
  "populacao",
  "densidade",
  #"taxa_analfabetismo_18_mais",
  #"indice_gini",
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
  #"Y_cases",
  "barreiras_sanitarias",
  "mascaras",
  "restricao_atv_nao_essenciais",
  "restricao_circulacao",
  "restricao_transporte_publico",
  "total_nfi"
)]

dat <- dat %>%
  summarise(
    "Tenure in STEM job" = tenure,
    "Female" = as.numeric(mulher),
    "Age" = idade,
    "Education" = instrucao,
    "Incumbent when elected" = as.numeric(reeleito),
    "Party ideology" = ideology_party,
    "Deaths per 100k inhabitants" = Y_deaths_sivep,
    "Hospitalizations per 100k inhabitants" = Y_hosp,
    "Cordon sanitaire" = barreiras_sanitarias,
    "Face covering required" = mascaras,
    "Closure of non-essential activities" = restricao_atv_nao_essenciais,
    "Gathering prohibition" = restricao_circulacao,
    "Public transport restriction" = restricao_transporte_publico,
    "Number of Non-Pharma. Interventions" = total_nfi,
    "Log of population in 2010" = log(populacao),
    "Human Development Index" = idhm,
    "Per capita income" = renda_pc,
    "Population density" = densidade,
    "Urban population rate" = per_populacao_urbana,
    "Men population rate" = per_populacao_homens,
    "Physicians per 1k inhabitants" = tx_med,
    "Health municipal spending rate" = pct_desp_recp_saude_mun,
    "Community health agency coverage rate" = cob_esf,
    "Hospital beds per 100k population" = tx_leito_sus,
    stem_background
  )




## creating table without groups

tab <- datasummary(All(data.frame(dat)) ~ N  + Min + Mean + Max + SD,
                   data = dat,
                   fmt = 2)
tab

datasummary(
  All(data.frame(dat)) ~ N  + Min + Mean + Max + SD,
  data = dat,
  fmt = 2,
  align = "lrrrrr",
  title = "Summary Statistics",
  output = paste0(output_dir, "/tables/table_sum_stats.md")
)


## creating table with groups

dat$stem_background <- ifelse(dat$stem_background == 1, "STEM", "Non-STEM")

datasummary_balance(
  ~ stem_background,
  dinm_statistic = "p.value",
  data = dat,
  fmt = 2,
  align = "lrrrrrr",
  title = "Summary Statistics by Group",
  output = paste0(output_dir, "/tables/table_sum_stats_groups.md")
)

## Figures for STEM candidates ---------------------------------------------------------

### Main occupations

profi <- df %>% 
  filter(stem_background == 1) %>%
  group_by(cbo_agregado_nome_caser.laverde.rothwell) %>%
  summarise(number = n(), .groups = "drop") %>%
  mutate(percentage = 100 * number / sum(number)) %>%
  slice_max(order_by = percentage, n = 10) %>%
  arrange(desc(percentage))


library(forcats)

# Ordering
profi <- profi %>%
  mutate(cbo_agregado_nome_caser.laverde.rothwell = fct_reorder(
    cbo_agregado_nome_caser.laverde.rothwell, percentage, .desc = FALSE
  ))

# Creating the plot
p <- ggplot(profi, aes(y = cbo_agregado_nome_caser.laverde.rothwell, x = percentage)) +
  geom_bar(pattern = "occupation", stat = "identity") +
  geom_text(aes(label = sprintf("%.0f%%", percentage)), 
            hjust = -0.1, # Posiciona os rótulos fora das barras
            size = 5) +   # Tamanho do texto
  theme_minimal(base_size = 16) +
  xlab("") +  # Remove o rótulo do eixo X
  ylab("Occupations") +
  xlim(0, 60) + 
  #ggtitle("STEM mayors’ most common occupations") +
  scale_fill_discrete(name = "occupation") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),  # Remove os valores do eixo X
    axis.ticks.x = element_blank(),  # Remove as marcas do eixo X
    axis.title = element_blank() 
  )

p

ggsave(
  filename = paste0(output_dir, "/figures/barplot_stem_cbos_stem_ocupacao.png"),
  plot = p,  
  height = 4.0,
  width = 10,
  dpi = 600 
)



### getting all Brazilian municipalities

df_cities <- read.csv("data/raw/cities_and_states.csv")
df_cities$id_municipio <- as.character(df_cities$id_municipio)
#df_cities$coorte <- as.factor(2016) 

df_boxplots <- merge(df_cities, df, by = c("id_municipio", "sigla_uf"), all = TRUE) # creating a dataset with all municipalities


### ploting

states <- df_boxplots %>%
  #filter(coorte == 2016) %>% 
  group_by(sigla_uf) %>% 
  dplyr::summarise(perc_stem = sum(stem_background == 1, na.rm = TRUE) / length(id_municipio)) %>% 
  arrange(desc(perc_stem))

box2 <- ggplot(states, aes(y = perc_stem * 100)) + 
  geom_boxplot() + 
  theme_minimal(base_size = 16) + 
  #xlab("cohort") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x =  element_blank()
    ) #+
  #ylab("% of cities with a STEM candidate among top 2 voted ")

box2 

ggsave(
  filename = paste0(output_dir, "/figures/sumstats_boxplot.png"),
  plot = box2,  # Replace with the actual ggplot object
  height = 3.0,
  width = 2.5,
  dpi = 600
)



# Creating map  ---------------------------------------------------------

## states
#utils::remove.packages('geobr')
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

dados_mapa <- read_state(year=2015, showProgress = FALSE, simplified = FALSE)

#dados_mapa <- readRDS("Dados/input/220811_geo_dados_estados_2019.rds")


sf2 <- states 


#sf2 <- sf2 %>% 
 # rename(sigla_uf = state)

dados_mapa <- dados_mapa %>% 
  rename(sigla_uf = abbrev_state)


sf2 <- left_join(dados_mapa, sf2, by = c("sigla_uf"))
dim(df)

sf2$perc_stem = sf2$perc_stem * 100


sf2 %>%
  #filter(sigla_uf != "AC") %>% 
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

ggsave(
  filename = paste0(output_dir, "/figures/mapa_stem_estados.png"),
 # plot = box2,  # Replace with the actual ggplot object
  height = 5.0,
  width = 10
)



## municipios

mun <- read_municipality(year=2015, showProgress = FALSE, simplified = FALSE)

#mun <- readRDS("Dados/input/220811_geo_dados_municipios_2019.rds")

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
  geom_sf(aes(fill = stem_background), alpha = .7, color = NA) +
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


ggsave(
  filename = paste0(output_dir, "/figures/mapa_stem_municipios_2016.png"),
  # plot = box2,  # Replace with the actual ggplot object
  height = 5.0,
  width = 10,
  dpi = 600
)


#sf3 %>%
#  filter(code_state == 35) %>%  # para ver o mapa inteiro é só tirar o code_region
#  ggplot() +
#  geom_sf(data = subset(dados_mapa, code_state == 35)) +
#  geom_sf(aes(fill = stem_background), alpha = .7, color = NA) +
#  labs(#title="Municipalities where a STEM canditate was among the top 2 voted in SP (2016)",
#    caption='Source: Author', size = 8) +
#  scale_fill_manual(values = c("red", "blue"),
#                    name = "STEM candidate",
#                    na.value = "grey90",
#                    labels = c("Lost","Won", "Not in top 2")) +
#  theme_minimal(base_size = 16) + 
#  theme(axis.title = element_blank(),
#        axis.text = element_blank(),
#        axis.ticks = element_blank(),
#        panel.grid = element_blank())
#
#ggsave(
#  filename = paste0(output_dir, "/figures/mapa_stem_SP.png"),
#  # plot = box2,  # Replace with the actual ggplot object
#  height = 5.5,
#  width = 7.5
#)

# Plots - Discontinuity  ----------------------------------------------

## outcomes

### running regressions 

amostra <- cbind()

state.f = factor(df$sigla_uf)
state.d = model.matrix( ~ state.f + 0) # creating fixed effects

year.f = factor(df$coorte) # creating dummies
year.d = model.matrix( ~ year.f + 0)

covsZ  = cbind(state.d, year.d) # Controls

r4 = rdrobust(
  df$Y_deaths_sivep,
  df$X,
  p      = poli,
  kernel = k,
  h      = janela,
  subset = amostra,
  #covs   = covsZ
)
r5 = rdrobust(
  df$Y_hosp,
  df$X,
  kernel = k,
  h      = janela,
  p      = poli,
  subset = amostra,
  #covs   = covsZ
)

summary(r4)
summary(r5)

### hosp 

df_plots <- df[abs(df$X) < r4$bws[1], ]

#poli = 1
#covsZ = cbind(state.d, df$mulher)
#covsZ = df$tenure


hosp <- rdplot(df_plots$Y_hosp, df_plots$X,
               #covs = covsZ,
               p = poli,
               #x.lim = c(r4$bws[1] * -1, r4$bws[1]),
               #y.lim = c(75, 175),
               #shade = TRUE,
               #subset = amostra,
               #h = r4$bws[1],
               #scale = 5,
               #ci = 95,
               binselect = "esmv",
               kernel = k,
               x.label = "STEM candidate's margin of victory",
               y.label = "",
               title = "(A) Impact of Treatment on Hospitalizations"
               )


### deaths


death <- rdplot(df_plots$Y_deaths_sivep , df_plots$X,
                #covs = covsZ,
                p = poli,
                #x.lim = c(r5$bws[1] * -1, r5$bws[1]),
                y.lim = c(75, 175),
                #shade = TRUE,
                #h = r5$bws[1],
                #scale = 5,
                #ci = 90,
                binselect = "esmv",
                kernel = k,
                x.label = "STEM candidate's margin of victory",
                y.label = "",
                title = "(B) Impact of Treatment on Deaths"#,
               # col.lines = "white"
               )

death <- death$rdplot

death <- death +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 10, face = "plain"),
    title = element_text(size = 12),
    panel.grid = element_blank()
  )

death


hosp <- hosp$rdplot

hosp <- hosp +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    axis.title = element_text(size = 10, face = "plain"),
    title = element_text(size = 12),
    panel.grid = element_blank()
  )

hosp

plots <- hosp / death

plots

ggsave(paste0(output_dir, "/figures/bigsample_plots_outcomes.png"), plot = plots,
       width = 8.0,
       height = 5,
       units = "in")



ggplot(df[abs(df$X) <= 0.05, ], aes(x = X, y = Y_deaths_sivep)) +
  geom_point()

ggplot(df[abs(df$X) <= 0.05, ], aes(x = X, y = Y_hosp)) +
  geom_point()
