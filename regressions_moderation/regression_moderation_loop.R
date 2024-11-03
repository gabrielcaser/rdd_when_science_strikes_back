#TODO 

## Fix characteristic (tenure) problem "NA" during many regressions


## Libraries
library(dplyr)
library(plm)
library(readxl)
library(stargazer)
library(skimr)
library(broom)
library(writexl)

# Initial commands
rm(list = ls(all.names = TRUE)) # clear objects
gc() # free up memory

# Setting
work_dir                       = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/regressions_moderation"
output_dir                     = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/regressions_moderation/output"
create_dataset_for_regressions = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/output"

# Define the datasets to be used in the loop
datasets <- c("strictdefinition", "broaddefinition")

# Define the victory margin windows
victory_margins <- seq(0.02, 1.00, by = 0.02)

# Define the dependent variables to be used in the loop
dependent_vars <- c("Y_deaths_sivep", "Y_hosp", "total_nfi")

# Define the values for sch_non_stem_cdt
sch_non_stem_cdt_values <- c("Yes", "No")

# Define the controls for mayor and municipality
controls_mayor_yes <- " + instrucao + mulher + ideology_party + idade + reeleito"
controls_mayor_no <- ""
controls_municipality_yes <- " + densidade + idhm + renda_pc + per_populacao_urbana + per_populacao_homens + tx_med_ch + pct_desp_recp_saude_mun + tx_leito_sus + cob_esf + ideology_municipality"
controls_municipality_no <- ""

# Define the interaction terms and corresponding main control terms
interactions <- list(
  "Municipality Revenue Moderation" = list("interaction" = "inter_receita_stem", "main_control" = "receita_2015"),
  "Tenure Moderation" = list("interaction" = "inter_tenure_stem", "main_control" = "tenure")
)

# Create an empty dataframe to store results
consolidated_results <- data.frame()

# Define the years to be used in the loop
year_groups <- list(
  "2016" = c(2016),
  "2020" = c(2020),
  "both" = c(2016, 2020)
)

for (dataset in datasets) {
  # Open the dataset
  df <- readRDS(paste(create_dataset_for_regressions, "/data/rdd_data_moderation_", dataset, ".rds", sep = ""))
  
  # Merge with Revenue data
  data_revenue <- read_excel(paste(work_dir, "/input/ipea_receitas_municipais.xlsx", sep = ""), sheet = "Receitas Totais")
  
  data_revenue <- data_revenue %>% 
    reframe(id_municipio = Cod.IBGE,
            receita_2015 = `2015` / 10000000)
  
  df <- merge(df, data_revenue, by = "id_municipio")
  rm(data_revenue)
  
  df$tenure <- log((df$tenure / 12) + 1)
  
  for (year_group in names(year_groups)) {
    years <- year_groups[[year_group]]
    df_year <- df %>% filter(coorte %in% years)
    
    for (margin in victory_margins) {
      for (dep_var in dependent_vars) {
        for (sch_value in sch_non_stem_cdt_values) {
          for (control_mayor in c("Yes", "No")) {
            for (control_municipality in c("Yes", "No")) {
              for (interaction_name in names(interactions)) {
                interaction <- interactions[[interaction_name]]$interaction
                main_control <- interactions[[interaction_name]]$main_control
                
                df_subset <- df_year[df_year$X >= -margin & df_year$X <= margin, ]
                if (sch_value == "Yes") {
                  df_subset <- df_subset[df_subset$sch_non_stem_cdt == 1, ]
                }
                df_subset$victory_margin_window <- margin * 100
                df_subset$dependent_var <- dep_var
                df_subset$sch_non_stem_cdt_value <- sch_value
                df_subset$control_mayor <- control_mayor
                df_subset$control_municipality <- control_municipality
                df_subset$year_group <- year_group
              
                
                # Creating running variable (T_X) and Treatment variable (T)
                df_subset$T <- ifelse(df_subset$X >= 0, 1, 0)
                df_subset$T_X <- df_subset$X * df_subset$T
                
                df_subset$inter_receita_stem <- df_subset$receita_2015 * df_subset$T
                df_subset$inter_tenure_stem  <- df_subset$tenure * df_subset$T

                df_subset$interaction  <- df_subset[[interaction]]
                df_subset$main_control <- df_subset[[main_control]]
                
                
                
                # Final table
                formula_controls_mayor <- if (control_mayor == "Yes") controls_mayor_yes else controls_mayor_no
                formula_controls_municipality <- if (control_municipality == "Yes") controls_municipality_yes else controls_municipality_no
                formula_controls_coorte <- if (year_group == "both") " + coorte" else ""
                formula <- as.formula(paste(dep_var, "~ X + T + T_X + interaction + main_control", formula_controls_mayor, formula_controls_municipality, formula_controls_coorte))
                
                out <- lm(formula, data = df_subset)
                summary(out)
                

                # Extract relevant statistics
                results <- tidy(out)
                model_info <- glance(out)
                relevant_vars <- results %>% filter(term %in% c("T", "interaction", "main_control"))
                
                # Create a summary dataframe
                summary_df <- relevant_vars %>%
                  select(term, estimate, std.error, statistic, p.value) %>%
                  mutate(lower_ci = estimate - 1.96 * std.error,
                         upper_ci = estimate + 1.96 * std.error,
                         stem_definition = dataset,
                         n_obs = model_info$nobs,
                         victory_margin_window = margin * 100,
                         dependent_var = dep_var,
                         sch_non_stem_cdt_value = sch_value,
                         control_mayor = control_mayor,
                         control_municipality = control_municipality,
                         year_group = year_group,
                         interaction_term = interaction_name) %>%
                  rename(impact = estimate,
                         std_error = std.error,
                         t_statistic = statistic,
                         p_value = p.value)
                
                # Append results to the consolidated dataframe
                consolidated_results <- rbind(consolidated_results, summary_df)
              }
            }
          }
        }
      }
    }
  }
}

# Save the consolidated results to an Excel file
write_xlsx(consolidated_results, path = paste(output_dir, "/tables/results.xlsx", sep = ""))
