# This program runs all possible models and saves them in a single Excel file

# Settings

## Libraries
library(dplyr)
library(plm)
library(readxl)
library(stargazer)
library(skimr)
library(writexl)
library(rdrobust)

# Initial commands
rm(list = ls(all.names = TRUE)) # clear objects
gc() # free up memory

# Setting
work_dir <- "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/regressions_all"
output_dir <- "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/regressions_all/output"
create_dataset_for_regressions <- "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/6_create_rdd_dataset/output"

# Create an empty list to store all regression summaries and controls_ols
regression_summaries <- list()
controls_ols <- c("")
controls_rdd <- cbind()
Model <- 0  # Initialize the Model counter

for (regression_type in c("OLS","RDD")) {
  
  for (kernel_type in c("triangular", "uniform")) {
    
    #for (window in c(NULL, 0.03, 0.05, 0.07, 0.09, 0.11, 0.13, 0.15)) {
      
      for (coef_type in c("Robust", "Conventional")) {
        
        for (stem_definition in c("strict", "broad")) {
          
          for (non_stem_education in c("higher education", "all")) {
            
            for (cohort in c("2016", "2020", "all")) {
              
              for (time_fe in c("yes", "no")){
                
                for (state_fe in c("yes", "no")) {
                  
                  for (mayor_characteristics in c("yes", "no")) {
                    
                    for(mun_characteristics in c("yes","no")) {
                      
                      Model <- Model + 1  # Increment the Model counter for each y_var
                      
                      for (y_var in c("Y_hosp", "Y_deaths_sivep")) {
                        
                        # Oppening dataset
                        df <- readRDS(paste(create_dataset_for_regressions, "/data/rdd_data_moderation_", stem_definition, "definition.rds", sep = ""))
                        
                        # Creating log(hosp)
                        #df$log_Y_hosp = log(df$Y_hosp)
                        #df$log_Y_deaths_sivep = log(df$Y_deaths_sivep)
                        # Filtering
                        
                        if (cohort == "2016") {
                          df <- df %>% dplyr::filter(coorte == 2016)
                        }
                        else if (cohort == "2020") {
                          df <- df %>% dplyr::filter(coorte == 2020)
                        }
                        
                        if (non_stem_education == "higher education") {
                          df <- df %>% dplyr::filter(sch_non_stem_cdt == 1)
                        }
                        
                        # controls_ols
                        
                        if (time_fe == "yes" & cohort == "all") {
                          controls_ols <- c(controls_ols, "coorte")
                          
                          year.f = factor(df$coorte) # creating dummies
                          year.d = model.matrix(~year.f+0)
                          controls_rdd = cbind(controls_rdd, year.d)
                        }
                        
                        if (state_fe == "yes") {
                          controls_ols <- c(controls_ols, "sigla_uf")
                          
                          state.f = factor(df$sigla_uf) # creating dummies
                          state.d = model.matrix(~state.f+0)
                          controls_rdd = cbind(controls_rdd, state.d)
                        }
                        
                        if (mayor_characteristics == "yes") {
                          controls_ols <- c(controls_ols, "mulher", "idade", "reeleito", "ideology_party")
                          controls_rdd = cbind(controls_rdd, df$mulher, df$idade, df$reeleito, df$ideology_party)
                        }
                        
                        if (mun_characteristics == "yes") {
                          controls_ols <- c(controls_ols, "renda_pc", "populacao", "idhm", "densidade", "per_populacao_homens", "pct_desp_recp_saude_mun", "tx_med_ch", "cob_esf", "tx_leito_sus", "ideology_municipality")
                          controls_rdd = cbind(controls_rdd, df$renda_pc, log(df$populacao), df$idhm, log(df$densidade), df$per_populacao_homens, df$pct_desp_recp_saude_mun, df$tx_med_ch, df$cob_esf, df$tx_leito_sus, df$ideology_municipality)
                          
                        }
                        
                        if (regression_type == "OLS") {
                          controls_ols <- paste(controls_ols, collapse = " + ") # separating controls_ols by "+"
                        }
                        
                        
                        # Running regressions
                        if (regression_type == "OLS") {
                          
                          formula <- as.formula(paste(y_var, "~ X + T + T_X", controls_ols))
                          out <- lm(formula, data = df)
                          
                          # Extract coefficients, standard errors, t-values, p-values, and number of observations
                          coefficients <- coef(out)[["T"]]
                          standard_errors <- summary(out)$coefficients[, "Std. Error"][["T"]]
                          t_values <- coefficients / standard_errors
                          p_values <- summary(out)$coefficients[, "Pr(>|t|)"][["T"]]
                          number_obs <- length(out$model[, 1])
                          eff_number_obs <- NA
                        }
                        
                        else if (regression_type == "RDD") {
                          poli = 1
                          
                          out_rdd <- rdrobust(df[[y_var]],  df$X, p = poli, kernel = kernel_type, h = cbind(), bwselect = "mserd", covs = controls_rdd)
                          
                          # Extract coefficients, standard errors, t-values, p-values, and number of observations
                          
                          if (coef_type == "Robust") {
                            coef_n = 3
                          }
                          else if (coef_type == "Conventional") {
                            coef_n = 1
                          }
                          
                          coefficients <- out_rdd[["coef"]][coef_n] # 1 is for conventional and 3 is for robust
                          standard_errors <- out_rdd[["se"]][coef_n]
                          t_values <- coefficients / standard_errors
                          p_values <- out_rdd[["pv"]][coef_n]
                          number_obs <- out_rdd$N[1] + out_rdd$N[2]
                          eff_number_obs <- out_rdd$N_h[1] + out_rdd$N_h[2]
                          
                        }
                        
                        
                        # Combine all information into a data frame
                        regression_summary <- data.frame(
                          Model = Model,
                          Cohort = cohort,
                          Stem_definition = stem_definition,
                          Non_stem_HE = non_stem_education,
                          Regression_type = regression_type,
                          Coef_type = coef_type,
                          Window_range = "teste",
                          Kernel = kernel_type,
                          Y_var = y_var,
                          Number_obs = number_obs,
                          Eff.Number_obs = eff_number_obs,
                          Coefficient = coefficients,
                          Standard_Error = standard_errors,
                          T_Value = t_values,
                          P_Value = p_values,
                          State_FE = state_fe,
                          Cohort_FE = time_fe,
                          Mayors_vars = mayor_characteristics,
                          Munici_vars = mun_characteristics
                          
                        )
                        # Append the regression summary to the list
                        regression_summaries[[length(regression_summaries) + 1]] <- regression_summary
                        controls_ols <- c("")
                        controls_rdd <- cbind()
                        
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
#}




# Combine all regression summaries into a single data frame
regression_summary_all <- do.call(rbind, regression_summaries)

# Organize
regression_summary_all <- regression_summary_all %>%
  mutate(Window_range2 = case_when( Window_range == NA ~ "optimal",
                                   .default = as.character(Window_range))) %>% 
  arrange(Model)

# Filtering for redudant outcomes

#regression_summary_all <- subset(regression_summary_all, !(regression_type == "OLS" & Coef_type == "Robust"))

# Export to Excel
file_name <- paste(output_dir, "/regression_summary_all.xlsx", sep = "")
write_xlsx(regression_summary_all, file_name)

# Test
controls_ols <- c("")
controls_ols <- c(controls_ols, "coorte")
controls_ols <- c(controls_ols, "sigla_uf")
controls_ols <- paste(controls_ols, collapse = " + ")

formula <- as.formula(paste("Y_hosp", "~ X + T + T_X", controls_ols))
out <- lm(formula, data = df)

summary(out)