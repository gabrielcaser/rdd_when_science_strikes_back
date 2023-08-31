# Program - This program uses machine learning to classify mayors educational background as STEM or Non-STEM

# TO DO -------------------------------------------------------------------


# Libs --------------------------------------------------------------------

library(skimr)
library(pROC)
library(tree)
library(randomForest)
library(fastAdaboost)
library(naniar)
library(fastDummies)
library(tidyverse)
library(janitor)

## Directories

work_dir                  = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/3_create_education_data"
output_dir                = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/3_create_education_data/output"
create_electoral_data_dir = "C:/Users/gabri/OneDrive/Gabriel/Insper/Tese/Engenheiros/replication_code/rdd_when_science_strikes_back/1_create_electoral_data/output/data"

set.seed(1234) # making it reproducible

# Data --------------------------------------------------------------------

df <- readRDS(paste0(output_dir, "/data/230829_masked_ml_dataset_stem.Rds"))
df_nonstem <- readRDS(paste0(output_dir, "/data/230829_masked_ml_dataset_non_stem.Rds"))

# Removing cbo_agregado_ dummies

## Create a vector of variable names to remove
variables_to_remove <- grep("cbo_agregado_", names(df), value = TRUE)

## Remove the selected variables from the data frame
df <- df[, !(names(df) %in% variables_to_remove)]

## Creating aggregated data

df <- df %>% 
  mutate(cbo_2_digits = as.factor(substring(cbo_2002, 0,2)))

## Creating dummy columns for categorical variables (useful for running the algorithms)
df <- dummy_cols(df, select_columns = c("raca"))


## Removing candidates with missing educational background and creating copy with all candidates

df_all_candidates <- df

df <- df %>% 
 filter(!is.na(graduacao_stem))

## Fixing datatype

df$id_municipio <- as.factor(df$id_municipio)

# Creating Train / Test datasets ------------------------------------------------------

idx <- sample(1:nrow(df), size = round(0.7 * nrow(df)))
training <- df[idx, ] # used to train the classifier
test <- df[-idx, ] # used to apply the classifier

# Variables to not use

excluded_variables <- c("id_masked",
                        "nome",
                        "resultado",
                        "ano",
                        "cbo_2002",
                        "id_municipio",
                        "instrucao",
                        "ocupacao",
                        "sigla_partido",
                        "state",
                        "cbo_agregado",
                        "cbo_2002",
                        "raca",
                        "cbo_2_digits",
                        "graduacao",
                        'X2_lugar',
                        'X3_lugar',
                        'n_stem_candidates',
                        'stem_job',
                        'dif_votos_3_lugar',
                        'tenure')

# Logistic Regression -----------------------------------------------------

logreg <- glm(as.numeric(graduacao_stem) ~ . , data = training[,!(names(training)) %in% excluded_variables],  na.action = na.exclude) # It excludes the mentioned columns 

summary(logreg)

prob_logreg <- predict(logreg, newdata = test[,!(names(test)) %in% excluded_variables], type = "response") # it now uses the trained model 'logreg' to estimate the values in the test dataset

## optimal threshold

plot.roc(test$graduacao_stem, prob_logreg,
         print.thres = "best")

my_roc <- roc(test$graduacao_stem, prob_logreg)
best_threshold <- as.numeric(coords(my_roc,"best", ret = "threshold" ))

y_hat_logreg <- factor(ifelse(prob_logreg >= best_threshold, "Yes", "No"))
acc_logreg <- mean(as.numeric(y_hat_logreg) == as.numeric(test$graduacao_stem), na.rm = TRUE)
auc_logreg <- auc(test$graduacao_stem, prob_logreg)[1]


## optimal confusion table

table(Predicted = y_hat_logreg, Observed = test$graduacao_stem)


# Classification Tree -----------------------------------------------------


ctree <- tree(graduacao_stem ~ ., data = training[, !(names(training)) %in% excluded_variables], na.action = na.exclude)
plot(ctree)
text(ctree)
prob_ctree <- predict(ctree, newdata = test, type = "vector")[, 2]


## best threshold 
plot.roc(test$graduacao_stem, prob_ctree,
         print.thres = "best")

my_roc <- roc(test$graduacao_stem, prob_ctree)
best_threshold <- as.numeric(coords(my_roc,"best", ret = "threshold" ))

y_hat_ctree <- factor(ifelse(prob_ctree >= best_threshold, "Yes", "No"))
acc_ctree <- mean(as.numeric(y_hat_ctree) == as.numeric(test$graduacao_stem), na.rm = TRUE)
auc_ctree <- auc(test$graduacao_stem, prob_ctree, na.rm = TRUE )[1]



## best confusion table

table(Predicted = y_hat_ctree, Observed = test$graduacao_stem)

# Random Forest -----------------------------------------------------------



rf <- randomForest(graduacao_stem ~ ., data = training[, !(names(training)) %in% excluded_variables],  na.action = na.exclude)
prob_rf <- predict(rf, newdata = test[, !(names(test)) %in% excluded_variables], type = "prob")[, 2] # [. 2] is getting only the probability of being treated

## optimal threshold
plot.roc(test$graduacao_stem, prob_rf,
         print.thres = "best")

my_roc <- roc(test$graduacao_stem, prob_rf)
best_threshold_rf <- as.numeric(coords(my_roc,"best", ret = "threshold" ))

y_hat_rf <- factor(ifelse(prob_rf >= best_threshold_rf, "Yes", "No"))
acc_rf <- mean(as.numeric(y_hat_rf) == (as.numeric(test$graduacao_stem)), na.rm = TRUE)
auc_rf <- auc(test$graduacao_stem, prob_rf)[1]

## optimal confusion table

table(Predicted = y_hat_rf, Observed = test$graduacao_stem)

# Boosting ----------------------------------------------------------------

boosting <- adaboost(graduacao_stem ~ ., data = training[, !(names(training)) %in% excluded_variables], nIter = 200,  na.action = na.exclude)
prob_boosting <- predict(boosting, newdata = test[, !(names(test)) %in% excluded_variables])$prob[, 2]

## optimal threshold 
plot.roc(test$graduacao_stem, prob_boosting,
                  print.thres = "best")

my_roc <- roc(test$graduacao_stem, prob_boosting)
best_threshold_boosting <- as.numeric(coords(my_roc,"best", ret = "threshold" ))


y_hat_boosting <- factor(ifelse(prob_boosting >= best_threshold_boosting, "Yes", "No"))
acc_boosting <- mean(as.numeric(y_hat_boosting) == as.numeric(test$graduacao_stem), na.rm = TRUE)
auc_boosting <- auc(test$graduacao_stem, prob_boosting)[1]

## optimal confusion table

table(Predicted = y_hat_boosting, Observed = test$graduacao_stem)

# Curva ROC ---------------------------------------------------------------

plot.roc(test$graduacao_stem, prob_ctree,
         col = 1, grid = TRUE,
         xlab = "FPR (1 -  Specificity)",
         ylab = "TPR (Sensitivity)",
         main = "ROC", legacy.axes = TRUE,
         asp = FALSE, las = 1)
plot.roc(test$graduacao_stem, prob_rf, col = 2, add = TRUE)
plot.roc(test$graduacao_stem, prob_boosting, col = 3, add = TRUE)
plot.roc(test$graduacao_stem, prob_logreg, col = 4, add = TRUE)
legend("bottomright",
       legend = c("Tree", "Random Forest", "Boosting", "Log Reg"),
       col = c(1, 2, 3, 4), lwd = 2, cex = 0.6, pch = 10)


# Using best model to predict education of candidates that we didn't find educational data ----------------------------------------------

## add candidates from 2016

prob_final <- predict(boosting, newdata = df_all_candidates[, !(names(df_all_candidates)) %in% excluded_variables])$prob[, 2]

y_hat_final <- factor(ifelse(prob_final >= best_threshold_boosting, "Yes", "No"))

df_all_candidates$previsao <- factor(ifelse(prob_final >= best_threshold_boosting, "Yes", "No"))

table(Predicted = y_hat_final, Observed = df_all_candidates$graduacao_stem)

df <- df_all_candidates


# Sums stats
df %>%
  filter(ocupacao == "engenheiro") %>% 
  summarise(id_masked, ocupacao,graduacao_stem, previsao, state) %>% 
  arrange(ocupacao,graduacao_stem, previsao)
  

## crating final educational variable

df$curso_stem <- ifelse(is.na(df$graduacao_stem), as.numeric(df$previsao) - 1, as.numeric(df$graduacao_stem) - 1) # maintaining the values that I classified by hand, and complementing the missing values of education with model prediction

# Dropping variables

rm(boosting, ctree, df_all_candidates, logreg, my_roc, rf, test, training)

df <- df %>% 
  summarise(coorte = ano,
            id_masked,
            state,
            id_municipio,
            tenure,
            tenure_rais,
            hours,
            stem_job,
            cbo_2002,
            cbo_agregado,
            curso_stem,
            resultado,
            sigla_partido,
            instrucao,
            ocupacao,
            genero,
            raca,
            idade,
            dif_votos_2_lugar,
            dif_votos_3_lugar,
            x2_lugar,
            x3_lugar)

# Cases where a candidate had different predicted STEM educational in different cohorts

df <- df %>% 
  group_by(id_masked) %>% 
  mutate(curso_stem = max(curso_stem)) %>% # attributing 1 for Stem-education if a candidate at least received 1 in one cohort
  ungroup()

# Merging with non-stem dataset

df_nonstem <- df_nonstem %>% 
  summarise(coorte = ano,
            id_masked,
            state,
            id_municipio,
            tenure,
            tenure_rais,
            hours,
            stem_job,
            cbo_2002,
            cbo_agregado,
            curso_stem = NA,
            resultado,
            sigla_partido,
            instrucao,
            ocupacao,
            genero,
            raca,
            idade,
            dif_votos_2_lugar,
            dif_votos_3_lugar,
            x2_lugar,
            x3_lugar)

df <- rbind(df, df_nonstem)

# Saving ------------------------------------------------------------------

saveRDS(df, file = paste(output_dir,"/data/candidates_dataset.Rds", sep = ""))

