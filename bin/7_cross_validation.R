# CROSS VALIDATION
setwd("Macroevolutionary_Drivers")

library(dplyr)
library(cogmod)

# Function to calculate BIC 
aic_bic_function <- function(x, loglik_model, df_model) {
  loglik_model=x[1]
  df_model=x[2]
  log_sample <- log(122)# samples model 
  df_model_bic <- 2*loglik_model
  bic_model <- df_model*log_sample-(df_model_bic)
  
  aic_model <- 2*df_model - df_model_bic
  aic_bic <- c(bic_model, aic_model)
  names(aic_bic)<-c("bic_model", "aic_model")
  return(aic_bic )
}

# TEST SHIFT MODEL VS SINGLE RATE MODEL

# Set working directory to where the files are
path_files_null_to_sr <- "cross_validation/null_model/fit_null_to_sr/"

# List the 4 specific files
files_null_to_sr <- c("fit_null_to_sr_bv.csv", "fit_null_to_sr_lv.csv", "fit_null_to_sr_hv.csv", "fit_null_to_sr_sv.csv")
files_path_null_to_sr <-  paste0(path_files_null_to_sr, files_null_to_sr)

# Read and bind them into one data frame
data_null_to_sr <- do.call(rbind, lapply(files_path_null_to_sr, read.csv))
# View the combined data
head(data_null_to_sr)

# read csv and unique highest likelihood
fit_null_to_sr_df <- as.data.frame(data_null_to_sr)
fit_null_to_sr_decrease <- fit_null_to_sr_df[order(data_null_to_sr$id, decreasing = FALSE), ] 
nrow(fit_null_to_sr_decrease )

fit_null_to_sr_unique <- fit_null_to_sr_decrease %>%
  group_by(id) %>%
  slice_max(loglik, with_ties = FALSE) %>%
  ungroup()

# merge both models
fit_sr_sr_simulation <- read.csv("model_simulation/sr/shift_rate_03_eqe_simulations.csv")
fit_sr_sr_simulation_df <- as.data.frame(fit_sr_sr_simulation)
fit_null_to_sr_simulation <- rbind(fit_null_to_sr_unique[c("id", "loglik", "df" )], fit_sr_sr_simulation_df[c("id", "loglik", "df" )])
fit_null_to_sr_simulation_simulation_decrease <- fit_null_to_sr_simulation[order(fit_null_to_sr_simulation$id, decreasing = FALSE), ] 

# BIC TEST 
model_comparison_selection_null_to_sr <- c()
for (i in seq_along(1:1000)) { 
  comparison_model_null_to_sr <- fit_null_to_sr_simulation_simulation_decrease %>% filter(id == i)
  models_comparison_aic_bic_null_to_sr <- apply(comparison_model_null_to_sr[ , c(2,3)], 1, aic_bic_function)
  
  # Bic weights
  models_comparison_bic_weights_null_to_sr  <- BICw(models_comparison_aic_bic_null_to_sr [1,])
  models_comparison_bic_weights_null_to_sr 
  
  # paste results and save
  models_comparison_aic_bic_results_null_to_sr  <- cbind(comparison_model_null_to_sr , models_comparison_aic_bic_null_to_sr [2, ], models_comparison_aic_bic_null_to_sr [1, ], models_comparison_bic_weights_null_to_sr $BICdelta,  models_comparison_bic_weights_null_to_sr $BICweights)  
  colnames(models_comparison_aic_bic_results_null_to_sr ) <- c(names(comparison_model_null_to_sr ), "AIC", "BIC", "BICdelta", "BICweights") 
  models_comparison_aic_bic_results_null_to_sr 
  model_comparison_select_null_to_sr  <- which(models_comparison_aic_bic_results_null_to_sr $BICdelta==0.0)
  model_comparison_selection_null_to_sr  <- rbind(model_comparison_selection_null_to_sr , models_comparison_aic_bic_results_null_to_sr [model_comparison_select_null_to_sr , ])
}

model_comparison_selection_df_null_to_sr  <- as.data.frame(model_comparison_selection_null_to_sr )
single_best_model_null_to_sr  <- sum(model_comparison_selection_null_to_sr [ ,3] == "3")
single_best_model_null_to_sr 
# 0 times null best fit sr

shift_best_model_null_to_sr  <- sum(model_comparison_selection_null_to_sr [ ,3] == "5")
shift_best_model_null_to_sr 
# 1000 times sr bets fit sr

fit_SR_null_model_simulation_null_to_sr  <- cbind(fit_sr_sr_simulation_df, fit_null_to_sr_unique, model_comparison_selection_df_null_to_sr)
write.csv(fit_SR_null_model_simulation_null_to_sr, file="cross_validation/null_model/fit_null_to_sr_simulation.csv")

############################################
# TEST SHIFT MODEL VS SINGLE RATE MODEL for shift_eqe simulations
# Set working directory to where the files are (optional if you use full paths)
path_files_sr_to_null <- "cross_validation/null_model/fit_sr_to_null/"

# List the 4 specific files
files_sr_to_null <- c("fit_sr_to_null_simulations_bv.csv", "fit_sr_to_null_simulations_lv.csv", "fit_sr_to_null_simulations_hv.csv", "fit_sr_to_null_simulations_sv.csv")
files_path_sr_to_null <-  paste0(path_files_sr_to_null ,files_sr_to_null)

# Read and bind them into one data frame
data_sr_to_null <- do.call(rbind, lapply(files_path_sr_to_null, read.csv))
# View the combined data
head(data_sr_to_null)

# read csv and unique highest likelihood
fit_sr_to_null_df <- as.data.frame(data_sr_to_null)
fit_sr_to_null_decrease <- fit_sr_to_null_df[order(data_sr_to_null$id, decreasing = FALSE), ] 
nrow(fit_sr_to_null_decrease )

fit_sr_to_null_unique <- fit_sr_to_null_decrease %>%
  group_by(id) %>%
  slice_max(loglik, with_ties = FALSE) %>%
  ungroup()

# merge both models
fit_sr_sr_simulation <- read.csv("model_simulation/null_model/null_model_simulations.csv")
fit_sr_sr_simulation_df <- as.data.frame(fit_sr_sr_simulation)
fit_sr_to_null_simulation <- rbind(fit_sr_to_null_unique[c("id", "loglik", "df" )], fit_sr_sr_simulation_df[c("id", "loglik", "df" )])
fit_sr_to_null_simulation_simulation_decrease <- fit_sr_to_null_simulation[order(fit_sr_to_null_simulation$id, decreasing = FALSE), ] 

# BIC TEST 
model_comparison_selection_sr_to_null <- c()
for (i in seq_along(1:1000)) { 
  comparison_model_sr_to_null <- fit_sr_to_null_simulation_simulation_decrease %>% filter(id == i)
  models_comparison_aic_bic_sr_to_null <- apply(comparison_model_sr_to_null[ , c(2,3)], 1, aic_bic_function)
  
  # Bic weights
  models_comparison_bic_weights_sr_to_null  <- BICw(models_comparison_aic_bic_sr_to_null [1,])
  models_comparison_bic_weights_sr_to_null 
  
  # paste results and save
  models_comparison_aic_bic_results_sr_to_null  <- cbind(comparison_model_sr_to_null , models_comparison_aic_bic_sr_to_null [2, ], models_comparison_aic_bic_sr_to_null [1, ], models_comparison_bic_weights_sr_to_null $BICdelta,  models_comparison_bic_weights_sr_to_null $BICweights)  
  colnames(models_comparison_aic_bic_results_sr_to_null ) <- c(names(comparison_model_sr_to_null ), "AIC", "BIC", "BICdelta", "BICweights") 
  models_comparison_aic_bic_results_sr_to_null 
  model_comparison_select_sr_to_null  <- which(models_comparison_aic_bic_results_sr_to_null $BICdelta==0.0)
  model_comparison_selection_sr_to_null  <- rbind(model_comparison_selection_sr_to_null , models_comparison_aic_bic_results_sr_to_null [model_comparison_select_sr_to_null , ])
}

model_comparison_selection_df_sr_to_null  <- as.data.frame(model_comparison_selection_sr_to_null )
single_best_model_sr_to_null  <- sum(model_comparison_selection_sr_to_null [ ,3] == "3")
single_best_model_sr_to_null 
# 986 times null best fit null

shift_best_model_sr_to_null  <- sum(model_comparison_selection_sr_to_null [ ,3] == "6")
shift_best_model_sr_to_null 
# 14 times sr bets fit null

fit_SR_null_model_simulation_sr_to_null  <- cbind(fit_sr_sr_simulation_df, fit_sr_to_null_unique, model_comparison_selection_df_sr_to_null)
write.csv(fit_SR_null_model_simulation_sr_to_null, file="cross_validation/null_model/fit_sr_to_null_simulation.csv")

######################################################################################################################################################
# TEST SHIFT MODEL VS TT MODEL Dryforest
# Set working directory to where the files are (optional if you use full paths)
path_files_sr_to_tt_dry <- "cross_validation/tt_dry_forest/fit_sr_to_tt/"

# List the 4 specific files
files_sr_to_tt_dry <- c("fit_sr_to_ttsr_table_sv.csv", "fit_sr_to_ttsr_table_lv.csv", "fit_sr_to_ttsr_table_hv.csv", "fit_sr_to_ttsr_table_sv.csv")
files_path_sr_to_tt_dry <-  paste0(path_files_sr_to_tt_dry,files_sr_to_tt_dry)

# Read and bind them into one data frame
data_sr_to_tt_dry <- do.call(rbind, lapply(files_path_sr_to_tt_dry, read.csv))
# View the combined data
head(data_sr_to_tt_dry)

# read csv and unique highest likelihood
fit_sr_to_tt_dry_df <- as.data.frame(data_sr_to_tt_dry)
fit_sr_to_tt_dry_decrease <- fit_sr_to_tt_dry_df[order(data_sr_to_tt_dry$id, decreasing = FALSE), ] 
nrow(fit_sr_to_tt_dry_decrease )

fit_sr_to_tt_dry_unique <- fit_sr_to_tt_dry_decrease %>%
  group_by(id) %>%
  slice_max(loglik, with_ties = FALSE) %>%
  ungroup()

# merge both models
fit_tt_to_tt_dry_simulation <- read.csv("model_simulation/tt_dry_forest/tt_dryforest_simulations.csv")
fit_tt_to_tt_dry_simulation_df <- as.data.frame(fit_tt_to_tt_dry_simulation )
fit_sr_to_tt_dry_simulation <- rbind(fit_sr_to_tt_dry_unique[c("id", "loglik", "df" )], fit_tt_to_tt_dry_simulation_df[c("id", "loglik", "df" )])
fit_sr_to_tt_dry_simulation_simulation_decrease <- fit_sr_to_tt_dry_simulation[order(fit_sr_to_tt_dry_simulation$id, decreasing = FALSE), ] 

# BIC TEST 
model_comparison_selection_sr_to_tt_dry <- c()
for (i in seq_along(1:1000)) { 
  comparison_model_sr_to_tt_dry <- fit_sr_to_tt_dry_simulation_simulation_decrease %>% filter(id == i)
  models_comparison_aic_bic_sr_to_tt_dry <- apply(comparison_model_sr_to_tt_dry[ , c(2,3)], 1, aic_bic_function)
  
  # Bic weights
  models_comparison_bic_weights_sr_to_tt_dry  <- BICw(models_comparison_aic_bic_sr_to_tt_dry [1,])
  models_comparison_bic_weights_sr_to_tt_dry 
  
  # paste results and save
  models_comparison_aic_bic_results_sr_to_tt_dry  <- cbind(comparison_model_sr_to_tt_dry , models_comparison_aic_bic_sr_to_tt_dry [2, ], models_comparison_aic_bic_sr_to_tt_dry [1, ], models_comparison_bic_weights_sr_to_tt_dry $BICdelta,  models_comparison_bic_weights_sr_to_tt_dry $BICweights)  
  colnames(models_comparison_aic_bic_results_sr_to_tt_dry ) <- c(names(comparison_model_sr_to_tt_dry ), "AIC", "BIC", "BICdelta", "BICweights") 
  models_comparison_aic_bic_results_sr_to_tt_dry 
  model_comparison_select_sr_to_tt_dry  <- which(models_comparison_aic_bic_results_sr_to_tt_dry $BICdelta==0.0)
  model_comparison_selection_sr_to_tt_dry  <- rbind(model_comparison_selection_sr_to_tt_dry , models_comparison_aic_bic_results_sr_to_tt_dry [model_comparison_select_sr_to_tt_dry , ])
}

model_comparison_selection_df_sr_to_tt_dry  <- as.data.frame(model_comparison_selection_sr_to_tt_dry )
single_best_model_sr_to_tt_dry  <- sum(model_comparison_selection_sr_to_tt_dry [ ,3] == "4")
single_best_model_sr_to_tt_dry 
# 976 times sr best fit tt

shift_best_model_sr_to_tt_dry  <- sum(model_comparison_selection_sr_to_tt_dry [ ,3] == "6")
shift_best_model_sr_to_tt_dry 
# 24 times tt bets fit sr

fit_sr_to_tt_dry  <- cbind(fit_tt_to_tt_dry_simulation_df, fit_sr_to_tt_dry_unique, model_comparison_selection_df_sr_to_tt_dry)
write.csv(fit_sr_to_tt_dry , file="ross_validation/tt_dry_forest/fit_sr_to_tt_dry_simulation.csv")

################################################################
# TEST sr MODEL VS tt MODEL for shift_eqe simulations
path_files_tt_to_sr_dry <- "cross_validation/tt_dry_forest/fit_tt_to_sr/"

# List the 4 specific files
files_tt_to_sr_dry <- c("fit_tt_to_srtt_table_bv.csv", "fit_tt_to_srtt_table_lv.csv", "fit_tt_to_srtt_table_hv.csv", "fit_tt_to_srtt_table_sv.csv")
files_path_tt_to_sr_dry <-  paste0(path_files_tt_to_sr_dry,files_tt_to_sr_dry)

# Read and bind them into one data frame
data_tt_to_sr_dry <- do.call(rbind, lapply(files_path_tt_to_sr_dry, read.csv))
# View the combined data
head(data_tt_to_sr_dry)

# read csv and unique highest likelihood
fit_tt_to_sr_dry_df <- as.data.frame(data_tt_to_sr_dry)
fit_tt_to_sr_dry_decrease <- fit_tt_to_sr_dry_df[order(data_tt_to_sr_dry$id, decreasing = FALSE), ] 
nrow(fit_tt_to_sr_dry_decrease )

fit_tt_to_sr_dry_unique <- fit_tt_to_sr_dry_decrease %>%
  group_by(id) %>%
  slice_max(loglik, with_ties = FALSE) %>%
  ungroup()

# merge both models
fit_sr_to_sr_dry_simulation <- read.csv("model_simulation/sr/shift_rate_03_eqe_simulations.csv")
fit_sr_to_sr_dry_simulation_df <- as.data.frame(fit_sr_to_sr_dry_simulation )
fit_tt_to_sr_dry_simulation <- rbind(fit_tt_to_sr_dry_unique[c("id", "loglik", "df" )], fit_sr_to_sr_dry_simulation_df[c("id", "loglik", "df" )])
fit_tt_to_sr_dry_simulation_simulation_decrease <- fit_tt_to_sr_dry_simulation[order(fit_tt_to_sr_dry_simulation$id, decreasing = FALSE), ] 

# BIC TEST 
model_comparison_selection_tt_to_sr_dry <- c()
for (i in seq_along(1:1000)) { 
  comparison_model_tt_to_sr_dry <- fit_tt_to_sr_dry_simulation_simulation_decrease %>% filter(id == i)
  models_comparison_aic_bic_tt_to_sr_dry <- apply(comparison_model_tt_to_sr_dry[ , c(2,3)], 1, aic_bic_function)
  
  # Bic weights
  models_comparison_bic_weights_tt_to_sr_dry  <- BICw(models_comparison_aic_bic_tt_to_sr_dry [1,])
  models_comparison_bic_weights_tt_to_sr_dry 
  
  # paste results and save
  models_comparison_aic_bic_results_tt_to_sr_dry  <- cbind(comparison_model_tt_to_sr_dry , models_comparison_aic_bic_tt_to_sr_dry [2, ], models_comparison_aic_bic_tt_to_sr_dry [1, ], models_comparison_bic_weights_tt_to_sr_dry $BICdelta,  models_comparison_bic_weights_tt_to_sr_dry $BICweights)  
  colnames(models_comparison_aic_bic_results_tt_to_sr_dry ) <- c(names(comparison_model_tt_to_sr_dry ), "AIC", "BIC", "BICdelta", "BICweights") 
  models_comparison_aic_bic_results_tt_to_sr_dry 
  model_comparison_select_tt_to_sr_dry  <- which(models_comparison_aic_bic_results_tt_to_sr_dry $BICdelta==0.0)
  model_comparison_selection_tt_to_sr_dry  <- rbind(model_comparison_selection_tt_to_sr_dry , models_comparison_aic_bic_results_tt_to_sr_dry [model_comparison_select_tt_to_sr_dry , ])
}

model_comparison_selection_df_tt_to_sr_dry  <- as.data.frame(model_comparison_selection_tt_to_sr_dry )
single_best_model_tt_to_sr_dry  <- sum(model_comparison_selection_tt_to_sr_dry [ ,3] == "4")
single_best_model_tt_to_sr_dry 
# 0 times tt best fit sr

shift_best_model_tt_to_sr_dry  <- sum(model_comparison_selection_tt_to_sr_dry [ ,3] == "5")
shift_best_model_tt_to_sr_dry 
# 1000 times sr bets fit sr

fit_tt_to_sr_dry  <- cbind(fit_sr_to_sr_dry_simulation_df, fit_tt_to_sr_dry_unique, model_comparison_selection_df_tt_to_sr_dry)
write.csv(fit_tt_to_sr_dry , file="cross_validation/tt_dry_forest/fit_tt_to_sr_dry_simulation.csv")

#######################################################
# TEST SHIFT MODEL VS TT MODEL tyrannidae
# Set working directory to where the files are (optional if you use full paths)
path_files_sr_to_tt_tyr <- "cross_validation/tt_tyrannidae/fit_sr_to_tt/"

# List the 4 specific files
files_sr_to_tt_tyr <- c("fit_sr_to_ttsr_table_sv.csv", "fit_sr_to_ttsr_table_lv.csv", "fit_sr_to_ttsr_table_hv.csv", "fit_sr_to_ttsr_table_sv.csv")
files_path_sr_to_tt_tyr <-  paste0(path_files_sr_to_tt_tyr,files_sr_to_tt_tyr)

# Read and bind them into one data frame
data_sr_to_tt_tyr <- do.call(rbind, lapply(files_path_sr_to_tt_tyr, read.csv))
# View the combined data
head(data_sr_to_tt_tyr)

# read csv and unique highest likelihood
fit_sr_to_tt_tyr_df <- as.data.frame(data_sr_to_tt_tyr)
fit_sr_to_tt_tyr_decrease <- fit_sr_to_tt_tyr_df[order(data_sr_to_tt_tyr$id, decreasing = FALSE), ] 
nrow(fit_sr_to_tt_tyr_decrease )

fit_sr_to_tt_tyr_unique <- fit_sr_to_tt_tyr_decrease %>%
  group_by(id) %>%
  slice_max(loglik, with_ties = FALSE) %>%
  ungroup()

# merge both models
fit_tt_to_tt_tyr_simulation <- read.csv("model_simulation/tt_tyrannidae/tt_tyrannidae_simulations.csv")
fit_tt_to_tt_tyr_simulation_df <- as.data.frame(fit_tt_to_tt_tyr_simulation )
fit_sr_to_tt_tyr_simulation <- rbind(fit_sr_to_tt_tyr_unique[c("id", "loglik", "df" )], fit_tt_to_tt_tyr_simulation_df[c("id", "loglik", "df" )])
fit_sr_to_tt_tyr_simulation_simulation_decrease <- fit_sr_to_tt_tyr_simulation[order(fit_sr_to_tt_tyr_simulation$id, decreasing = FALSE), ] 

# BIC TEST 
model_comparison_selection_sr_to_tt_tyr <- c()
for (i in seq_along(1:1000)) { 
  comparison_model_sr_to_tt_tyr <- fit_sr_to_tt_tyr_simulation_simulation_decrease %>% filter(id == i)
  models_comparison_aic_bic_sr_to_tt_tyr <- apply(comparison_model_sr_to_tt_tyr[ , c(2,3)], 1, aic_bic_function)
  
  # Bic weights
  models_comparison_bic_weights_sr_to_tt_tyr  <- BICw(models_comparison_aic_bic_sr_to_tt_tyr [1,])
  models_comparison_bic_weights_sr_to_tt_tyr 
  
  # paste results and save
  models_comparison_aic_bic_results_sr_to_tt_tyr  <- cbind(comparison_model_sr_to_tt_tyr , models_comparison_aic_bic_sr_to_tt_tyr [2, ], models_comparison_aic_bic_sr_to_tt_tyr [1, ], models_comparison_bic_weights_sr_to_tt_tyr $BICdelta,  models_comparison_bic_weights_sr_to_tt_tyr $BICweights)  
  colnames(models_comparison_aic_bic_results_sr_to_tt_tyr ) <- c(names(comparison_model_sr_to_tt_tyr ), "AIC", "BIC", "BICdelta", "BICweights") 
  models_comparison_aic_bic_results_sr_to_tt_tyr 
  model_comparison_select_sr_to_tt_tyr  <- which(models_comparison_aic_bic_results_sr_to_tt_tyr $BICdelta==0.0)
  model_comparison_selection_sr_to_tt_tyr  <- rbind(model_comparison_selection_sr_to_tt_tyr , models_comparison_aic_bic_results_sr_to_tt_tyr [model_comparison_select_sr_to_tt_tyr , ])
}

model_comparison_selection_df_sr_to_tt_tyr  <- as.data.frame(model_comparison_selection_sr_to_tt_tyr )
single_best_model_sr_to_tt_tyr  <- sum(model_comparison_selection_sr_to_tt_tyr [ ,3] == "4")
single_best_model_sr_to_tt_tyr 
# 968 times tt best fit tt

shift_best_model_sr_to_tt_tyr  <- sum(model_comparison_selection_sr_to_tt_tyr [ ,3] == "6")
shift_best_model_sr_to_tt_tyr 
# 32 times sr best fit tt

fit_sr_to_tt_tyr  <- cbind(fit_tt_to_tt_tyr_simulation_df, fit_sr_to_tt_tyr_unique, model_comparison_selection_df_sr_to_tt_tyr)
write.csv(fit_sr_to_tt_tyr , file="cross_validation/tt_tyrannidae/fit_sr_to_tt_tyrannidae_simulation.csv")

####################################################################
# TEST SHIFT MODEL VS TT MODEL tyrannidae
# Set working directory to where the files are (optional if you use full paths)
path_files_tt_to_sr_tyr <- "cross_validation/tt_tyrannidae/fit_tt_to_sr/"

# List the 4 specific files
files_tt_to_sr_tyr <- c("fit_tt_to_srtt_sv.csv", "fit_tt_to_srtt_lv.csv", "fit_tt_to_srtt_hv.csv", "fit_tt_to_srtt_sv.csv")
files_path_tt_to_sr_tyr <-  paste0(path_files_tt_to_sr_tyr,files_tt_to_sr_tyr)

# Read and bind them into one data frame
data_tt_to_sr_tyr <- do.call(rbind, lapply(files_path_tt_to_sr_tyr, read.csv))
# View the combined data
head(data_tt_to_sr_tyr)

# read csv and unique highest likelihood
fit_tt_to_sr_tyr_df <- as.data.frame(data_tt_to_sr_tyr)
fit_tt_to_sr_tyr_decrease <- fit_tt_to_sr_tyr_df[order(data_tt_to_sr_tyr$id, decreasing = FALSE), ] 
nrow(fit_tt_to_sr_tyr_decrease )

fit_tt_to_sr_tyr_unique <- fit_tt_to_sr_tyr_decrease %>%
  group_by(id) %>%
  slice_max(loglik, with_ties = FALSE) %>%
  ungroup()

# merge both models
fit_tt_to_tt_tyr_simulation <- read.csv("model_simulation/sr/shift_rate_03_eqe_simulations.csv")
fit_tt_to_tt_tyr_simulation_df <- as.data.frame(fit_tt_to_tt_tyr_simulation )
fit_tt_to_sr_tyr_simulation <- rbind(fit_tt_to_sr_tyr_unique[c("id", "loglik", "df" )], fit_tt_to_tt_tyr_simulation_df[c("id", "loglik", "df" )])
fit_tt_to_sr_tyr_simulation_simulation_decrease <- fit_tt_to_sr_tyr_simulation[order(fit_tt_to_sr_tyr_simulation$id, decreasing = FALSE), ] 

# BIC TEST 
model_comparison_selection_tt_to_sr_tyr <- c()
for (i in seq_along(1:1000)) { 
  comparison_model_tt_to_sr_tyr <- fit_tt_to_sr_tyr_simulation_simulation_decrease %>% filter(id == i)
  models_comparison_aic_bic_tt_to_sr_tyr <- apply(comparison_model_tt_to_sr_tyr[ , c(2,3)], 1, aic_bic_function)
  
  # Bic weights
  models_comparison_bic_weights_tt_to_sr_tyr  <- BICw(models_comparison_aic_bic_tt_to_sr_tyr [1,])
  models_comparison_bic_weights_tt_to_sr_tyr 
  
  # paste results and save
  models_comparison_aic_bic_results_tt_to_sr_tyr  <- cbind(comparison_model_tt_to_sr_tyr , models_comparison_aic_bic_tt_to_sr_tyr [2, ], models_comparison_aic_bic_tt_to_sr_tyr [1, ], models_comparison_bic_weights_tt_to_sr_tyr $BICdelta,  models_comparison_bic_weights_tt_to_sr_tyr $BICweights)  
  colnames(models_comparison_aic_bic_results_tt_to_sr_tyr ) <- c(names(comparison_model_tt_to_sr_tyr ), "AIC", "BIC", "BICdelta", "BICweights") 
  models_comparison_aic_bic_results_tt_to_sr_tyr 
  model_comparison_select_tt_to_sr_tyr  <- which(models_comparison_aic_bic_results_tt_to_sr_tyr $BICdelta==0.0)
  model_comparison_selection_tt_to_sr_tyr  <- rbind(model_comparison_selection_tt_to_sr_tyr , models_comparison_aic_bic_results_tt_to_sr_tyr [model_comparison_select_tt_to_sr_tyr , ])
}

model_comparison_selection_df_tt_to_sr_tyr  <- as.data.frame(model_comparison_selection_tt_to_sr_tyr )
single_best_model_tt_to_sr_tyr  <- sum(model_comparison_selection_tt_to_sr_tyr [ ,3] == "4")
single_best_model_tt_to_sr_tyr 
# 0 times t best fit sr

shift_best_model_tt_to_sr_tyr  <- sum(model_comparison_selection_tt_to_sr_tyr [ ,3] == "5")
shift_best_model_tt_to_sr_tyr 
# 1000 times sr bets fit sr

fit_tt_to_sr_tyr  <- cbind(fit_tt_to_tt_tyr_simulation_df, fit_tt_to_sr_tyr_unique, model_comparison_selection_df_tt_to_sr_tyr)
write.csv(fit_tt_to_sr_tyr , file="cross_validation/tt_tyrannidae/fit_tt_to_sr_tyrannidae_simulation.csv")

########################################################################################################################
# TEST SHIFT MODEL VS rr MODEL

# read csv and unique highest likelihood

#FIT SR to rrSR
# Set working directory to where the files are (optional if you use full paths)
path_files_rr_to_sr <- "cross_validation/rr/fit_sr_to_rr/"

# List the 4 specific files
files_rr_to_sr <- c("fit_sr_to_rr_table_hv.csv", "fit_sr_to_rr_table_lv.csv", "fit_sr_to_rr_table_bv.csv", "fit_sr_to_rr_table_sv.csv")
files_path_rr_to_sr <-  paste0(path_files_rr_to_sr, files_rr_to_sr)

# Read and bind them into one data frame
data_rr_to_sr <- do.call(rbind, lapply(files_path_rr_to_sr, read.csv))
# View the combined data
head(data_rr_to_sr)
# read csv and unique highest likelihood
fit_rr_to_sr_df <- as.data.frame(data_rr_to_sr)
fit_rr_to_sr_decrease <- fit_rr_to_sr_df[order(data_rr_to_sr$id, decreasing = FALSE), ] 
nrow(fit_rr_to_sr_decrease )

fit_rr_to_sr_unique <- fit_rr_to_sr_decrease %>%
  group_by(id) %>%
  slice_max(loglik, with_ties = FALSE) %>%
  ungroup()
fit_rr_to_sr_unique <-as.data.frame (fit_rr_to_sr_unique)
head(fit_rr_to_sr_unique)

# merge both models
fit_sr_sr_simulation <- read.csv("model_simulation/rr/relaxed_rate_simulations_table.csv")
fit_sr_sr_simulation_df <- as.data.frame(fit_sr_sr_simulation)
fit_rr_to_sr_simulation <- rbind(fit_rr_to_sr_unique[c("id", "loglik", "df" )], fit_sr_sr_simulation_df[c("id", "loglik", "df" )])
fit_rr_to_sr_simulation_simulation_decrease <- fit_rr_to_sr_simulation[order(fit_rr_to_sr_simulation$id, decreasing = FALSE), ] 

#group by id remove if na
fit_rr_to_sr_simulation_simulation_decrease <- fit_rr_to_sr_simulation_simulation_decrease %>%
  group_by(id) %>%
  filter(
    !any(is.na(across(everything()))) & 
      n() > 1
  ) %>%
  ungroup()

fit_rr_to_sr_simulation_simulation_decrease  <- as.data.frame(fit_rr_to_sr_simulation_simulation_decrease )

# BIC TEST 

model_comparison_selection_rr_to_sr <- c()
unique_ids <- unique(fit_rr_to_sr_simulation_simulation_decrease $id)

for (i in unique_ids) { 
  comparison_model_rr_to_sr <- fit_rr_to_sr_simulation_simulation_decrease %>% filter(id == i)
  models_comparison_aic_bic_rr_to_sr <- apply(comparison_model_rr_to_sr[ , c(2,3)], 1, aic_bic_function)
  
  # Bic weights
  models_comparison_bic_weights_rr_to_sr  <- BICw(models_comparison_aic_bic_rr_to_sr [1,])
  models_comparison_bic_weights_rr_to_sr 
  
  # paste results and save
  models_comparison_aic_bic_results_rr_to_sr  <- cbind(comparison_model_rr_to_sr , models_comparison_aic_bic_rr_to_sr [2, ], models_comparison_aic_bic_rr_to_sr [1, ], models_comparison_bic_weights_rr_to_sr $BICdelta,  models_comparison_bic_weights_rr_to_sr $BICweights)  
  colnames(models_comparison_aic_bic_results_rr_to_sr ) <- c(names(comparison_model_rr_to_sr ), "AIC", "BIC", "BICdelta", "BICweights") 
  models_comparison_aic_bic_results_rr_to_sr 
  model_comparison_select_rr_to_sr  <- which(models_comparison_aic_bic_results_rr_to_sr $BICdelta==0.0)
  model_comparison_selection_rr_to_sr  <- rbind(model_comparison_selection_rr_to_sr , models_comparison_aic_bic_results_rr_to_sr [model_comparison_select_rr_to_sr , ])
}

model_comparison_selection_df_rr_to_sr  <- as.data.frame(model_comparison_selection_rr_to_sr )
single_best_model_rr_to_sr  <- sum(model_comparison_selection_rr_to_sr [ ,3] == "6")
single_best_model_rr_to_sr 
# 48 times sr fit rr

shift_best_model_rr_to_sr  <- sum(model_comparison_selection_rr_to_sr [ ,3] == "4")
shift_best_model_rr_to_sr 
# 950 times rr bets fit rr

# vector NA
vector_na <- which(is.na(fit_sr_sr_simulation_df$mu ))
# Create empty rows (with NA) and correct row names
na_rows <- data.frame(matrix(NA, nrow = length(vector_na), ncol = ncol(model_comparison_selection_df_rr_to_sr)))
colnames(na_rows) <- colnames(model_comparison_selection_df_rr_to_sr)
rownames(na_rows) <- as.character(vector_na)
na_rows$id <- vector_na
# Combine original and NA rows, then sort
df_full <- rbind(model_comparison_selection_df_rr_to_sr, na_rows)
df_full <- df_full[order(df_full$id), ]

fit_SR_null_model_simulation_rr_to_sr  <- cbind(fit_sr_sr_simulation_df, fit_rr_to_sr_unique, df_full)
write.csv(fit_SR_null_model_simulation_rr_to_sr, file="cross_validation/rr/fit_sr_to_rr_simulation.csv")

##################
#FIT SR to rrSR
# Set working directory to where the files are (optional if you use full paths)
path_files_rr_to_sr <- "cross_validation/rr/fit_rr_to_sr/"

# List the 4 specific files
files_rr_to_sr <- c("fit_rr_to_sr_table_bv.csv", "fit_rr_to_sr_table_lv.csv", "fit_rr_to_sr_table_hv.csv", "fit_rr_to_sr_table_sv.csv")
files_path_rr_to_sr <-  paste0(path_files_rr_to_sr, files_rr_to_sr)

# Read and bind them into one data frame
data_rr_to_sr <- do.call(rbind, lapply(files_path_rr_to_sr, read.csv))
# View the combined data
head(data_rr_to_sr)

# read csv and unique highest likelihood
fit_rr_to_sr_df <- as.data.frame(data_rr_to_sr)
fit_rr_to_sr_decrease <- fit_rr_to_sr_df[order(data_rr_to_sr$id, decreasing = FALSE), ] 
nrow(fit_rr_to_sr_decrease )

fit_rr_to_sr_unique <- fit_rr_to_sr_decrease %>%
  group_by(id) %>%
  slice_max(loglik, with_ties = FALSE) %>%
  ungroup()
fit_rr_to_sr_unique


# merge both models
fit_sr_sr_simulation <- read.csv("model_simulation/sr/shift_rate_03_eqe_simulations.csv")
fit_sr_sr_simulation_df <- as.data.frame(fit_sr_sr_simulation)
fit_rr_to_sr_simulation <- rbind(fit_rr_to_sr_unique[c("id", "loglik", "df" )], fit_sr_sr_simulation_df[c("id", "loglik", "df" )])
fit_rr_to_sr_simulation_simulation_decrease <- fit_rr_to_sr_simulation[order(fit_rr_to_sr_simulation$id, decreasing = FALSE), ] 

#remove na
which(is.na(fit_rr_to_sr_simulation_simulation_decrease))
fit_rr_to_sr_simulation_simulation_decrease<- na.omit(fit_rr_to_sr_simulation_simulation_decrease)

# BIC TEST 
library(cogmod)
model_comparison_selection_rr_to_sr <- c()
for (i in seq_along(1:1000)) { 
  comparison_model_rr_to_sr <- fit_rr_to_sr_simulation_simulation_decrease %>% filter(id == i)
  models_comparison_aic_bic_rr_to_sr <- apply(comparison_model_rr_to_sr[ , c(2,3)], 1, aic_bic_function)
  
  # Bic weights
  models_comparison_bic_weights_rr_to_sr  <- BICw(models_comparison_aic_bic_rr_to_sr [1,])
  models_comparison_bic_weights_rr_to_sr 
  
  # paste results and save
  models_comparison_aic_bic_results_rr_to_sr  <- cbind(comparison_model_rr_to_sr , models_comparison_aic_bic_rr_to_sr [2, ], models_comparison_aic_bic_rr_to_sr [1, ], models_comparison_bic_weights_rr_to_sr $BICdelta,  models_comparison_bic_weights_rr_to_sr $BICweights)  
  colnames(models_comparison_aic_bic_results_rr_to_sr ) <- c(names(comparison_model_rr_to_sr ), "AIC", "BIC", "BICdelta", "BICweights") 
  models_comparison_aic_bic_results_rr_to_sr 
  model_comparison_select_rr_to_sr  <- which(models_comparison_aic_bic_results_rr_to_sr $BICdelta==0.0)
  model_comparison_selection_rr_to_sr  <- rbind(model_comparison_selection_rr_to_sr , models_comparison_aic_bic_results_rr_to_sr [model_comparison_select_rr_to_sr , ])
}

model_comparison_selection_df_rr_to_sr  <- as.data.frame(model_comparison_selection_rr_to_sr )
single_best_model_rr_to_sr  <- sum(model_comparison_selection_rr_to_sr [ ,3] == "4")
single_best_model_rr_to_sr 
# 5 times sr best fit eqe

shift_best_model_rr_to_sr  <- sum(model_comparison_selection_rr_to_sr [ ,3] == "5")
shift_best_model_rr_to_sr 
# 995 times sr bets fit sr

fit_SR_null_model_simulation_rr_to_sr  <- cbind(fit_sr_sr_simulation_df, fit_rr_to_sr_unique, model_comparison_selection_df_rr_to_sr)
write.csv(fit_SR_null_model_simulation_rr_to_sr, file="cross_validation/rr/fit_rr_to_sr_simulation.csv")
