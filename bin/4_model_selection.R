# Model selection

# Working directory
setwd("Macroevolutionary_Drivers")

library(dplyr)
library(cogmod)

# Function to calculate BIC 
aic_bic_function <- function(x, loglik_model, df_model) {
  loglik_model=x[1]
  df_model=x[2]
  log_sample <- log(127)# samples model 
  df_model_bic <- 2*loglik_model
  bic_model <- df_model*log_sample-(df_model_bic)
  
  aic_model <- 2*df_model - df_model_bic
  aic_bic <- c(bic_model, aic_model)
  names(aic_bic)<-c("bic_model", "aic_model")
  return(aic_bic )
}

# for Daisie models 
# Single Rate Model
single_rate_results <- read.csv("model_selection/null_model/null_model_results.csv")
single_rate_results <- as.data.frame(single_rate_results)

#select best run per model 
null_filtrado <- single_rate_results %>%
  group_by(model_name) %>%
  filter(loglik == max(loglik)) %>%
  slice(1) %>%
  ungroup()  
null_filtrado

single_rate_results <- as.data.frame(null_filtrado)
single_rate_results_cols <- select(single_rate_results, loglik, df)
single_rate_aic_bic <- apply(single_rate_results_cols, 1, aic_bic_function)

# Bic weights
single_rate_bic_weights <- BICw(single_rate_aic_bic[1,])
single_rate_bic_weights

# paste results and save
single_rate_aic_bic_results <- cbind(single_rate_results, single_rate_aic_bic[2, ], single_rate_aic_bic[1, ], single_rate_bic_weights$BICdelta,  single_rate_bic_weights$BICweights)  
colnames(single_rate_aic_bic_results) <- c(names(single_rate_results), "AIC", "BIC", "BICdelta", "BICweights") 
single_rate_aic_bic_results <- single_rate_aic_bic_results[order(single_rate_aic_bic_results$BICdelta), ]

write.csv(single_rate_aic_bic_results, "model_selection/null_model/single_rate_results_aic_bic.csv")

################################################################################
### FOR THE FAMILY
dryforest_tr_results <- read.csv("model_selection/tt_dry_forest/tt_dryforest_results.csv")
dryforest_tr_results  <- as.data.frame(dryforest_tr_results)

#select best run per model 
tr_filtrado <- dryforest_tr_results %>%
  group_by(model_name) %>%
  filter(loglik == max(loglik)) %>%
  slice(1) %>%
  ungroup()  
tr_filtrado

# df best run per model
dryforest_tr_filtrado  <- as.data.frame(tr_filtrado)

# CALCULATE AIC and BIC
tr_results_cols <- select(dryforest_tr_filtrado, loglik, df)
tr_results_aic_bic <- apply(tr_results_cols, 1, aic_bic_function)

# Bic weights
tr_bic_weights <- BICw(tr_results_aic_bic[1,])
tr_bic_weights

# paste results and save
tr_aic_bic_results <- cbind(dryforest_tr_filtrado, tr_results_aic_bic[2, ], tr_results_aic_bic[1, ], tr_bic_weights$BICdelta, tr_bic_weights$BICweights)  
colnames(tr_aic_bic_results) <- c(names(dryforest_tr_filtrado), "AIC", "BIC", "BICdelta", "BICweights") 
tr_aic_bic_results <- tr_aic_bic_results[order(tr_aic_bic_results$BICdelta), ]

write.csv(tr_aic_bic_results, "model_selection/tt_dry_forest/tt_dryforest_results_aic_bic.csv")

########################################
### FOR THE FAMILY
tyrannidae_tr_results <- read.csv("model_selection/tt_tyrannidae/tt_tyrannidae_results.csv")
tyrannidae_tr_results  <- as.data.frame(tyrannidae_tr_results)

#select best run per model 
tr_tyrannidae_filtrado <- tyrannidae_tr_results %>%
  group_by(model_name) %>%
  filter(loglik == max(loglik)) %>%
  slice(1) %>%
  ungroup()  
tr_tyrannidae_filtrado

# df best run per model
tyrannidae_tr_filtrado  <- as.data.frame(tr_tyrannidae_filtrado)

# CALCULATE AIC and BIC
tr_tyrannidae_results_cols <- select(tr_tyrannidae_filtrado, loglik, df)
tr_tyrannidae_results_aic_bic <- apply(tr_tyrannidae_results_cols, 1, aic_bic_function)

# Bic weights
tr_tyrannidae_bic_weights <- BICw(tr_tyrannidae_results_aic_bic[1,])
tr_tyrannidae_bic_weights

# paste results and save
tr_tyrannidae_aic_bic_results <- cbind(tyrannidae_tr_filtrado, tr_tyrannidae_results_aic_bic[2, ], tr_tyrannidae_results_aic_bic[1, ], tr_tyrannidae_bic_weights$BICdelta, tr_tyrannidae_bic_weights$BICweights)  
colnames(tr_tyrannidae_aic_bic_results) <- c(names(tyrannidae_tr_filtrado), "AIC", "BIC", "BICdelta", "BICweights") 
tr_tyrannidae_aic_bic_results <- tr_tyrannidae_aic_bic_results[order(tr_tyrannidae_aic_bic_results$BICdelta), ]

write.csv(tr_tyrannidae_aic_bic_results, "model_selection/tt_tyrannidae/tt_tyrannidae_results_aic_bic.csv")########################################

####################################
# rr models
####################################
relaxed_all_rate_results <- read.csv("model_selection/rr/relaxed_rate_results.csv")
relaxed_all_rate_results <- as.data.frame(relaxed_all_rate_results)

#select best run per model 
relaxed_rate_results <- relaxed_all_rate_results %>%
  group_by(model) %>%
  filter(loglik == max(loglik)) %>%
  slice(1) %>%
  ungroup()  
relaxed_rate_results

relaxed_all_rate_results_cols <- select(relaxed_rate_results, loglik, df)
models_rate_aic_bic <- apply(relaxed_all_rate_results_cols, 1, aic_bic_function)

# Bic weights
relaxed_all_rate_bic_weights <- BICw(models_rate_aic_bic[1,])
relaxed_all_rate_bic_weights

# paste results and save
relaxed_all_rate_aic_bic_results <- cbind(relaxed_rate_results, models_rate_aic_bic[2, ], models_rate_aic_bic[1, ], relaxed_all_rate_bic_weights$BICdelta,  relaxed_all_rate_bic_weights$BICweights)  
colnames(relaxed_all_rate_aic_bic_results) <- c(names(relaxed_rate_results), "AIC", "BIC", "BICdelta", "BICweights") 
relaxed_all_rate_aic_bic_results

relaxed_all_rate_aic_bic_results <- relaxed_all_rate_aic_bic_results[order(relaxed_all_rate_aic_bic_results$BICdelta), ]
write.csv(relaxed_all_rate_aic_bic_results, file = "model_selection/rr/relaxed_rate_results_aic_bic.csv")

#######################################################
#SR models 

vector_names_sr_models_results <- c("sr_results", "sr_eqa_results",
                            "sr_eqa_eqe_results", "sr_eqe_results",
                            "sr_eqi_results", "sr_eqi_eqa_results",
                            "sr_eqi_eqe_results")

vector_names_sr_models <- c("sr", "sr_eqa",
                            "sr_eqa_eqe", "sr_eqe",
                            "sr_eqi", "sr_eqi_eqa",
                            "sr_eqi_eqe")

#import csv
#select higher likelihood per model
for (i in seq_along (vector_names_sr_models)) { 
  sr_data <- read.csv(paste0("model_selection/sr/",vector_names_sr_models[i],"/", vector_names_sr_models_results[i], ".csv"))
  sr_data_df <- as.data.frame(sr_data)
  df_filtrado <- sr_data_df %>%
    group_by(likelihood_time.i.) %>%
    filter(loglik == max(loglik)) %>%
    slice(1) %>%
    ungroup()  
  df_filtrado <-  df_filtrado %>%
    mutate(decision = ifelse(mu < lambda_a, "rejected", "accepted"))
  df_filtrado_df <- as.data.frame(df_filtrado)
  assign(paste0("conditional_",vector_names_sr_models[i]), df_filtrado_df)
  }

# make a table with all models
conditional_sr_results  <- c()
for (i in seq_along (vector_names_sr_models)) { 
  get_df <- get(paste0("conditional_",vector_names_sr_models[i]))
  print(colnames(get_df))
 conditional_sr_results <- rbind(conditional_sr_results, get_df )
  }

# rm 0.01 values
conditional_sr_results <- conditional_sr_results %>%
  filter(!likelihood_time.i. %in%  c(0.31, 0.71,1.61, 1.81 ))

# save the results
write.csv(conditional_sr_results, "model_selection/sr/sr_conditional_results.csv")

# For conditional SR models
#shift_all_rate_results_conditional <- read.csv("sr/best_shift_rate_time_conditional_cl0.csv")
#shift_all_rate_results_conditional <- as.data.frame(shift_all_rate_results_conditional)
#shift_all_rate_results_conditional_cols <- select(shift_all_rate_results_conditional, loglik, df)

# calculate AIC and BIC
conditional_sr_results_df <- as.data.frame(conditional_sr_results)
head(conditional_sr_results_df)
# remove data with higher speciation rate
conditional_sr_results_accepted <- conditional_sr_results_df %>%
  filter(decision != "rejected")
models_sr_aic_bic <- apply(conditional_sr_results_accepted[ ,c(15,16)], 1, aic_bic_function)
# Bic weights
best_sr_bic_weights <- BICw(models_sr_aic_bic[1,])
best_sr_bic_weights

# paste results and save
conditional_sr_aic_bic_results <- cbind(conditional_sr_results_accepted, models_sr_aic_bic[2, ], models_sr_aic_bic[1, ], best_sr_bic_weights$BICdelta,  best_sr_bic_weights$BICweights)  
colnames(conditional_sr_aic_bic_results) <- c(names(conditional_sr_results_accepted), "AIC", "BIC", "BICdelta", "BICweights") 
head(conditional_sr_aic_bic_results)

conditional_sr_aic_bic_results <- conditional_sr_aic_bic_results[order(conditional_sr_aic_bic_results$BICdelta), ]
write.csv(conditional_sr_aic_bic_results, file = "model_selection/sr/sr_conditional_results_aic_bic.csv")
############################################################

# PLOT LIKELIHOOD AND PARAMETERS VALUES FOR BEST SHIFT MODEL
# FIG SM2
library(ggplot2)

# as data fram
shift_rate_all_loglik_df <- as.data.frame(conditional_sr_results)

p_all <- ggplot(data = shift_rate_all_loglik_df, aes(x = likelihood_time.i., y = loglik)) +
  geom_point(aes(color = factor(decision))) +
  labs(x = "Time shift in rates") +
  #geom_point( size = 1.5) 
  theme_bw()

p_all + facet_grid(model_name ~ ., scale = "free_y") +
  theme(legend.position='none') +
  scale_color_brewer(palette="Paired") +
  scale_x_reverse()

##################################
## Overall best models

head(shift_rate_all_loglik_df)
sr_best_model_filtrado <- conditional_sr_results_accepted %>%
  group_by(model_name) %>%
  filter(loglik == max(loglik)) %>%
  slice(1) %>%
  ungroup()  

# select colunms
sr_best_model_results <- as.data.frame(sr_best_model_filtrado)
sr_best_model_results_cols <- select(sr_best_model_results, loglik, df)

# Bic weights
sr_best_models_aic_bic <- apply(sr_best_model_results_cols, 1, aic_bic_function)
sr_best_models_aic_bic_weights <- BICw(sr_best_models_aic_bic[1,])
sr_best_models_aic_bic_weights

# paste results and save
sr_best_models_aic_bic_results <- cbind(sr_best_model_results, sr_best_models_aic_bic[2, ], sr_best_models_aic_bic[1, ], sr_best_models_aic_bic_weights$BICdelta,  sr_best_models_aic_bic_weights$BICweights)  
colnames(sr_best_models_aic_bic_results) <- c(names(sr_best_model_results), "AIC", "BIC", "BICdelta", "BICweights") 
write.csv(sr_best_models_aic_bic_results, file = "model_selection/sr/Table_SM1.csv")

####################################
# #best model
####################################
best_all_rate_results <- read.csv("model_selection/best_model_selection.csv")
best_all_rate_results <- as.data.frame(best_all_rate_results)

best_all_rate_results_cols <- select(best_all_rate_results, Loglik, df)
models_rate_aic_bic <- apply(best_all_rate_results_cols, 1, aic_bic_function)

# Bic weights
best_all_rate_bic_weights <- BICw(models_rate_aic_bic[1,])
best_all_rate_bic_weights

# paste results and save
best_all_rate_aic_bic_results <- cbind(best_all_rate_results, models_rate_aic_bic[2, ], models_rate_aic_bic[1, ], best_all_rate_bic_weights$BICdelta,  best_all_rate_bic_weights$BICweights)  
colnames(best_all_rate_aic_bic_results) <- c(names(best_all_rate_results), "AIC", "BIC", "BICdelta", "BICweights") 
best_all_rate_aic_bic_results

write.csv(best_all_rate_aic_bic_results, file = "model_selection/best_model_aic_bic_results.csv")
