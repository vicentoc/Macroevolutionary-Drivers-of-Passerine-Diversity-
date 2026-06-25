# Model Simulation

setwd("Macroevolutionary_Drivers")

library(ggpubr)
library(gridExtra)
library(ggplot2)

# load null model simulations
load("model_simulation/null_model/null_model_simulations.RData")
head(single_rate_simulations)

# Plots for simulations with DAISIE simple model
simulated_species_single_rate <- c()
simulated_species_single_rate_endemic <- c()
simulated_species_single_rate_no_endemic <- c()

for (i in seq_along(single_rate_simulations)) {
  simulated_species_number_simple_daisie <- single_rate_simulations[[i]][[1]]$stt_all[26,5]
  simulated_species_single_rate <- rbind(simulated_species_single_rate, simulated_species_number_simple_daisie)
  
  simulated_species_number_simple_daisie_endemic <- single_rate_simulations[[i]][[1]]$stt_all[26,3]
  simulated_species_single_rate_endemic <- rbind(simulated_species_single_rate_endemic, simulated_species_number_simple_daisie_endemic)
  
  simulated_species_number_simple_daisie_no_endemic <- single_rate_simulations[[i]][[1]]$stt_all[26,2]
  simulated_species_single_rate_no_endemic <- rbind(simulated_species_single_rate_no_endemic, simulated_species_number_simple_daisie_no_endemic)
}

# histogram total species

sr_species_total <- 122 
sr_species_endemic <- 10 
sr_species_non_endemic <- 112

head(simulated_species_single_rate)
ggplot(simulated_species_single_rate, aes(x = present)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18)) +
  geom_vline(xintercept = sr_species_total, color = "blue", linetype = "dashed", size = 1)

# histogram endemic species
head(simulated_species_single_rate_endemic)
ggplot(simulated_species_single_rate_endemic, aes(x = nA)) +
  geom_histogram(bins = 15, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = sr_species_endemic, color = "blue", linetype = "dashed", size = 1)


# histogram Non-endemic species
head(simulated_species_single_rate_no_endemic)
ggplot(simulated_species_single_rate_no_endemic, aes(x = nI)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = sr_species_non_endemic, color = "blue", linetype = "dashed", size = 1)

########################################################################################################################
# ESTIMATIONS WITH BEST FITTED MODEL TO THE DATA

#To read the values for simulated islands
single_rate_bootstrap <- read.csv("model_simulation/null_model/null_model_simulations.csv")
# as data frame
single_rate_bootstrap_df <- as.data.frame(single_rate_bootstrap)
# remove Nas
single_rate_bootstrap_df <-na.omit(single_rate_bootstrap_df)

# Values for parameters 
sr_mu <- 1.78	
sr_K	<- "INF"						
sr_gamma <-	0.12
sr_lambda_a	<- 0.17

## Histograms
ggplot(single_rate_bootstrap_df, aes(x = gamma)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = "gamma") +
  theme(axis.text.x = element_text(size=18, color = "black"),
        axis.text.y = element_text(size=18, color = "black")) +
  geom_vline(xintercept = sr_gamma, color = "blue", linetype = "dashed", size = 1)


ggplot(single_rate_bootstrap_df, aes(x = mu)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = "mu") +
  theme(axis.text.x = element_text(size=18, color = "black"),
        axis.text.y = element_text(size=18, color = "black")) +
geom_vline(xintercept = sr_mu, color = "blue", linetype = "dashed", size = 1)

ggplot(single_rate_bootstrap_df, aes(x = lambda_a)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = "Anagenesis (lambda_a)") +
theme(axis.text.x = element_text(size=18, color = "black"),
            axis.text.y = element_text(size=18, color = "black")) +
geom_vline(xintercept = sr_lambda_a, color = "blue", linetype = "dashed", size = 1)

########################################################################################################################
# plot parameters values density for simulated communities 

my_theme_angle <- theme(axis.text.x = element_text(size=24, color = "black"), 
                        axis.text.y = element_text(size=24, color = "black"),
                        labs( x = NULL,  y = NULL, cex = 36))

my_theme = theme(axis.text.x = element_text(size=24, color = "black"), 
                 axis.text.y = element_text(size=24, color = "black"),
                 labs( x = NULL,  y = NULL, cex = 36))
# gamma and mu
sra <- ggplot(single_rate_bootstrap_df, aes(mu, gamma)) +
  my_theme_angle +
  geom_point() +
  geom_point(aes(x=sr_mu,y=sr_gamma), col = 'blue', pch = 19, cex = 3)

# gamma and lambda_a
srb <- ggplot(single_rate_bootstrap_df, aes(lambda_a, gamma)) +
  my_theme_angle +
  geom_point() +
  geom_point(aes(x=sr_lambda_a,y=sr_gamma), col = 'blue', pch = 19, cex = 3) 
plot(srb )

# mu and lambda_a
src <- ggplot(single_rate_bootstrap_df, aes(lambda_a, mu)) +
  my_theme +
  geom_point() +
  geom_point(aes(x=sr_lambda_a,y=sr_mu), col = 'blue', pch = 19, cex = 3)

###########################################################################################################################
# shift_03ml RATE MODEL
#To read the values for simulated islands
shift_03ml_eqe_rate_bootstrap <- read.csv("model_simulation/sr/shift_rate_03_eqe_simulations.csv")
# as data frame
shift_03ml_eqe_rate_bootstrap_df <- as.data.frame(shift_03ml_eqe_rate_bootstrap)
# remove Nas
#shift_03ml_eqe_rate_bootstrap_df <- fit_shift_eqe_sim_simulation_unique 
#write.csv(fit_shift_eqe_sim_simulation_unique, file = "model_simulation\\sr\\fit_shift_eqe_sim_simulation_unique.csv")

# Plots for simulations with DAISIE shift_03ml model
load("model_simulation/sr/shift_rate_03_eqe_simulations.RDATA")
head(shift_rate_03_eqe_simulations)

simulated_species_shift_03ml_eqe_daisie <- c()
simulated_species_shift_03ml_eqe_daisie_endemic <- c()
simulated_species_shift_03ml_eqe_daisie_no_endemic <- c()
for (i in seq_along(shift_rate_03_eqe_simulations)) {
  simulated_species_number_shift_03ml_eqe_daisie <- shift_rate_03_eqe_simulations[[i]][[1]]$stt_all[26,5]
  simulated_species_shift_03ml_eqe_daisie <- rbind(simulated_species_shift_03ml_eqe_daisie, simulated_species_number_shift_03ml_eqe_daisie)
  
  simulated_species_number_shift_03ml_eqe_daisie_endemic <- shift_rate_03_eqe_simulations[[i]][[1]]$stt_all[26,3]
  simulated_species_shift_03ml_eqe_daisie_endemic <- rbind(simulated_species_shift_03ml_eqe_daisie_endemic, simulated_species_number_shift_03ml_eqe_daisie_endemic)
  
  simulated_species_number_shift_03ml_eqe_daisie_no_endemic <- shift_rate_03_eqe_simulations[[i]][[1]]$stt_all[26,2]
  simulated_species_shift_03ml_eqe_daisie_no_endemic <- rbind(simulated_species_shift_03ml_eqe_daisie_no_endemic, simulated_species_number_shift_03ml_eqe_daisie_no_endemic)
}

# histogram for diversity
shift_03mlr_eqe_species_total <- 122
shift_03mlr_eqe_species_endemic <- 10 
shift_03mlr_eqe_species_non_endemic <- 112

head(simulated_species_shift_03ml_eqe_daisie)
ggplot(simulated_species_shift_03ml_eqe_daisie, aes(x = present)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = shift_03mlr_eqe_species_total, color = "blue", linetype = "dashed", size = 1)

# histogram endemic species
head(simulated_species_shift_03ml_eqe_daisie_endemic)
ggplot(simulated_species_shift_03ml_eqe_daisie_endemic, aes(x = nA)) +
  geom_histogram(bins = 15, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = shift_03mlr_eqe_species_endemic, color = "blue", linetype = "dashed", size = 1)

# histogram Non-endemic species
head(simulated_species_shift_03ml_eqe_daisie_no_endemic)
ggplot(simulated_species_shift_03ml_eqe_daisie_no_endemic, aes(x = nI)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = shift_03mlr_eqe_species_non_endemic, color = "blue", linetype = "dashed", size = 1)

#########################################################################################################

shift_03mlr_eqe_mu <- 0.6728
shift_03mlr_eqe_gamma <-  0.0238
shift_03mlr_eqe_lambda_a <- 0.3322

shift_03mlr_eqe_gamma2 <-     0.1479  
shift_03mlr_eqe_lambda_a2     <- 2.40E-14


# Histograms
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(x = mu)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = shift_03mlr_eqe_mu, color = "blue", linetype = "dashed", size = 1)

  
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(x = gamma)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = shift_03mlr_eqe_gamma, color = "blue", linetype = "dashed", size = 1)

ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(x = lambda_a)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL)  +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = shift_03mlr_eqe_lambda_a, color = "blue", linetype = "dashed", size = 1)

ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(x = gamma2)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL)  +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black"))+
  geom_vline(xintercept = shift_03mlr_eqe_gamma2, color = "blue", linetype = "dashed", size = 1)

ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(x = log(lambda_a2))) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black"))+
  geom_vline(xintercept = log(shift_03mlr_eqe_lambda_a2), color = "blue", linetype = "dashed", size = 1)

#Density paremeters
# gamma vs mu
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(mu, gamma)) +
  my_theme +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(y=shift_03mlr_eqe_gamma,x=shift_03mlr_eqe_mu), col = 'blue', pch = 19, cex = 3)

# gamma vs lambda a
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(gamma, lambda_a)) +
  my_theme +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(x=shift_03mlr_eqe_gamma,y=shift_03mlr_eqe_lambda_a), col = 'blue', pch = 19, cex = 3) 

# gamma vs gamma2 
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(gamma, gamma2)) +
  my_theme +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(x=shift_03mlr_eqe_gamma,y=shift_03mlr_eqe_gamma2), col = 'blue', pch = 19, cex = 3)

# mu vs gamma2 
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(mu, gamma2)) +
  my_theme +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(x=shift_03mlr_eqe_mu,y=shift_03mlr_eqe_gamma2), col = 'blue', pch = 19, cex = 3)

# mu vs lambda_a
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(mu, lambda_a)) +
  my_theme +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(x=shift_03mlr_eqe_mu,y=shift_03mlr_eqe_lambda_a), col = 'blue', pch = 19, cex = 3)

# gamma2 vs lambda_a
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(lambda_a, gamma2)) +
  my_theme_angle +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(y=shift_03mlr_eqe_gamma2,x=shift_03mlr_eqe_lambda_a), col = 'blue', pch = 19, cex = 3)

# mu vs lambda_a2
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(mu, log(lambda_a2))) +
  my_theme +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(x=shift_03mlr_eqe_mu,y=log(shift_03mlr_eqe_lambda_a2)), col = 'blue', pch = 19, cex = 3)

# gamma vs lambda_a2
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(gamma,log(lambda_a2))) +
  my_theme_angle +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(x=shift_03mlr_eqe_gamma,y=log(shift_03mlr_eqe_lambda_a2)), col = 'blue', pch = 19, cex = 3)

# gamma2 vs lambda_a2
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(gamma2,log(lambda_a2))) +
  my_theme_angle +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(x=shift_03mlr_eqe_gamma2,y=log(shift_03mlr_eqe_lambda_a2)), col = 'blue', pch = 19, cex = 3)

# lambda vs lambda_a2
ggplot(shift_03ml_eqe_rate_bootstrap_df, aes(lambda_a,log(lambda_a2))) +
  my_theme_angle +
  geom_point() +
  labs( x = NULL, y = NULL) +
  geom_point(aes(x=shift_03mlr_eqe_lambda_a,y=log(shift_03mlr_eqe_lambda_a2)), col = 'blue', pch = 19, cex = 3)

#########################################
# PLOT FOR RELAXED RATE EXTINCTION
#########################################
# Imagen 430 360

#load file
load("model_simulation/rr/relaxed_rate_simulations.RDATA")
relaxed_rate_simulations

# Plots for simulations with DAISIE simple model
simulated_species_relaxed_rate <- c()
simulated_species_relaxed_rate_endemic <- c()
simulated_species_relaxed_rate_no_endemic <- c()

for (i in seq_along(relaxed_rate_simulations)) {
  simulated_species_number_simple_daisie <- relaxed_rate_simulations[[i]][[1]]$stt_all[26,5]
  simulated_species_relaxed_rate <- rbind(simulated_species_relaxed_rate, simulated_species_number_simple_daisie)
  
  simulated_species_number_simple_daisie_endemic <- relaxed_rate_simulations[[i]][[1]]$stt_all[26,3]
  simulated_species_relaxed_rate_endemic <- rbind(simulated_species_relaxed_rate_endemic, simulated_species_number_simple_daisie_endemic)
  
  simulated_species_number_simple_daisie_no_endemic <- relaxed_rate_simulations[[i]][[1]]$stt_all[26,2]
  simulated_species_relaxed_rate_no_endemic <- rbind(simulated_species_relaxed_rate_no_endemic, simulated_species_number_simple_daisie_no_endemic)
}

# histogram total species

relax_species_total <- 122 
relax_species_endemic <- 10 
relax_species_non_endemic <- 112 

head(simulated_species_relaxed_rate)

ggplot(simulated_species_relaxed_rate, aes(x = present)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = relax_species_total, color = "blue", linetype = "dashed", size = 1)

# histogram endemic species
head(simulated_species_relaxed_rate_endemic)
ggplot(simulated_species_relaxed_rate_endemic, aes(x = nA)) +
  geom_histogram(bins = 15, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = relax_species_endemic, color = "blue", linetype = "dashed", size = 1)

# histogram Non-endemic species
head(simulated_species_relaxed_rate_no_endemic)
ggplot(simulated_species_relaxed_rate_no_endemic, aes(x = nI)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = relax_species_non_endemic, color = "blue", linetype = "dashed", size = 1)

# ESTIMATIONS WITH BEST FITTED MODEL TO THE DATA
##############################
#To read the values for simulated islands
relaxed_rate_bootstrap <- read.csv("model_simulation/rr/relaxed_rate_simulations_table.csv")
# as data fram
relaxed_rate_bootstrap_df <- as.data.frame(relaxed_rate_bootstrap)
# remove Nas
relaxed_rate_bootstrap_df <-na.omit(relaxed_rate_bootstrap_df)
# columns 3,5,6,8,10,11

# 0	5.434908769	Inf	0.196463454	0.172767033	4.032640345	-529.1956714	4	extinction

relax_mu <- 5.43
relax_gamma <-	0.19
relax_lambda_a	<- 0.17
relax_sd	<- 4.03

## Histograms
ggplot(relaxed_rate_bootstrap_df, aes(x = gamma)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = relax_gamma, color = "blue", linetype = "dashed", size = 1)

ggplot(relaxed_rate_bootstrap_df, aes(x = mu)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = relax_mu, color = "blue", linetype = "dashed", size = 1)

ggplot(relaxed_rate_bootstrap_df, aes(x = lambda_a)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = relax_lambda_a, color = "blue", linetype = "dashed", size = 1)

ggplot(relaxed_rate_bootstrap_df, aes(x = sd)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = relax_sd, color = "blue", linetype = "dashed", size = 1)

########
# gamma and mu
ggplot(relaxed_rate_bootstrap_df, aes(mu, gamma)) +
  my_theme_angle +
  geom_point() +
  labs( x = NULL,  y = NULL, cex = 36) +
  geom_point(aes(x=relax_mu,y=relax_gamma), col = 'blue', pch = 19, cex = 3)

# gamma and lambda_a
ggplot(relaxed_rate_bootstrap_df, aes( gamma,lambda_a)) +
  my_theme_angle +
  labs( x = NULL,  y = NULL, cex = 36) +
  geom_point() +
  geom_point(aes(y=relax_lambda_a,x=relax_gamma), col = 'blue', pch = 19, cex = 3) 

# mu and lambda_a
ggplot(relaxed_rate_bootstrap_df, aes(mu, lambda_a)) +
  my_theme +
  geom_point() +
  labs( x = NULL,  y = NULL, cex = 36) +
  geom_point(aes(y=relax_lambda_a,x=relax_mu), col = 'blue', pch = 19, cex = 3)

########################################################################
# two RATE MODEL DRY FOREST
two_rate_forest_bootstrap <- read.csv("model_simulation/tt_dry_forest/tt_dryforest_simulations.csv")
# as data frame
two_rate_forest_bootstrap_df <- as.data.frame(two_rate_forest_bootstrap)
# remove Nas
two_rate_forest_bootstrap_df <-na.omit(two_rate_forest_bootstrap_df)

# Plots for simulations with DAISIE two model

load("model_simulation/tt_dry_forest/tt_dryforest_simulations.RDATA")
two_forest_rate_simulations <- two_groups_simulations 
simulated_species_two_forest_daisie1 <- c()
simulated_species_two_forest_daisie_endemic1 <- c()
simulated_species_two_forest_daisie_no_endemic1 <- c()
simulated_species_two_forest_daisie2 <- c()
simulated_species_two_forest_daisie_endemic2 <- c()
simulated_species_two_forest_daisie_no_endemic2 <- c()

for (i in seq_along(two_forest_rate_simulations)) {
  simulated_species_number_two_forest_daisie1 <- two_forest_rate_simulations[[i]][[1]]$stt_type1[26,5]
  simulated_species_two_forest_daisie1 <- rbind(simulated_species_two_forest_daisie1, simulated_species_number_two_forest_daisie1)
  
  simulated_species_number_two_forest_daisie_endemic1 <- two_forest_rate_simulations[[i]][[1]]$stt_type1[26,3]
  simulated_species_two_forest_daisie_endemic1 <- rbind(simulated_species_two_forest_daisie_endemic1, simulated_species_number_two_forest_daisie_endemic1)
  
  simulated_species_number_two_forest_daisie_no_endemic1 <- two_forest_rate_simulations[[i]][[1]]$stt_type1[26,2]
  simulated_species_two_forest_daisie_no_endemic1 <- rbind(simulated_species_two_forest_daisie_no_endemic1, simulated_species_number_two_forest_daisie_no_endemic1)
  
  # for 2
  simulated_species_number_two_forest_daisie2 <- two_forest_rate_simulations[[i]][[1]]$stt_type2[26,5]
  simulated_species_two_forest_daisie2 <- rbind(simulated_species_two_forest_daisie2, simulated_species_number_two_forest_daisie2)
  
  simulated_species_number_two_forest_daisie_endemic2 <- two_forest_rate_simulations[[i]][[1]]$stt_type2[26,3]
  simulated_species_two_forest_daisie_endemic2 <- rbind(simulated_species_two_forest_daisie_endemic2, simulated_species_number_two_forest_daisie_endemic2)
  
  simulated_species_number_two_forest_daisie_no_endemic2 <- two_forest_rate_simulations[[i]][[1]]$stt_type2[26,2]
  simulated_species_two_forest_daisie_no_endemic2 <- rbind(simulated_species_two_forest_daisie_no_endemic2, simulated_species_number_two_forest_daisie_no_endemic2)
}

# for species 
two_f_r_species_total1 <- 55
two_f_r_species_total2 <- 67

two_f_r_species_endemic1 <- 4
two_f_r_species_endemic2 <- 6

two_f_r_species_non_endemic1 <- 51
two_f_r_species_non_endemic2 <- 61

# FOR SPECIES TYPE 1 (non-DRY FOREST)
# histogram total species
head(simulated_species_two_forest_daisie1)
ggplot(simulated_species_two_forest_daisie1, aes(x = present)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_f_r_species_total1, color = "blue", linetype = "dashed", size = 1)

# histogram endemic species
head(simulated_species_two_forest_daisie_endemic1)
ggplot(simulated_species_two_forest_daisie_endemic1, aes(x = nA)) +
  geom_histogram(bins = 15, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_f_r_species_endemic1, color = "blue", linetype = "dashed", size = 1)

# histogram Non-endemic species
head(simulated_species_two_forest_daisie_no_endemic1)
ggplot(simulated_species_two_forest_daisie_no_endemic1, aes(x = nI)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_f_r_species_non_endemic1, color = "blue", linetype = "dashed", size = 1)

#################################################################################################
# FOR SPECIES TYPE 2 (DRY FOREST)
# histogram total species
head(simulated_species_two_forest_daisie2)
ggplot(simulated_species_two_forest_daisie2, aes(x = present)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_f_r_species_total2, color = "blue", linetype = "dashed", size = 1)

# histogram endemic species
head(simulated_species_two_forest_daisie_endemic2)
ggplot(simulated_species_two_forest_daisie_endemic2, aes(x = nA)) +
  geom_histogram(bins = 10, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_f_r_species_endemic2, color = "blue", linetype = "dashed", size = 1)

# histogram Non-endemic species
head(simulated_species_two_forest_daisie_no_endemic2)
ggplot(simulated_species_two_forest_daisie_no_endemic2, aes(x = nI)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_f_r_species_non_endemic2, color = "blue", linetype = "dashed", size = 1)

##################################################

two_f_lambdaa <- 0.0
two_f_mu <- 1.7818  
two_f_K <- Inf                           
two_f_gamma <-  0.1043
two_f_lambda_a  <-  0.1746

two_f_lambdaa <- 0.0
two_f_mu2 <- 1.7818
two_f_K <- Inf                                
two_f_gamma2 <- 0.1284
two_f_lambda_a2 <- 0.1746 

# Histograms
ggplot(two_rate_forest_bootstrap_df, aes(x = mu)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = two_f_mu, color = "blue", linetype = "dashed", size = 1)

ggplot(two_rate_forest_bootstrap_df, aes(x = gamma)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = two_f_gamma, color = "blue", linetype = "dashed", size = 1)

ggplot(two_rate_forest_bootstrap_df, aes(x = lambda_a)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = two_f_lambda_a, color = "blue", linetype = "dashed", size = 1)

ggplot(two_rate_forest_bootstrap_df, aes(x = gamma2)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=24, color = "black"),
        axis.text.y = element_text(size=24, color = "black")) +
  geom_vline(xintercept = two_f_gamma2, color = "blue", linetype = "dashed", size = 1)

##################################################
#Density parameter values

#GROUP 1
# gamma mu
ggplot(two_rate_forest_bootstrap_df, aes(mu, gamma)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(y=two_f_gamma,x=two_f_mu), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_forest_bootstrap_df$gamma, y=two_rate_forest_bootstrap_df$mu)

# gamma mu
ggplot(two_rate_forest_bootstrap_df, aes(mu, lambda_a)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(x=two_f_mu,y=two_f_lambda_a), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_forest_bootstrap_df$gamma, y=two_rate_forest_bootstrap_df$mu)

# gamma lambda_a
ggplot(two_rate_forest_bootstrap_df, aes(gamma, lambda_a)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(x=two_f_gamma,y=two_f_lambda_a), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_forest_bootstrap_df$gamma, y=two_rate_forest_bootstrap_df$mu)

#GROUP 2
# gamma mu2
ggplot(two_rate_forest_bootstrap_df, aes(mu2, gamma2)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(y=two_f_gamma2,x=two_f_mu2), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_forest_bootstrap_df$gamma, y=two_rate_forest_bootstrap_df$mu)

# gamma2 mu2
ggplot(two_rate_forest_bootstrap_df, aes(gamma2, lambda_a2)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(x=two_f_gamma2,y=two_f_lambda_a2), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_forest_bootstrap_df$gamma, y=two_rate_forest_bootstrap_df$mu)


#################################################################################################
# Plots for two rate model Tyrannidae
two_rate_family_bootstrap <- read.csv("model_simulation/tt_tyrannidae/tt_tyrannidae_simulations.csv")
# as data frame
two_rate_family_bootstrap_df <- as.data.frame(two_rate_family_bootstrap[1:16])
# remove Nas
two_rate_family_bootstrap_df <-na.omit(two_rate_family_bootstrap_df)

# Plots for simulations with DAISIE two model

load("model_simulation/tt_tyrannidae/tt_tyrannidae_simulations.RDATA")
two_family_rate_simulations <- two_groups_simulations 
simulated_species_two_family_daisie1 <- c()
simulated_species_two_family_daisie_endemic1 <- c()
simulated_species_two_family_daisie_no_endemic1 <- c()
simulated_species_two_family_daisie2 <- c()
simulated_species_two_family_daisie_endemic2 <- c()
simulated_species_two_family_daisie_no_endemic2 <- c()

for (i in seq_along(two_family_rate_simulations)) {
  simulated_species_number_two_family_daisie1 <- two_family_rate_simulations[[i]][[1]]$stt_type1[26,5]
  simulated_species_two_family_daisie1 <- rbind(simulated_species_two_family_daisie1, simulated_species_number_two_family_daisie1)
  
  simulated_species_number_two_family_daisie_endemic1 <- two_family_rate_simulations[[i]][[1]]$stt_type1[26,3]
  simulated_species_two_family_daisie_endemic1 <- rbind(simulated_species_two_family_daisie_endemic1, simulated_species_number_two_family_daisie_endemic1)
  
  simulated_species_number_two_family_daisie_no_endemic1 <- two_family_rate_simulations[[i]][[1]]$stt_type1[26,2]
  simulated_species_two_family_daisie_no_endemic1 <- rbind(simulated_species_two_family_daisie_no_endemic1, simulated_species_number_two_family_daisie_no_endemic1)
  
  # for 2
  simulated_species_number_two_family_daisie2 <- two_family_rate_simulations[[i]][[1]]$stt_type2[26,5]
  simulated_species_two_family_daisie2 <- rbind(simulated_species_two_family_daisie2, simulated_species_number_two_family_daisie2)
  
  simulated_species_number_two_family_daisie_endemic2 <- two_family_rate_simulations[[i]][[1]]$stt_type2[26,3]
  simulated_species_two_family_daisie_endemic2 <- rbind(simulated_species_two_family_daisie_endemic2, simulated_species_number_two_family_daisie_endemic2)
  
  simulated_species_number_two_family_daisie_no_endemic2 <- two_family_rate_simulations[[i]][[1]]$stt_type2[26,2]
  simulated_species_two_family_daisie_no_endemic2 <- rbind(simulated_species_two_family_daisie_no_endemic2, simulated_species_number_two_family_daisie_no_endemic2)
}


#for species 
two_fam_r_species_total1 <- 95
two_fam_r_species_total2 <- 27 

two_fam_r_species_endemic1 <- 9
two_fam_r_species_endemic2 <- 1

two_fam_r_species_non_endemic1 <- 86
two_fam_r_species_non_endemic2 <- 26

# histogram total species
head(simulated_species_two_family_daisie1)
ggplot(simulated_species_two_family_daisie1, aes(x = present)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_fam_r_species_total1, color = "blue", linetype = "dashed", size = 1)

# histogram endemic species
head(simulated_species_two_family_daisie_endemic1)
ggplot(simulated_species_two_family_daisie_endemic1, aes(x = nA)) +
  geom_histogram(bins = 15, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_fam_r_species_endemic1, color = "blue", linetype = "dashed", size = 1)

# histogram Non-endemic species
head(simulated_species_two_family_daisie_no_endemic1)
ggplot(simulated_species_two_family_daisie_no_endemic1, aes(x = nI)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_fam_r_species_non_endemic1, color = "blue", linetype = "dashed", size = 1)

# FOR SPECIES TYPE 2 (DRY family)
head(simulated_species_two_family_daisie2)
ggplot(simulated_species_two_family_daisie2, aes(x = present)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_fam_r_species_total2, color = "blue", linetype = "dashed", size = 1)

# histogram endemic species
head(simulated_species_two_family_daisie_endemic2)
ggplot(simulated_species_two_family_daisie_endemic2, aes(x = nA)) +
  geom_histogram(bins = 8, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_fam_r_species_endemic2, color = "blue", linetype = "dashed", size = 1)

# histogram Non-endemic species
head(simulated_species_two_family_daisie_no_endemic2)
ggplot(simulated_species_two_family_daisie_no_endemic2, aes(x = nI)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL,  y = NULL, cex = 36) +
  theme(axis.text.x = element_text(size=22, color = "black"),
        axis.text.y = element_text(size=22, color = "black"),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)) +
  geom_vline(xintercept = two_fam_r_species_non_endemic2, color = "blue", linetype = "dashed", size = 1)


# TO PLOT PARAMETER STIMATION FOR THESE GROUPS
# BEST AIC best likelihood
# t tyrannidae_ndd_cl0_eqe_eqa*    0      1.780187192     Inf     0.117413948     0.167790933
#                                  0      1.780187192     Inf     0.138503001     0.167790933     0.19

two_fam_lambdaa <- 0.0
two_fam_mu <-  1.7832
two_fam_K <- Inf                           
two_fam_gamma <-   0.1115
two_fam_lambda_a  <-  0.1746

two_fam_lambdaa <- 0.0
two_fam_mu2 <-  1.7832
two_fam_K <- Inf                                
two_fam_gamma2 <- 0.1379
two_fam_lambda_a2 <- 0.1746


# Histograms
ggplot(two_rate_family_bootstrap_df, aes(x = mu)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=28, color = "black"),
        axis.text.y = element_text(size=28, color = "black")) +
  geom_vline(xintercept = two_fam_mu, color = "blue", linetype = "dashed", size = 1)

ggplot(two_rate_family_bootstrap_df, aes(x = gamma)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=28, color = "black"),
        axis.text.y = element_text(size=28, color = "black")) +
  geom_vline(xintercept = two_fam_gamma, color = "blue", linetype = "dashed", size = 1)


ggplot(two_rate_family_bootstrap_df, aes(x = lambda_a)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=28, color = "black"),
        axis.text.y = element_text(size=28, color = "black")) +
  geom_vline(xintercept = two_fam_lambda_a, color = "blue", linetype = "dashed", size = 1)

ggplot(two_rate_family_bootstrap_df, aes(x = gamma2)) +
  geom_histogram(bins = 30, fill = "gray", color = "white") +
  labs( x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size=28, color = "black"),
        axis.text.y = element_text(size=28, color = "black")) +
  geom_vline(xintercept = two_fam_gamma2, color = "blue", linetype = "dashed", size = 1)


#GROUP 1
#Density parameter values
#GROUP 1
# gamma mu
ggplot(two_rate_family_bootstrap_df, aes(mu, gamma)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(y=two_fam_gamma,x=two_fam_mu), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_family_bootstrap_df$gamma, y=two_rate_family_bootstrap_df$mu)

# mu lambda
ggplot(two_rate_family_bootstrap_df, aes(mu, lambda_a)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(x=two_fam_mu,y=two_fam_lambda_a), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_family_bootstrap_df$gamma, y=two_rate_family_bootstrap_df$mu)

# gamma lambda_a
ggplot(two_rate_family_bootstrap_df, aes(gamma, lambda_a)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(x=two_fam_gamma,y=two_fam_lambda_a), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_family_bootstrap_df$gamma, y=two_rate_family_bootstrap_df$mu)

#GROUP 2
# gamma mu2
ggplot(two_rate_family_bootstrap_df, aes(mu2, gamma2)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(y=two_fam_gamma2,x=two_fam_mu2), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_family_bootstrap_df$gamma, y=two_rate_family_bootstrap_df$mu)

# gamma2 lambda2
ggplot(two_rate_family_bootstrap_df, aes(gamma2, lambda_a2)) +
  my_theme_angle +
  labs( x = NULL, y = NULL) +
  geom_point() +
  geom_point(aes(x=two_fam_gamma2,y=two_fam_lambda_a2), col = 'blue', pch = 19, cex = 3)
#R2_gamma_mu <- rsq(x=two_rate_family_bootstrap_df$gamma, y=two_rate_family_bootstrap_df$mu)

#########################################################################################################
# Figure 3
# Shift rate best model LTT PLOT

DAISIE::DAISIE_plot_sims(shift_rate_03_eqe_simulations)

####################################################################################
# Figure SM2
load("model_simulation/sr/best_sr_no_selected/best_models_sr_simulations.RDATA")

DAISIE::DAISIE_plot_sims(shift_rate_simulations)
DAISIE::DAISIE_plot_sims(shift_rate_eqa_simulations)
DAISIE::DAISIE_plot_sims(shift_rate_eqa_eqe_simulations)
DAISIE::DAISIE_plot_sims(shift_rate_eqi_simulations)
DAISIE::DAISIE_plot_sims(shift_rate_eqi_eqa_simulations)
DAISIE::DAISIE_plot_sims(shift_rate_eqi_eqe_simulations)

####################################################################################
# Figure SM11

# Single rate LTT PLOT
DAISIE::DAISIE_plot_sims(single_rate_simulations)
DAISIE::DAISIE_plot_sims(relaxed_rate_simulations)
DAISIE::DAISIE_plot_sims(two_forest_rate_simulations, type = "type1_species")
DAISIE::DAISIE_plot_sims(two_forest_rate_simulations, type = "type2_species")
DAISIE::DAISIE_plot_sims(two_family_rate_simulations, type = "type1_species")
DAISIE::DAISIE_plot_sims(two_family_rate_simulations, type = "type2_species")
