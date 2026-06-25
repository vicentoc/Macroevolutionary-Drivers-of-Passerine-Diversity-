# CROSS VALIDATION

library(dplyr)

#set wd
setwd("Macroevolutionary_Drivers")

set.seed(42)
# IMPORT SIMULATIONS FROM TWO TYPE 
two_type_family_simulations <- load("model_simulation/tt_tyrannidae/tt_tyrannidae_simulations.RDATA")
#Toc change the name to
two_groups_simulations_family  <- two_groups_simulations 

# Then all in the same type  
datalist_family <- two_groups_simulations_family

for (i in seq_along(1:1000)) {
  simulation <- datalist_family[[i]]
  numsim <- length(simulation)
  for (j in 2:numsim) {
    datalist_family[[i]][[j]]$type1or2  <- 1
  }} 

# To modify in the table and get one type only
two_groups_simulations_family1 <- datalist_family

for (i in seq_along(1:1000)) {
  two_groups_simulations_family1[[i]][[1]]$not_present_type2 <- NULL
  two_groups_simulations_family1[[i]][[1]]$stt_type1 <- NULL
  two_groups_simulations_family1[[i]][[1]]$stt_type2 <- NULL
  names(two_groups_simulations_family1[[i]][[1]]) <- c("island_age", "not_present", "stt_all")
  number.sp <- 2000 - two_groups_simulations_family1[[i]][[1]]$stt_all[26, 5]
  two_groups_simulations_family1[[i]][[1]]$not_present <- number.sp[[1]]
}

# Table two_groups_simulations_family1
# Table from two type to fit SR

##################################################################################
# IMPORT SIMULATIONS FROM TWO TYPE 
two_type_forest_simulations <- load("model_simulation/tt_dry_forest/tt_dryforest_simulations.RDATA")
#To change the name to
two_groups_simulations_forest  <- two_groups_simulations 
datalist_forest <- two_groups_simulations_forest 

for (i in seq_along(1:1000)) {
  simulation <- datalist_forest[[i]]
  numsim <- length(simulation)
  for (j in 2:numsim) {
    datalist_forest[[i]][[j]]$type1or2  <- 1
  }} 

# To modify in the table and get one type only
two_groups_simulations_forest1 <- datalist_forest

for (i in seq_along(1:1000)) {
  two_groups_simulations_forest1[[i]][[1]]$not_present_type2 <- NULL
  two_groups_simulations_forest1[[i]][[1]]$stt_type1 <- NULL
  two_groups_simulations_forest1[[i]][[1]]$stt_type2 <- NULL
  names(two_groups_simulations_forest1[[i]][[1]]) <- c("island_age", "not_present", "stt_all")
  number.sp <- 2000 - two_groups_simulations_forest1[[i]][[1]]$stt_all[26, 5]
  two_groups_simulations_forest1[[i]][[1]]$not_present <- number.sp[[1]]
}

# Table two_groups_simulations_forest1
# Table from two type to fit SR

##################################################################################################

# To perform cross validation
# Shift some species from type 1 to type 2

# IMPORT SIMULATIONS FROM shift-rate
sr_eqe_simulations <- load("model_simulation/sr/shift_rate_03_eqe_simulations.RDATA")
datalist_sr03eqe_family <- shift_rate_03_eqe_simulations

#Shift some to type 2 with proportion 0.19
prop_family <- 0.19
for (i in seq_along(1:1000)) {
  simulation <- datalist_sr03eqe_family[[i]]
  numsim <- length(simulation)
  for (j in 2:numsim) {
    datalist_sr03eqe_family[[i]][[j]]$type1or2 <- 1 + (runif(1) < prop_family)
  }
  datalist_sr03eqe_family[[i]][[1]]$not_present_type1 <- (1 - prop_family) * 2000 -
    length(which(unlist(datalist_sr03eqe_family[[i]])[which(names(unlist(datalist_sr03eqe_family[[i]])) == "type1or2")] == 1))
  datalist_sr03eqe_family[[i]][[1]]$not_present_type2 <- prop_family * 2000 -
    length(which(unlist(datalist_sr03eqe_family[[i]])[which(names(unlist(datalist_sr03eqe_family[[i]])) == "type1or2")] == 2))
}
datalist_sr03eqe_family[[1]][[1]]

###############################################################################
datalist_sr03eqe_dryforest <- shift_rate_03_eqe_simulations

#Shift some to type 2 with proportion 0.5
prop_dryforest <- 0.55
for (i in seq_along(1:1000)) {
  simulation <- datalist_sr03eqe_dryforest[[i]]
  numsim <- length(simulation)
  for (j in 2:numsim) {
    datalist_sr03eqe_dryforest[[i]][[j]]$type1or2 <- 1 + (runif(1) < prop_dryforest)
  }
  datalist_sr03eqe_dryforest[[i]][[1]]$not_present_type1 <- (1 - prop_dryforest) * 2000 -
    length(which(unlist(datalist_sr03eqe_dryforest[[i]])[which(names(unlist(datalist_sr03eqe_dryforest[[i]])) == "type1or2")] == 1))
  datalist_sr03eqe_dryforest[[i]][[1]]$not_present_type2 <- prop_dryforest * 2000 -
    length(which(unlist(datalist_sr03eqe_dryforest[[i]])[which(names(unlist(datalist_sr03eqe_dryforest[[i]])) == "type1or2")] == 2))
}
datalist_sr03eqe_dryforest[[1]][[1]]

##############################################

# Save the city object
saveRDS(datalist_sr03eqe_family, "cross_validation/simulation_sr_tt_tyrannidae.rds")
saveRDS(datalist_sr03eqe_dryforest, "cross_validation/simulation_sr_tt_dryforest.rds")
saveRDS(two_groups_simulations_forest1,"cross_validation/simulation_tt_dryforest_sr.rds")
saveRDS(two_groups_simulations_family1, "cross_validation/simulation_tt_tyrannidae_sr.rds")
