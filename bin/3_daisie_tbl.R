# Daisie Models 
library(DAISIE)
library(DAISIEprep)

# Working directory
setwd("Macroevolutionary_Drivers")

# Import py_island_tbl_26 with divergence times
py_island_tbl_26 <- read.csv("data/daisie_input/py_tbl_26_final.csv")
py_island_tbl_26_df <- as.data.frame(py_island_tbl_26)

# Make the DAISIE input datafile
py_datalist_26 <- DAISIE::DAISIE_dataprep (
  datatable = py_island_tbl_26_df, # datatable
  island_age = 2.6, 
  M = 2000,
  number_clade_types = 1,
  list_type2_clades = NULL,
  prop_type2_pool = "proportional",
  epss = 1E-5
)

head(py_datalist_26)

###############################
# Plot figure 2 
plot_py (py_datalist_26)
##############################

#########################
# TT model Tyrannidae
#########################
py_island_tbl_26_tyrannidae <- read.csv("data/daisie_input/py_tbl_26_tyrannidae.csv")
py_island_tbl_26_df_tyrannidae <- as.data.frame(py_island_tbl_26_tyrannidae)
py_datalist_26_tyrannidae <- DAISIE_dataprep (
  datatable = py_island_tbl_26_df_tyrannidae, # datatable
  island_age = 2.6,
  M = 2000,
  number_clade_types = 2,
  list_type2_clades = "Tyrannidae",
  prop_type2_pool = 0.19,
  epss = 1E-5)

head(py_datalist_26_tyrannidae)

#########################
# TT model dry forest
#########################
py_island_tbl_26_dryforest <- read.csv("data/daisie_input/py_tbl_26_dryforest.csv")
py_island_tbl_26_df_dryforest <- as.data.frame(py_island_tbl_26_dryforest)
py_datalist_26_dryforest <- DAISIE_dataprep (
  datatable = py_island_tbl_26_df_dryforest, # data table
  island_age = 2.6,
  M = 2000,
  number_clade_types = 2,
  list_type2_clades = "dryforest",
  prop_type2_pool = 0.50,
  epss = 1E-5)

head(py_datalist_26_dryforest)
