library(DAISIEprep)
library(ape)
library(phylobase)

# setwd to genetic_sequences folder
getwd()
setwd("Macroevolutionary_Drivers")
#Load function ars_plot
source("data/ars_plot.R")

# Family Cardinalinae
# Cardinalis cardinalis
# from the ultrametric tree 
cardinalis_tree <- ape::read.nexus("trees/cardinalidae/cardinalis_mcct.tree")
cardinalis_tree <- as(cardinalis_tree, "phylo4")
phylobase::plot(cardinalis_tree, cex = 0.1)

# select individuals on the island and its status
cardinalis_status <- data.frame(tip_labels = c("Cardinalis_cardinalis_JF795728_Yucatan",
                                               "Cardinalis_cardinalis_JF795720_Yucatan",
                                               "Cardinalis_cardinalis_JF795723_Yucatan",
                                               "Cardinalis_cardinalis_JF795724_Yucatan",
                                               "Cardinalis_cardinalis_JF795725_Yucatan",
                                               "Cardinalis_cardinalis_JF795726_Yucatan",
                                               "Cardinalis_cardinalis_JF795727_Yucatan",
                                               "Cardinalis_cardinalis_JF795729_Campeche",
                                               "Cardinalis_cardinalis_JF795737_Isla_Cozumel",
                                               "Cardinalis_cardinalis_JF795736_Isla_Cozumel"),
                                tip_endemicity_status = c("nonendemic", "nonendemic", "nonendemic", "nonendemic",
                                                          "nonendemic", "nonendemic", "nonendemic", "nonendemic", "nonendemic","nonendemic"))

# set the endemicity status in the tree
cardinalis_endemicity <- create_endemicity_status(
  phylo = cardinalis_tree,
  island_species = cardinalis_status)

cardinalis_phylo <- phylobase::phylo4d(cardinalis_tree, cardinalis_endemicity)

#to save in RDS format
saveRDS(cardinalis_phylo, file = "trees/cardinalidae/cardinalis_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
cardinalis_phylod <- readRDS("trees/cardinalidae/cardinalis_rds.rds")
cardinalis_phylod_asr <- add_asr_node_states(phylod = cardinalis_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

cardinalis_island_tbl <- extract_island_species(
  phylod = cardinalis_phylod_asr, 
  extraction_method = "asr")

ars_plot(cardinalis_phylod_asr, node_pies = TRUE)
cardinalis_island_tbl
cardinalis_island_tbl@island_tbl$branching_times
# col_tim: 0.7658873  min_age: 0.304

## Cyanocompsa parellina*
cyanocompsapa_tree <- ape::read.nexus("trees/cardinalidae/cyanocompsapa_mcct.tre")

cyanocompsapa_tree <- as(cyanocompsapa_tree, "phylo4")
phylobase::plot(cyanocompsapa_tree, cex = 0.1)

# select individuals on the island and its status
cyanocompsapa_status <- data.frame(tip_labels = c("cyanocompsa_parellina_18_cam_MEX",
                                                  "cyanocompsa_parellina_19_qroo_MEX",
                                                  "cyanocompsa_parellina_8_cam_MEX",
                                                  "cyanocompsa_parellina_11_qroo_MEX",
                                                  "cyanocompsa_parellina_9_que_MEX"
),

tip_endemicity_status = c("nonendemic","nonendemic",
                          "nonendemic","nonendemic",
                          "nonendemic"))

# set the endemicity status in the tree
cyanocompsapa_endemicity <- create_endemicity_status(
  phylo =cyanocompsapa_tree,
  island_species =cyanocompsapa_status)

cyanocompsapa_phylo <- phylobase::phylo4d(cyanocompsapa_tree,cyanocompsapa_endemicity)

#to save in RDS format
saveRDS(cyanocompsapa_phylo, file = "trees/cardinalidae/cyanocompsapa_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
cyanocompsapa_phylod <- readRDS("trees/cardinalidae/cyanocompsapa_rds.rds")
cyanocompsapa_phylod_asr <- add_asr_node_states(phylod =cyanocompsapa_phylod, 
                                                asr_method = "mk", 
                                                tie_preference = "island", 
                                                earliest_col = FALSE)

cyanocompsapa_island_tbl <- extract_island_species(
  phylod =cyanocompsapa_phylod_asr, 
  extraction_method = "asr")

ars_plot(cyanocompsapa_phylod_asr, node_pies = TRUE)
cyanocompsapa_island_tbl
cyanocompsapa_island_tbl@island_tbl$branching_times
# 0.1595 - 0.3000

# Cyanoloxia cyanoides
cyanocompsacy_tree <- ape::read.nexus("trees/cardinalidae/cyanocompsacy_mcct.tre")

cyanocompsacy_tree <- as(cyanocompsacy_tree, "phylo4")
phylobase::plot(cyanocompsacy_tree, cex = 0.1)

# select individuals on the island and its status
cyanocompsacy_status <- data.frame(tip_labels = c("Cyanocompsa_cyanoides_45_BEL",
                                                  "Cyanocompsa_cyanoides_46_BEL",
                                                  "Cyanocompsa_cyanoides_47_BEL",
                                                  "Cyanocompsa_cyanoides_48_BEL",
                                                  "Cyanocompsa_cyanoides_49_BEL",
                                                  "Cyanocompsa_cyanoides_30_BEL",
                                                  "Cyanocompsa_cyanoides_17_BEL",
                                                  "Cyanocompsa_cyanoides_58_Chi_MEX",
                                                  "Cyanocompsa_cyanoides_40_tab_MEX",
                                                  "Cyanocompsa_cyanoides_41_ver_MEX",
                                                  "Cyanocompsa_cyanoides_42_ver_MEX",
                                                  "Cyanocompsa_cyanoides_22_oax_MEX",
                                                  "Cyanocompsa_cyanoides_2_HON",
                                                  "Cyanocompsa_cyanoides_18_PAN",
                                                  "Cyanocompsa_cyanoides_39_PAN",
                                                  "Cyanocompsa_cyanoides_78_CR",
                                                  "Cyanocompsa_cyanoides_8_NIC"
                                                  ),
                                   
                                   tip_endemicity_status = c("nonendemic","nonendemic",
                                                             "nonendemic","nonendemic",
                                                             "nonendemic","nonendemic",
                                                             "nonendemic","nonendemic",
                                                             "nonendemic","nonendemic",
                                                             "nonendemic","nonendemic",
                                                             "nonendemic",
                                                             "nonendemic","nonendemic",
                                                             "nonendemic",
                                                             "nonendemic"))

# set the endemicity status in the tree
cyanocompsacy_endemicity <- create_endemicity_status(
  phylo =cyanocompsacy_tree,
  island_species =cyanocompsacy_status)

cyanocompsacy_phylo <- phylobase::phylo4d(cyanocompsacy_tree,cyanocompsacy_endemicity)

#to save in RDS format
saveRDS(cyanocompsacy_phylo, file = "trees/cardinalidae/cyanocompsacy_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
cyanocompsacy_phylod <- readRDS("trees/cardinalidae/cyanocompsacy_rds.rds")
cyanocompsacy_phylod_asr <- add_asr_node_states(phylod =cyanocompsacy_phylod, 
                                                asr_method = "mk", 
                                                tie_preference = "island", 
                                                earliest_col = FALSE)

cyanocompsacy_island_tbl <- extract_island_species(
  phylod =cyanocompsacy_phylod_asr, 
  extraction_method = "asr")

ars_plot(cyanocompsacy_phylod_asr, node_pies = TRUE)
cyanocompsacy_island_tbl
cyanocompsacy_island_tbl@island_tbl$branching_times
#  Non_endemic_MaxAge_MinAge	0.20,0.03

# Habia fuscicauda
# Habia rubica
habiasp_tree <- ape::read.nexus("trees/cardinalidae/hfusc_div_time_daisie.tre")
which(habiasp_tree$edge.length < 0)
# 19 29
habiasp_tree$edge.length[62] <- 0.0
habiasp_tree$edge.length[63] <- 0.0
habiasp_tree$edge.length[83] <- 0.0

habiasp_tree <- as(habiasp_tree, "phylo4")
phylobase::plot(habiasp_tree, cex = 0.1)

# select individuals on the island and its status
habiasp_status <- data.frame(tip_labels = c(#"Habia_fuscicauda_QR_14",
                                            #"Habia_fuscicauda_Cam_10",
                                            #"Habia_fuscicauda_QR_20",
                                            #"Habia_fuscicauda_TaB_05",
                                            #"Habia_fuscicauda_Cam_13",
                                            #"Habia_fuscicauda_TaB_04",
                                            #"Habia_fuscicauda_TaB_07",
                                            #"Habia_fuscicauda_TaB_02",
                                            #"Habia_fuscicauda_QR_21",
                                            #"Habia_fuscicauda_Chi_09",
                                            #"Habia_fuscicauda_Chi_08",
                                            #"Habia_fuscicauda_Cam_09",
                                            #"Habia_fuscicauda_Cam_14",
                                            #"Habia_fuscicauda_QR_09",
                                            #"Habia_fuscicauda_SLP_01",
                                             # "Habia_fuscicauda_QR_12",
                                            #"Habia_fuscicauda_QR_07",
                                            "Habia_rubica_MX_Cam_H_rubica",
                                            "Habia_fuscicauda_Cam_11",
                                            "Habia_fuscicauda_Cam_12"),
                             
                             tip_endemicity_status = c(#"nonendemic","nonendemic",
                                                       #"nonendemic",
                                                       #"nonendemic",
                                                       #"nonendemic","nonendemic",
                                                       #"nonendemic",
                                                       #"nonendemic","nonendemic",
                                                       #"nonendemic",
                                                       #"nonendemic",
                                                       #"nonendemic","nonendemic",
                                                       #"nonendemic","nonendemic",
                                                       #"nonendemic",
                                                      "nonendemic",
                                                       "nonendemic", "nonendemic"))

# set the endemicity status in the tree
habiasp_endemicity <- create_endemicity_status(
  phylo = habiasp_tree,
  island_species = habiasp_status)

habiasp_phylo <- phylobase::phylo4d(habiasp_tree, habiasp_endemicity)
habiasp_phylo@edge.length[1]<- 0.0

#to save in RDS format
saveRDS(habiasp_phylo, file = "trees/cardinalidae/habiap_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
habiap_phylod <- readRDS("trees/cardinalidae/habiap_rds.rds")
habiap_phylod_asr <- add_asr_node_states(phylod = habiap_phylod, 
                                         asr_method = "mk", 
                                         tie_preference = "island", 
                                         earliest_col = FALSE)

habiap_island_tbl <- extract_island_species(
  phylod = habiap_phylod_asr, 
  extraction_method = "asr")

ars_plot(habiap_phylod_asr, node_pies = TRUE)
habiap_island_tbl
habiap_island_tbl@island_tbl$branching_times

# Habia fuscicauda Non_endemic_MaxAge_MinAge   2.12,1.74
# Habia rubica     Non_endemic 0.04	

# Piranga roseogularis
piranga_dt_tree <- ape::read.nexus("trees/cardinalidae/piranga_mcct.tree")
piranga_dt_tree <- as(piranga_dt_tree, "phylo4")
phylobase::plot(piranga_dt_tree, cex = 0.1)

# select individuals on the island and its status
piranga_dt_status <- data.frame(tip_labels = c("Piranga_roseogularis_QRo","Piranga_roseogularis_QRo2"),
                                tip_endemicity_status = c("endemic", "endemic"))

# set the endemicity status in the tree
piranga_dt_endemicity <- create_endemicity_status(
  phylo = piranga_dt_tree,
  island_species = piranga_dt_status)

piranga_dt_phylo <- phylobase::phylo4d(piranga_dt_tree, piranga_dt_endemicity)

#to save in RDS format
saveRDS(piranga_dt_phylo, file = "trees/cardinalidae/piranga_dt_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
piranga_phylod <- readRDS("trees/cardinalidae/piranga_dt_rds.rds")
piranga_phylod_asr <- add_asr_node_states(phylod = piranga_phylod, 
                                          asr_method = "mk", 
                                          tie_preference = "island", 
                                          earliest_col = FALSE)

piranga_island_tbl <- extract_island_species(
  phylod = piranga_phylod_asr, 
  extraction_method = "asr")

ars_plot(piranga_phylod_asr, node_pies = TRUE)
piranga_island_tbl
piranga_island_tbl@island_tbl$branching_times

# Corvidae
# cyanocorax yucatanicus
cyanocorax_tree <- ape::read.nexus("trees/corvidae/cyanocorax_mcct.tree")
cyanocorax_tree <- as(cyanocorax_tree, "phylo4")
phylobase::plot(cyanocorax_tree, cex = 0.1)

# select individuals on the island and its status
cyanocorax_status <- data.frame(tip_labels = c("Cyanocorax_yucatanicus_DQ912613_Yucatan",
                                               "Cyanocorax_yucatanicus_GU144848_Campeche"),
                                tip_endemicity_status = c("endemic", "endemic"))

# set the endemicity status in the tree
cyanocorax_endemicity <- create_endemicity_status(
  phylo = cyanocorax_tree,
  island_species = cyanocorax_status)

cyanocorax_phylo <- phylobase::phylo4d(cyanocorax_tree, cyanocorax_endemicity)

#to save in RDS format
saveRDS(cyanocorax_phylo, file = "trees/corvidae/cyanocorax_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
cyanocorax_phylod <- readRDS("trees/corvidae/cyanocorax_rds.rds")
cyanocorax_phylod_asr <- add_asr_node_states(phylod = cyanocorax_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

cyanocorax_island_tbl <- extract_island_species(
  phylod = cyanocorax_phylod_asr, 
  extraction_method = "min")

ars_plot(cyanocorax_phylod_asr, node_pies = TRUE)
cyanocorax_island_tbl
cyanocorax_island_tbl@island_tbl$branching_times
# branching_times 2.29  min_age 0.29

#Fringillidae
# Euphonia affinis
euphonia_tree <- ape::read.nexus("trees/fringillidae/euphonia_mcct.tree")
which(euphonia_tree$edge.length < 0)
# 6 12
euphonia_tree$edge.length[6] <- 0
euphonia_tree$edge.length[12] <- 0
euphonia_tree <- as(euphonia_tree, "phylo4")
phylobase::plot(euphonia_tree, cex = 0.1)

# select individuals on the island and its status
euphonia_status <- data.frame(tip_labels = c(
                                             "Euphonia_affinis_MT452166_Yucatan_Mexico",
                                            "Euphonia_affinis_MT452165_Yucatan_Mexico",
                                             "Euphonia_affinis_MT452155_Campeche_Mexico",
                                             "Euphonia_affinis_MT452148_Campeche_Mexico"
                                             ,
                                             "Euphonia_affinis_MT452163_Veracruz_Mexico"
),
tip_endemicity_status = c("nonendemic", "nonendemic",
                          "nonendemic", "nonendemic", 
                          "nonendemic"))

# set the endemicity status in the tree
euphonia_endemicity <- create_endemicity_status(
  phylo = euphonia_tree,
  island_species = euphonia_status)

euphonia_phylo <- phylobase::phylo4d(euphonia_tree, euphonia_endemicity)

#to save in RDS format
saveRDS(euphonia_phylo, file = "trees/fringillidae/euphonia_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
euphonia_phylod <- readRDS("trees/fringillidae/euphonia_rds.rds")
euphonia_phylod_asr <- add_asr_node_states(phylod = euphonia_phylod, 
                                           asr_method = "mk", 
                                           tie_preference = "island", 
                                           earliest_col = FALSE)

euphonia_island_tbl <- extract_island_species(
  phylod = euphonia_phylod_asr, 
  extraction_method = "asr")

ars_plot(euphonia_phylod_asr, node_pies = TRUE)
euphonia_island_tbl
euphonia_island_tbl@island_tbl$branching_times
# col_time = 0.33    min_age 0.20

# Euphonia Gouldi
euphoniag_tree <- ape::read.nexus("trees/fringillidae/euphoniag_mcct.tre")
euphoniag_tree <- as(euphoniag_tree, "phylo4")
phylobase::plot(euphoniag_tree, cex = 0.1)

# select individuals on the island and its status
euphoniag_status <- data.frame(tip_labels = c(#"Euphonia_gouldi_Veracruz_FJ231671",
  #"Euphonia_gouldi_Veracruz_FJ231672",
  #"Euphonia_gouldi_Veracruz_FJ231673",
  #"Euphonia_gouldi_Veracruz_FJ231674",
  #"Euphonia_gouldi_Veracruz_FJ231675",
  #"Euphonia_gouldi_Veracruz_FJ231676",
  "Euphonia_gouldi_Belize_FJ231677",
  "Euphonia_gouldi_Belize_FJ231678",
  "Euphonia_gouldi_Belize_FJ231679",
  "Euphonia_gouldi_Belize_FJ231680",
  "Euphonia_gouldi_Belize_FJ231681",
  "Euphonia_gouldi_Belize_FJ231682",
  "Euphonia_gouldi_Belize_FJ231683",
  "Euphonia_gouldi_Belize_FJ231684",
  "Euphonia_gouldi_Belize_FJ231685"
  ,
  "Euphonia_gouldi_Honduras_FJ231686",
  #"Euphonia_gouldi_Honduras_FJ231689",
  #"Euphonia_gouldi_Honduras_FJ231691",
  "Euphonia_gouldi_Honduras_FJ231694"
),

tip_endemicity_status = c(#"nonendemic", "nonendemic",
  #"nonendemic", "nonendemic",
  #"nonendemic", "nonendemic",
  #"nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic"))

# set the endemicity status in the tree
euphoniag_endemicity <- create_endemicity_status(
  phylo =euphoniag_tree,
  island_species =euphoniag_status)

euphoniag_phylo <- phylobase::phylo4d(euphoniag_tree,euphoniag_endemicity)

#to save in RDS format
saveRDS(euphoniag_phylo, file = "trees/fringillidae/euphoniag_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
euphoniag_phylod <- readRDS("trees/fringillidae/euphoniag_rds.rds")
euphoniag_phylod_asr <- add_asr_node_states(phylod =euphoniag_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

euphoniag_island_tbl <- extract_island_species(
  phylod =euphoniag_phylod_asr, 
  extraction_method = "asr")

ars_plot(euphoniag_phylod_asr, node_pies = TRUE)
euphoniag_island_tbl
euphoniag_island_tbl@island_tbl$col_time
euphoniag_island_tbl@island_tbl$min_age
# branching_times 0.14  min_age 0.11    

# Family Furnariidae
## Automolus
automolus_tree <- ape::read.nexus("trees/furnariidae/automolus_mcct.tre")

automolus_tree <- as(automolus_tree, "phylo4")
phylobase::plot(automolus_tree, cex = 0.1)

# select individuals on the island and its status
automolus_status <- data.frame(tip_labels = c( "Automolus_ochrolaemus_KM079756_Belize",
  "Automolus_ochrolaemus_KM079755_Belize"
  # ,
  #"Automolus_ochrolaemus_KM079858_Ver_Mexico",
  #"Automolus_ochrolaemus_KM079859_Ver_Mexico",
  # "Automolus_ochrolaemus_KM079859_Chi_Mexico",
  #"Automolus_ochrolaemus_KM079857_Chi_Mexico",
   # "Automolus_ochrolaemus_KM079861_Oax_Mexico"
  #"Automolus_ochrolaemus_KM079856_Chi_Mexico"
),

tip_endemicity_status = c(#"nonendemic" , "nonendemic",
                          # "nonendemic", "nonendemic",
                           "nonendemic", #"nonendemic",
                           "nonendemic"#, "nonendemic"
))

# set the endemicity status in the tree
automolus_endemicity <- create_endemicity_status(
  phylo =automolus_tree,
  island_species =automolus_status)

automolus_phylo <- phylobase::phylo4d(automolus_tree,automolus_endemicity)

#to save in RDS format
saveRDS(automolus_phylo, file = "trees/furnariidae/automolus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
automolus_phylod <- readRDS("trees/furnariidae/automolus_rds.rds")
automolus_phylod_asr <- add_asr_node_states(phylod =automolus_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

automolus_island_tbl <- extract_island_species(
  phylod =automolus_phylod_asr, 
  extraction_method = "asr")

ars_plot(automolus_phylod_asr, node_pies = TRUE)
automolus_island_tbl
automolus_island_tbl@island_tbl$branching_times
#    min_age 0.023 max_age 0.012

## Dendrocincla homochroa
dendrocincla_tree <- ape::read.nexus("trees/furnariidae/dendrocincla_mcct.tre")

dendrocincla_tree <- as(dendrocincla_tree, "phylo4")
phylobase::plot(dendrocincla_tree, cex = 0.1)

# select individuals on the island and its status
dendrocincla_status <- data.frame(tip_labels = c("Dendrocincla_homochroa_FJ175824_Belize",
                                                 "Dendrocincla_homochroa_FJ175823_Belize",
                                                 "Dendrocincla_homochroa_FJ175822_Belize",
                                                 "Dendrocincla_homochroa_GU215376_El_Salvador",
                                                 "Dendrocincla_anabatina_JF975335"
),

tip_endemicity_status = c("nonendemic", "nonendemic",
                          "nonendemic", "nonendemic",
                          "nonendemic"))

# set the endemicity status in the tree
dendrocincla_endemicity <- create_endemicity_status(
  phylo =dendrocincla_tree,
  island_species =dendrocincla_status)

dendrocincla_phylo <- phylobase::phylo4d(dendrocincla_tree,dendrocincla_endemicity)

#to save in RDS format
saveRDS(dendrocincla_phylo, file = "trees/furnariidae/dendrocincla_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
dendrocincla_phylod <- readRDS("trees/furnariidae/dendrocincla_rds.rds")
dendrocincla_phylod_asr <- add_asr_node_states(phylod =dendrocincla_phylod, 
                                               asr_method = "mk", 
                                               tie_preference = "island", 
                                               earliest_col = FALSE)

dendrocincla_island_tbl <- extract_island_species(
  phylod =dendrocincla_phylod_asr, 
  extraction_method = "asr")

ars_plot(dendrocincla_phylod_asr, node_pies = TRUE)
dendrocincla_island_tbl
dendrocincla_island_tbl@island_tbl$min_age
dendrocincla_island_tbl@island_tbl$col_time
# Dendrocincla_anabatina col_time 2.68 
# Dendrocincla_homochroa  col_time 0.215   min_age 0.149

# Xenops minutus
xenops_tree <- ape::read.nexus("trees/furnariidae/xenops_mcct.tre")
xenops_tree <- as(xenops_tree, "phylo4")
phylobase::plot(xenops_tree, cex = 0.1)

# select individuals on the island and its status
xenops_status <- data.frame(tip_labels = c("Xenops_minutus_FJ175807_Bel",
                                           "Xenops_minutus_FJ175806_Bel",
                                           "Xenops_minutus_FJ175805_Bel",
                                           "Xenops_minutus_FJ175804_Bel",
                                           "Xenops_minutus_FJ175803_Bel",
                                           "Xenops_minutus_FJ175802_Bel",
                                           "Xenops_minutus_FJ175801_Bel"),
                            
                            tip_endemicity_status = c("nonendemic", "nonendemic",
                                                      "nonendemic", "nonendemic",
                                                      "nonendemic", "nonendemic",
                                                      "nonendemic"))

# set the endemicity status in the tree
xenops_endemicity <- create_endemicity_status(
  phylo =xenops_tree,
  island_species =xenops_status)

xenops_phylo <- phylobase::phylo4d(xenops_tree,xenops_endemicity)

#to save in RDS format
saveRDS(xenops_phylo, file = "trees/furnariidae/xenops_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
xenops_phylod <- readRDS("trees/furnariidae/xenops_rds.rds")
xenops_phylod_asr <- add_asr_node_states(phylod =xenops_phylod, 
                                         asr_method = "mk", 
                                         tie_preference = "island", 
                                         earliest_col = FALSE)

xenops_island_tbl <- extract_island_species(
  phylod =xenops_phylod_asr, 
  extraction_method = "asr")

ars_plot(xenops_phylod_asr, node_pies = TRUE)
xenops_island_tbl
xenops_island_tbl@island_tbl$branching_times
# min_age 0.114853 col_time 0.9856513

# Xiphorhynchus flavigaster
xiphorhynchus_tree <- ape::read.nexus("trees/furnariidae/xiphorhynchus_mcct.tre")

xiphorhynchus_tree <- as(xiphorhynchus_tree, "phylo4")
phylobase::plot(xiphorhynchus_tree, cex = 0.1)

# select individuals on the island and its status
xiphorhynchus_status <- data.frame(tip_labels = c("Xiphorhynchus_flavigaster_MW385328_Cam",
                                                  "Xiphorhynchus_flavigaster_MW385329_Cam",
                                                  "Xiphorhynchus_flavigaster_MW385327_Cam",
                                                  "Xiphorhynchus_flavigaster_MW385326_Yuc",
                                                  "Xiphorhynchus_flavigaster_MW385325_Yuc",
                                                  #"Xiphorhynchus_flavigaster_MW385332_Chi",
                                                  #"Xiphorhynchus_flavigaster_MW385331_Chi",
                                                  #"Xiphorhynchus_flavigaster_MW385365_ver",
                                                  #"Xiphorhynchus_flavigaster_MW385353_Oax",
                                                  "Xiphorhynchus_flavigaster_MW385315_Sal"
                                                  #"Xiphorhynchus_flavigaster_MW385366_ver"
),

tip_endemicity_status = c(#"nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic",# "nonendemic",
  #"nonendemic", 
  "nonendemic"))

# set the endemicity status in the tree
xiphorhynchus_endemicity <- create_endemicity_status(
  phylo =xiphorhynchus_tree,
  island_species =xiphorhynchus_status)

xiphorhynchus_phylo <- phylobase::phylo4d(xiphorhynchus_tree,xiphorhynchus_endemicity)

#to save in RDS format
saveRDS(xiphorhynchus_phylo, file = "trees/furnariidae/xiphorhynchus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
xiphorhynchus_phylod <- readRDS("trees/furnariidae/xiphorhynchus_rds.rds")
xiphorhynchus_phylod_asr <- add_asr_node_states(phylod =xiphorhynchus_phylod, 
                                                asr_method = "mk", 
                                                tie_preference = "island", 
                                                earliest_col = FALSE)

xiphorhynchus_island_tbl <- extract_island_species(
  phylod =xiphorhynchus_phylod_asr, 
  extraction_method = "asr")

ars_plot(xiphorhynchus_phylod_asr, node_pies = TRUE)
xiphorhynchus_island_tbl
xiphorhynchus_island_tbl@island_tbl$branching_times
# min_age 0.03  max_age 0.15

# Dendrocolaptes sanctithomae
dendrocolaptes_tree <- ape::read.nexus("trees/furnariidae/dendrocolaptes_mcct.tre")
dendrocolaptes_tree <- as(dendrocolaptes_tree, "phylo4")
phylobase::plot(dendrocolaptes_tree, cex = 0.1)

# select individuals on the island and its status
dendrocolaptes_status <- data.frame(tip_labels = c("Dendrocolaptes_sanctithomae_FJ175850_Belize",
                                                   "Dendrocolaptes_sanctithomae_FJ175849_Belize"
                                                   #,
                                                   #"Dendrocolaptes_sanctithomae_KC815117"
),

tip_endemicity_status = c("nonendemic","nonendemic"
                          
                          #,"nonendemic"
))

# set the endemicity status in the tree
dendrocolaptes_endemicity <- create_endemicity_status(
  phylo =dendrocolaptes_tree,
  island_species =dendrocolaptes_status)

dendrocolaptes_phylo <- phylobase::phylo4d(dendrocolaptes_tree,dendrocolaptes_endemicity)

#to save in RDS format
saveRDS(dendrocolaptes_phylo, file = "trees/furnariidae/dendrocolaptes_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
dendrocolaptes_phylod <- readRDS("trees/furnariidae/dendrocolaptes_rds.rds")
dendrocolaptes_phylod_asr <- add_asr_node_states(phylod =dendrocolaptes_phylod, 
                                                 asr_method = "mk", 
                                                 tie_preference = "island", 
                                                 earliest_col = FALSE)

dendrocolaptes_island_tbl <- extract_island_species(
  phylod =dendrocolaptes_phylod_asr, 
  extraction_method = "asr")

ars_plot(dendrocolaptes_phylod_asr, node_pies = TRUE)
dendrocolaptes_island_tbl
dendrocolaptes_island_tbl@island_tbl$col_time
dendrocolaptes_island_tbl@island_tbl$min_age
# min_age 0.07174229 col_time 0.1172948

## Sclerurus guatemalensis
sclerurus_tree <- ape::read.nexus("trees/furnariidae/sclerurus_mcct.tre")
sclerurus_tree <- as(sclerurus_tree, "phylo4")
phylobase::plot(sclerurus_tree, cex = 0.1)

# select individuals on the island and its status
sclerurus_status <- data.frame(tip_labels = c("Sclerurus_guatemalensis_FJ175818_Belize",
                                              "Sclerurus_guatemalensis_JQ903758_Honduras",
                                              "Sclerurus_guatemalensis_JQ903757_Belize"
                                              #,
                                              #"Sclerurus_guatemalensis_JQ903756_Mexico"
),

tip_endemicity_status = c(#"nonendemic",
  "nonendemic",
  "nonendemic",
  "nonendemic"))

# set the endemicity status in the tree
sclerurus_endemicity <- create_endemicity_status(
  phylo =sclerurus_tree,
  island_species =sclerurus_status)

sclerurus_phylo <- phylobase::phylo4d(sclerurus_tree,sclerurus_endemicity)

#to save in RDS format
saveRDS(sclerurus_phylo, file = "trees/furnariidae/sclerurus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
sclerurus_phylod <- readRDS("trees/furnariidae/sclerurus_rds.rds")
sclerurus_phylod_asr <- add_asr_node_states(phylod =sclerurus_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

sclerurus_island_tbl <- extract_island_species(
  phylod =sclerurus_phylod_asr, 
  extraction_method = "asr")

ars_plot(sclerurus_phylod_asr, node_pies = TRUE)
sclerurus_island_tbl
sclerurus_island_tbl@island_tbl$min_age
# min_age 0.0446 col_time 0.2178

# Sittasomus griseicapillus
sittasomus_tree <- ape::read.nexus("trees/furnariidae/sittasomus_mcct.tre")
sittasomus_tree <- as(sittasomus_tree, "phylo4")
phylobase::plot(sittasomus_tree, cex = 0.1)

# select individuals on the island and its status
sittasomus_status <- data.frame(tip_labels = c("Sittasomus_griseicapillus_KF705459_Argentina"
),

tip_endemicity_status = c("nonendemic"))

# set the endemicity status in the tree
sittasomus_endemicity <- create_endemicity_status(
  phylo =sittasomus_tree,
  island_species =sittasomus_status)

sittasomus_phylo <- phylobase::phylo4d(sittasomus_tree,sittasomus_endemicity)

#to save in RDS format
saveRDS(sittasomus_phylo, file = "trees/furnariidae/sittasomus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
sittasomus_phylod <- readRDS("trees/furnariidae/sittasomus_rds.rds")
sittasomus_phylod_asr <- add_asr_node_states(phylod =sittasomus_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

sittasomus_island_tbl <- extract_island_species(
  phylod =sittasomus_phylod_asr, 
  extraction_method = "asr")

ars_plot(sittasomus_phylod_asr, node_pies = TRUE)
sittasomus_island_tbl
sittasomus_island_tbl@island_tbl$col_time
# max_age 1.24 

## lepidocolaptes
lepidocolaptes_tree <- ape::read.nexus("trees/furnariidae/lepidocolaptes_mcct.tre")


lepidocolaptes_tree <- as(lepidocolaptes_tree, "phylo4")
phylobase::plot(lepidocolaptes_tree, cex = 0.1)

# select individuals on the island and its status
lepidocolaptes_status <- data.frame(tip_labels = c("Lepidocolaptes_souleyetii_JQ829003_Nicaragua",
                                                   "Lepidocolaptes_souleyetii_GU215193_Nicaragua",
                                                   "Lepidocolaptes_souleyetii_JQ829002_Chi_Mex",
                                                   "Lepidocolaptes_souleyetii_JQ829001_Oax_Mex"
),

tip_endemicity_status = c("nonendemic", "nonendemic",
                          "nonendemic", "nonendemic"))

# set the endemicity status in the tree
lepidocolaptes_endemicity <- create_endemicity_status(
  phylo =lepidocolaptes_tree,
  island_species =lepidocolaptes_status)

lepidocolaptes_phylo <- phylobase::phylo4d(lepidocolaptes_tree,lepidocolaptes_endemicity)

#to save in RDS format
saveRDS(lepidocolaptes_phylo, file = "trees/furnariidae/lepidocolaptes_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
lepidocolaptes_phylod <- readRDS("trees/furnariidae/lepidocolaptes_rds.rds")
lepidocolaptes_phylod_asr <- add_asr_node_states(phylod =lepidocolaptes_phylod, 
                                                 asr_method = "mk", 
                                                 tie_preference = "island", 
                                                 earliest_col = FALSE)

lepidocolaptes_island_tbl <- extract_island_species(
  phylod =lepidocolaptes_phylod_asr, 
  extraction_method = "asr")

ars_plot(lepidocolaptes_phylod_asr, node_pies = TRUE)
lepidocolaptes_island_tbl
lepidocolaptes_island_tbl@island_tbl$branching_times
# Max_age 1.55

# Hirundinidae
# Petrochelidon fulva
petrochelydon_tree <- ape::read.nexus("trees/hirundinidae/petrochelydon_mcct.tree")
petrochelydon_tree <- as(petrochelydon_tree, "phylo4")
phylobase::plot(petrochelydon_tree, cex = 0.1)

# select individuals on the island and its status
petrochelydon_status <- data.frame(tip_labels = c("Petrochelidon_fulva_Yucatan",
                                                  "Petrochelidon_fulva_Yucatan2"),
                                   tip_endemicity_status = c("nonendemic", "nonendemic"))

# set the endemicity status in the tree
petrochelydon_endemicity <- create_endemicity_status(
  phylo = petrochelydon_tree,
  island_species = petrochelydon_status)

petrochelydon_phylo <- phylobase::phylo4d(petrochelydon_tree, petrochelydon_endemicity)

#to save in RDS format
saveRDS(petrochelydon_phylo, file = "trees/hirundinidae/petrochelydon_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
petrochelydon_phylod <- readRDS("trees/hirundinidae/petrochelydon_rds.rds")
petrochelydon_phylod_asr <- add_asr_node_states(phylod = petrochelydon_phylod, 
                                                asr_method = "mk", 
                                                tie_preference = "island", 
                                                earliest_col = FALSE)

petrochelydon_island_tbl <- extract_island_species(
  phylod = petrochelydon_phylod_asr, 
  extraction_method = "asr")

ars_plot(petrochelydon_phylod_asr, node_pies = TRUE)
petrochelydon_island_tbl
petrochelydon_island_tbl@island_tbl$branching_times
# branching_times 0.12 min_age 0.024

# Tachycineta albilinea
tachycineta_tree <- ape::read.nexus("trees/hirundinidae/tachycineta_mcct.tree")
tachycineta_tree <- as(tachycineta_tree, "phylo4")
phylobase::plot(tachycineta_tree, cex = 0.1)

# select individuals on the island and its status
tachycineta_status <- data.frame(tip_labels = c("Tachycineta_albilinea_Belice"),
                                 tip_endemicity_status = c("nonendemic"))

# set the endemicity status in the tree
tachycineta_endemicity <- create_endemicity_status(
  phylo = tachycineta_tree,
  island_species = tachycineta_status)

tachycineta_phylo <- phylobase::phylo4d(tachycineta_tree, tachycineta_endemicity)

#to save in RDS format
saveRDS(tachycineta_phylo, file = "trees/hirundinidae/tachycineta_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
Tachycineta_phylod <- readRDS("trees/hirundinidae/Tachycineta_rds.rds")
Tachycineta_phylod_asr <- add_asr_node_states(phylod = Tachycineta_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

Tachycineta_island_tbl <- extract_island_species(
  phylod = Tachycineta_phylod_asr, 
  extraction_method = "asr")

ars_plot(Tachycineta_phylod_asr, node_pies = TRUE)
Tachycineta_island_tbl
Tachycineta_island_tbl@island_tbl$branching_times
# branching_times 0.660

# Icteridae
# Amblycercus holosericeus
amblycereus_tree <- ape::read.nexus("trees/icteridae/amblycereus_mcct.tree")
amblycereus_tree <- as(amblycereus_tree, "phylo4")
phylobase::plot(amblycereus_tree, cex = 0.1)

# select individuals on the island and its status
amblycereus_status <- data.frame(tip_labels = c("Amblycereus_holosericeus_AY117722_Yucatan",
                                                "Amblycereus_holosericeus_AY117723_Yucatan"
),
tip_endemicity_status = c("nonendemic","nonendemic"))

# set the endemicity status in the tree
amblycereus_endemicity <- create_endemicity_status(
  phylo = amblycereus_tree,
  island_species = amblycereus_status)

amblycereus_phylo <- phylobase::phylo4d(amblycereus_tree, amblycereus_endemicity)

#to save in RDS format
saveRDS(amblycereus_phylo, file = "trees/icteridae/amblycereus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
amblycereus_phylod <- readRDS("trees/icteridae/amblycereus_rds.rds")
amblycereus_phylod_asr <- add_asr_node_states(phylod = amblycereus_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

amblycereus_island_tbl <- extract_island_species(
  phylod = amblycereus_phylod_asr, 
  extraction_method = "min")

ars_plot(amblycereus_phylod_asr, node_pies = TRUE)
amblycereus_island_tbl
amblycereus_island_tbl@island_tbl$branching_times
# col_time 2.48  min_age 0.06

# Icterus chrysater
chrysaster_tree <- ape::read.nexus("trees/icteridae/chrysater_mcct.tree")
chrysaster_tree <- as(chrysaster_tree, "phylo4")

which(chrysaster_tree$edge.length < 0)
# 8
chrysaster_tree$edge.length[8] <- 0
phylobase::plot(chrysaster_tree, cex = 0.5)

# select individuals on the island and its status
chrysaster_status <- data.frame(tip_labels = c(
  "Icterus_chrysater_KF676926_Yucatan_Mexico",
  "Icterus_chrysater_KF676927_Yucatan_Mexico",
  "Icterus_chrysater_KF676928_Yucatan_Mexico",
  "Icterus_chrysater_KF676929_Yucatan_Mexico",
  "Icterus_chrysater_KF676930_Yucatan_Mexico", 
  "Icterus_chrysater_KF676936_Yucatan_Mexico",
  "Icterus_chrysater_KF676940_El_Salvador",
  "Icterus_chrysater_KF676942_Chiapas_Mexico", #
  "Icterus_chrysater_KF676944_Yucatan_Mexico"),
  tip_endemicity_status = c("nonendemic", "nonendemic",
                            "nonendemic", "nonendemic", 
                            "nonendemic",
                            "nonendemic", "nonendemic", "nonendemic",
                            "nonendemic"
  ))

# set the endemicity status in the tree
chrysaster_endemicity <- create_endemicity_status(
  phylo = chrysaster_tree,
  island_species = chrysaster_status)

chrysaster_phylo <- phylobase::phylo4d(chrysaster_tree, chrysaster_endemicity)

#to save in RDS format
saveRDS(chrysaster_phylo, file = "trees/icteridae/chrysaster_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
chrysaster_phylod <- readRDS("trees/icteridae/chrysaster_rds.rds")
chrysaster_phylod_asr <- add_asr_node_states(phylod = chrysaster_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

chrysaster_island_tbl <- extract_island_species(
  phylod = chrysaster_phylod_asr, 
  extraction_method = "asr")

ars_plot(chrysaster_phylod_asr, node_pies = TRUE)
chrysaster_island_tbl
chrysaster_island_tbl@island_tbl$branching_times
#  Max_time 0.06	min_age 0.007

# Icterus fuertesi
fuertesi_tree <- ape::read.nexus("trees/icteridae/fuertesi_mcct.tre")
fuertesi_tree <- as(fuertesi_tree, "phylo4")

phylobase::plot(fuertesi_tree, cex = 0.5)

# select individuals on the island and its status
fuertesi_status <- data.frame(tip_labels = c( #"Icterus_prosthemelas_AY211213_Yucatan",
  #"Icterus_prosthemelas_AY211212_Campeche",
  "Icterus_fuertesi_AY211198_USA",
  "Icterus_fuertesi_AY211215_Veracruz",
  "Icterus_fuertesi_AY211214_Veracruz",
  "Icterus_fuertesi_AY211209_Veracruz",
  "Icterus_fuertesi_AY211208_Veracruz",
  "Icterus_fuertesi_AY211206_Veracruz",
  "Icterus_fuertesi_AY212245_Veracruz"
),

tip_endemicity_status = c(#"nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic"
))

# set the endemicity status in the tree
fuertesi_endemicity <- create_endemicity_status(
  phylo = fuertesi_tree,
  island_species = fuertesi_status)

fuertesi_phylo <- phylobase::phylo4d(fuertesi_tree, fuertesi_endemicity)

#to save in RDS format
saveRDS(fuertesi_phylo, file = "trees/icteridae/fuertesi_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
fuertesi_phylod <- readRDS("trees/icteridae/fuertesi_rds.rds")
fuertesi_phylod_asr <- add_asr_node_states(phylod = fuertesi_phylod, 
                                           asr_method = "mk", 
                                           tie_preference = "island", 
                                           earliest_col = FALSE)

fuertesi_island_tbl <- extract_island_species(
  phylod = fuertesi_phylod_asr, 
  extraction_method = "asr")

ars_plot(fuertesi_phylod_asr, node_pies = TRUE)
fuertesi_island_tbl
fuertesi_island_tbl@island_tbl$branching_times
# Icterus_prosthemelas col_time 2.6  min_age 0.07

# Icterus gularis (Icterus genus)
icterus_genus_tree <- ape::read.nexus("trees/icteridae/icterus_genus_mcct.tree")
icterus_genus_tree <- as(icterus_genus_tree, "phylo4")


which(icterus_genus_tree $edge.length < 0)
# 66
icterus_genus_tree$edge.length[66] <- 0

phylobase::plot(icterus_genus_tree, cex = 0.5)

# select individuals on the island and its status
icterus_genus_status <- data.frame(tip_labels = c("Icterus_auratus_AF099312_Yucatan_Mexico",
                                                  "Icterus_auratus_MN380342_Yucatan_Mexico",
                                                  "Icterus_cucullatus_AF099322_Campeche_Mexico",
                                                  "Icterus_dominicensis_prosthemelas_AF099327_Campeche_Mexico",
                                                  "Icterus_dominicensis_AF109449_Mexico",
                                                  "Icterus_dominicensis_AF109448_Mexico",
                                                  "Icterus_gularis_MN380375_Yucatan_Mexico",
                                                  "Icterus_gularis_MN380381_Campeche_Mexico",
                                                  "Icterus_gularis_MN380382_Yucatan_Mexico",
                                                  "Icterus_gularis_MN380385_Yucatan_Mexico",
                                                  "Icterus_gularis_MN380386_Yucatan_Mexico",
                                                  "Icterus_gularis_MN380389_Yucatan_Mexico",
                                                  "Icterus_gularis_MN380390_Campeche_Mexico",
                                                  "Icterus_gularis_MN380391_Campeche_Mexico",
                                                  "Icterus_gularis_MN380392_Yucatan_Mexico",
                                                  "Icterus_gularis_MN380393_Campeche_Mexico"
),
tip_endemicity_status = c( "endemic", "endemic",
                           "nonendemic","nonendemic",
                           "nonendemic","nonendemic",
                           "nonendemic",
                           "nonendemic","nonendemic",
                           "nonendemic","nonendemic",
                           "nonendemic","nonendemic",
                           "nonendemic","nonendemic",
                           "nonendemic"
))

# set the endemicity status in the tree
icterus_genus_endemicity <- create_endemicity_status(
  phylo = icterus_genus_tree,
  island_species = icterus_genus_status)

icterus_genus_phylo <- phylobase::phylo4d(icterus_genus_tree, icterus_genus_endemicity)

#to save in RDS format
saveRDS(icterus_genus_phylo, file = "trees/icteridae/icterus_genus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
icterus_genus_phylod <- readRDS("trees/icteridae/icterus_genus_rds.rds")
icterus_genus_phylod_asr <- add_asr_node_states(phylod = icterus_genus_phylod, 
                                                asr_method = "mk", 
                                                tie_preference = "island", 
                                                earliest_col = FALSE)

icterus_genus_island_tbl <- extract_island_species(
  phylod = icterus_genus_phylod_asr, 
  extraction_method = "asr")

ars_plot(icterus_genus_phylod_asr, node_pies = TRUE)
icterus_genus_island_tbl
icterus_genus_island_tbl@island_tbl$branching_times
# Icterus auratus col_time 2.037   min_age 0.02
# Icterus_gularis  col_time 0.12   min_age 0.08

# Icterus gularis
icgularis_tree <- ape::read.nexus("trees/icteridae/icgularis_mcct.tre")
icgularis_tree <- as(icgularis_tree, "phylo4")
phylobase::plot(icgularis_tree, cex = 0.5)

# select individuals on the island and its status
icgularis_status <- data.frame(tip_labels = c( "Icterus_gularis_MN380391_Campeche_Mexico",
                                               "Icterus_gularis_MN380390_Campeche_Mexico",
                                               "Icterus_gularis_MN380393_Campeche_Mexico",
                                               "Icterus_gularis_MN380389_Yucatan_Mexico",
                                               "Icterus_gularis_MN380392_Yucatan_Mexico",
                                               "Icterus_gularis_MN380375_Yucatan_Mexico",
                                               "Icterus_gularis_MN380382_Yucatan_Mexico",
                                               "Icterus_gularis_MN380386_Yucatan_Mexico",
                                               "Icterus_gularis_MN380385_Yucatan_Mexico",
                                               "Icterus_gularis_MN380381_Campeche_Mexico",
                                               "Icterus_auratus_AF099312_Yucatan_Mexico",
                                               "Icterus_auratus_MN380342_Yucatan_Mexico"
                                               
),
tip_endemicity_status = c( "nonendemic", "nonendemic",
                           "nonendemic",
                           "nonendemic", "nonendemic",
                           "nonendemic",
                           "nonendemic", "nonendemic",
                           "nonendemic","nonendemic",
                           "nonendemic","nonendemic"
                           
))

# set the endemicity status in the tree
icgularis_endemicity <- create_endemicity_status(
  phylo = icgularis_tree,
  island_species = icgularis_status)

icgularis_phylo <- phylobase::phylo4d(icgularis_tree, icgularis_endemicity)

#to save in RDS format
saveRDS(icgularis_phylo, file = "trees/icteridae/icgularis_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
icgularis_phylod <- readRDS("trees/icteridae/icgularis_rds.rds")
icgularis_phylod_asr <- add_asr_node_states(phylod = icgularis_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

icgularis_island_tbl <- extract_island_species(
  phylod = icgularis_phylod_asr, 
  extraction_method = "asr")

ars_plot(icgularis_phylod_asr, node_pies = TRUE)
icgularis_island_tbl
icgularis_island_tbl@island_tbl$branching_times
# Icterus gularis col_time 0.1	min_age 0.0627
# Icterus auratus col_time 2.07	min_age 0.02

# Icterus mesomelas
mesomelas_tree <- ape::read.nexus("trees/icteridae/mesomelas_mcct.tre")
mesomelas_tree <- as(mesomelas_tree, "phylo4")

phylobase::plot(mesomelas_tree, cex = 0.5)

# select individuals on the island and its status
mesomelas_status <- data.frame(tip_labels = c( "Icterus_mesomelas_AF099341_Mexico_Chi"),
                               
                               tip_endemicity_status = c(
                                 "nonendemic"))

# set the endemicity status in the tree
mesomelas_endemicity <- create_endemicity_status(
  phylo = mesomelas_tree,
  island_species = mesomelas_status)

mesomelas_phylo <- phylobase::phylo4d(mesomelas_tree, mesomelas_endemicity)

#to save in RDS format
saveRDS(mesomelas_phylo, file = "trees/icteridae/mesomelas_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
mesomelas_phylod <- readRDS("trees/icteridae/mesomelas_rds.rds")
mesomelas_phylod_asr <- add_asr_node_states(phylod = mesomelas_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

mesomelas_island_tbl <- extract_island_species(
  phylod = mesomelas_phylod_asr, 
  extraction_method = "asr")

ars_plot(mesomelas_phylod_asr, node_pies = TRUE)
mesomelas_island_tbl
mesomelas_island_tbl@island_tbl$branching_times
# Icterus_mesomelas col_time 0.95

# Icterus fuertesi 
icfuertesi_tree <- ape::read.nexus("trees/icteridae/icfuertesi_mcct.tre")
icfuertesi_tree <- as(icfuertesi_tree, "phylo4")
phylobase::plot(icfuertesi_tree, cex = 0.5)

# select individuals on the island and its status
icfuertesi_status <- data.frame(tip_labels = c("Icterus_cucullatus_AF099284",
                                               "Icterus_prosthemelas_AY211213_Yucatan",
                                               "Icterus_prosthemelas_AY211212_Campeche",
                                               "Icterus_fuertesi_AY211215_Veracruz",
                                               "Icterus_fuertesi_AY211214_Veracruz",
                                               "Icterus_fuertesi_AY211206_Veracruz",
                                               "Icterus_fuertesi_AY212245_Veracruz"
),
tip_endemicity_status = c( "nonendemic", "nonendemic",
                           "nonendemic",
                           "nonendemic", "nonendemic",
                           "nonendemic", "nonendemic"))

# set the endemicity status in the tree
icfuertesi_endemicity <- create_endemicity_status(
  phylo = icfuertesi_tree,
  island_species = icfuertesi_status)

icfuertesi_phylo <- phylobase::phylo4d(icfuertesi_tree, icfuertesi_endemicity)

#to save in RDS format
saveRDS(icfuertesi_phylo, file = "trees/icteridae/icfuertesi_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
icfuertesi_phylod <- readRDS("trees/icteridae/icfuertesi_rds.rds")
icfuertesi_phylod_asr <- add_asr_node_states(phylod = icfuertesi_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

icfuertesi_island_tbl <- extract_island_species(
  phylod = icfuertesi_phylod_asr, 
  extraction_method = "asr")

ars_plot(icfuertesi_phylod_asr, node_pies = TRUE)
icfuertesi_island_tbl
icfuertesi_island_tbl@island_tbl$branching_times
# Icterus_cucullatus      col_time 0.63    
# Icterus_fuertesi (spurius)       max_age  0.27   
# Icterus_prosthemelas     max_age 2.69    min_age   0.07

#molothrus
molothrus_tree <- ape::read.nexus("trees/icteridae/molothrus_mcct.tree")
molothrus_tree <- as(molothrus_tree, "phylo4")
phylobase::plot(molothrus_tree, cex = 0.1)

# select individuals on the island and its status
molothrus_status <- data.frame(tip_labels = c("Molothrus_aeneus_DQ433805_USA",
                                              "Molothrus_aeneus_DQ433806_USA",
                                              "Molothrus_aeneus_DQ433804_USA",
                                              "Molothrus_aeneus_DQ433808_USA",
                                              "Molothrus_aeneus_DQ433803_USA",
                                              "Molothrus_aeneus_DQ433807_USA",
                                              "Molothrus_aeneus_KM894343_El_Salvador",
                                              "Molothrus_aeneus_DQ433025_Panama"
),
tip_endemicity_status = c( "nonendemic",
                           "nonendemic",
                           "nonendemic",
                           "nonendemic",
                           "nonendemic",
                           "nonendemic",
                           "nonendemic",
                           "nonendemic"))

# set the endemicity status in the tree
molothrus_endemicity <- create_endemicity_status(
  phylo = molothrus_tree,
  island_species = molothrus_status)

molothrus_phylo <- phylobase::phylo4d(molothrus_tree, molothrus_endemicity)

#to save in RDS format
saveRDS(molothrus_phylo, file = "trees/icteridae/molothrus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
molothrus_phylod <- readRDS("trees/icteridae/molothrus_rds.rds")
molothrus_phylod_asr <- add_asr_node_states(phylod = molothrus_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

molothrus_island_tbl <- extract_island_species(
  phylod = molothrus_phylod_asr, 
  extraction_method = "asr")

ars_plot(molothrus_phylod_asr, node_pies = TRUE)
molothrus_island_tbl
molothrus_island_tbl@island_tbl$branching_times
# max_age 0.918

## Molothrus oryzivorus
molothrusor_tree <- ape::read.nexus("trees/icteridae/molothrus_ory_mcct.tre")


molothrusor_tree <- as(molothrusor_tree, "phylo4")
phylobase::plot(molothrusor_tree, cex = 0.1)

# select individuals on the island and its status
molothrusor_status <- data.frame(tip_labels = c("Molothrus_oryzivorus_JQ175401_Pan",
                                                "Molothrus_oryzivorus_JQ175403_Guy",
                                                "Molothrus_oryzivorus_JQ175402_Guy"),
                                 
                                 tip_endemicity_status = c("nonendemic",
                                                           "nonendemic",
                                                           "nonendemic"))

# set the endemicity status in the tree
molothrusor_endemicity <- create_endemicity_status(
  phylo =molothrusor_tree,
  island_species =molothrusor_status)

molothrusor_phylo <- phylobase::phylo4d(molothrusor_tree,molothrusor_endemicity)

#to save in RDS format
saveRDS(molothrusor_phylo, file = "trees/icteridae/molothrusor_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
molothrusor_phylod <- readRDS("trees/icteridae/molothrusor_rds.rds")
molothrusor_phylod_asr <- add_asr_node_states(phylod =molothrusor_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

molothrusor_island_tbl <- extract_island_species(
  phylod =molothrusor_phylod_asr, 
  extraction_method = "asr")

ars_plot(molothrusor_phylod_asr, node_pies = TRUE)
molothrusor_island_tbl
molothrusor_island_tbl@island_tbl$branching_times
# Max_time 2.22

# Quiscalus mexicanus
quiscalus_tree <- ape::read.nexus("trees/icteridae/quiscalus_mcct.tree")
quiscalus_tree <- as(quiscalus_tree, "phylo4")
phylobase::plot(quiscalus_tree, cex = 0.1)

# select individuals on the island and its status
quiscalus_status <- data.frame(tip_labels = c("Quiscalus_mexicanus_EU414595_Panama",
                                              "Quiscalus_mexicanus_EU414594_Panama",
                                              "Quiscalus_mexicanus_EU414593_Panama",
                                              "Quiscalus_mexicanus_EU414591_Nicaragua",
                                              "Quiscalus_mexicanus_EU414590_Nicaragua",
                                              "Quiscalus_mexicanus_EU414589_Nicaragua",
                                              "Quiscalus_mexicanus_EU414588_Honduras",
                                              "Quiscalus_mexicanus_EU414587_Honduras",
                                              "Quiscalus_mexicanus_EU414585_Guatemala",
                                              "Quiscalus_mexicanus_EU414584_Guatemala",
                                              "Quiscalus_mexicanus_EU414583_Guatemala",
                                              "Quiscalus_mexicanus_EU414582_Guatemala",
                                              "Quiscalus_mexicanus_EU414581_Oaxaca_Mexico",
                                              "Quiscalus_mexicanus_EU414579_Mex_city_Mexico",
                                              "Quiscalus_mexicanus_EU414578_Mex_city_Mexico"),
                               tip_endemicity_status = c("nonendemic", "nonendemic", 
                                                         "nonendemic", "nonendemic",
                                                         "nonendemic", "nonendemic", 
                                                         "nonendemic", "nonendemic", 
                                                         "nonendemic", "nonendemic",
                                                         "nonendemic", "nonendemic", 
                                                         "nonendemic", "nonendemic",
                                                         "nonendemic"
                               ))

# set the endemicity status in the tree
quiscalus_endemicity <- create_endemicity_status(
  phylo = quiscalus_tree,
  island_species = quiscalus_status)

quiscalus_phylo <- phylobase::phylo4d(quiscalus_tree, quiscalus_endemicity)

#to save in RDS format
saveRDS(quiscalus_phylo, file = "trees/icteridae/quiscalus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
quiscalus_phylod <- readRDS("trees/icteridae/quiscalus_rds.rds")
quiscalus_phylod_asr <- add_asr_node_states(phylod = quiscalus_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

quiscalus_island_tbl <- extract_island_species(
  phylod = quiscalus_phylod_asr, 
  extraction_method = "asr")

ars_plot(quiscalus_phylod_asr, node_pies = TRUE)
quiscalus_island_tbl
quiscalus_island_tbl@island_tbl$branching_times
# Max_time 0.2914402

# Family mimidae
# Mimus gilvus
mimus_tree <- ape::read.nexus("trees/mimidae/mimus_mcct.tree")
mimus_tree <- as(mimus_tree, "phylo4")
phylobase::plot(mimus_tree, cex = 0.1)

# select individuals on the island and its status
mimus_status <- data.frame(tip_labels = c("Mimus_gilvus_JN799667_Yucatan_Mexico",
                                          "Mimus_gilvus_JN799668_Yucatan_Mexico",
                                          "Mimus_gilvus_JN799669_Campeche_Mexico"#,
                                          #"Mimus_gilvus_JN799670_Quintana_Roo_Mexico"
),
tip_endemicity_status = c("nonendemic", #"nonendemic",
                          "nonendemic", "nonendemic"
))

# set the endemicity status in the tree
mimus_endemicity <- create_endemicity_status(
  phylo = mimus_tree,
  island_species = mimus_status)

mimus_phylo <- phylobase::phylo4d(mimus_tree, mimus_endemicity)

#to save in RDS format
saveRDS(mimus_phylo, file = "trees/mimidae/mimus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
mimus_phylod <- readRDS("trees/mimidae/mimus_rds.rds")
mimus_phylod_asr <- add_asr_node_states(phylod = mimus_phylod, 
                                        asr_method = "mk", 
                                        tie_preference = "island", 
                                        earliest_col = FALSE)

mimus_island_tbl <- extract_island_species(
  phylod = mimus_phylod_asr, 
  extraction_method = "asr")

ars_plot(mimus_phylod_asr, node_pies = TRUE)
mimus_island_tbl
mimus_island_tbl@island_tbl$branching_times
# col_time  0.57   branching_times 0.25

# Melanoptila glabrirostris
melanoptila_tree <- ape::read.nexus("trees/mimidae/melanoptila_mcct.tree")
melanoptila_tree <- as(melanoptila_tree, "phylo4")
phylobase::plot(melanoptila_tree, cex = 0.1)

# select individuals on the island and its status
melanoptila_status <- data.frame(tip_labels = c("Melanoptila_glabrirostris_AF140893_Endemic_Mexico",
                                                "Melanoptila_glabrirostris_EF468197_Endemic_Mexico",
                                                "Melanoptila_glabrirostris_JN799663_Endemic_Mexico"),
                                 tip_endemicity_status = c("endemic", "endemic", "endemic"))

# set the endemicity status in the tree
melanoptila_endemicity <- create_endemicity_status(
  phylo = melanoptila_tree,
  island_species = melanoptila_status)

melanoptila_phylo <- phylobase::phylo4d(melanoptila_tree, melanoptila_endemicity)

#to save in RDS format
saveRDS(melanoptila_phylo, file = "trees/mimidae/melanoptila_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
melanoptila_phylod <- readRDS("trees/mimidae/melanoptila_rds.rds")
melanoptila_phylod_asr <- add_asr_node_states(phylod = melanoptila_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

melanoptila_island_tbl <- extract_island_species(
  phylod = melanoptila_phylod_asr, 
  extraction_method = "asr")

ars_plot(melanoptila_phylod_asr, node_pies = TRUE)
melanoptila_island_tbl
melanoptila_island_tbl@island_tbl$branching_times
# min_age  0.480430

# Family Oxyruncidae
# Onychorhynchus coronatus
onychorhynchus_tree <- ape::read.nexus("trees/oxyruncidae/onychorhynchus_mcct.tree")
onychorhynchus_tree <- as(onychorhynchus_tree, "phylo4")
phylobase::plot(onychorhynchus_tree, cex = 0.1)

# select individuals on the island and its status
onychorhynchus_status <- data.frame(tip_labels = c("Onychorhynchus_coronatus_FJ175945_Belize",
                                                   "Onychorhynchus_coronatus_FJ175946_Belize",
                                                   "Onychorhynchus_coronatus_FJ175947_Belize"),
                                    tip_endemicity_status = c( "nonendemic", "nonendemic", "nonendemic"  ))

# set the endemicity status in the tree
onychorhynchus_endemicity <- create_endemicity_status(
  phylo = onychorhynchus_tree,
  island_species = onychorhynchus_status)

onychorhynchus_phylo <- phylobase::phylo4d(onychorhynchus_tree, onychorhynchus_endemicity)

#to save in RDS format
saveRDS(onychorhynchus_phylo, file = "trees/oxyruncidae/onychorhynchus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
onychorhynchus_phylod <- readRDS("trees/oxyruncidae/onychorhynchus_rds.rds")
onychorhynchus_phylod_asr <- add_asr_node_states(phylod = onychorhynchus_phylod, 
                                                 asr_method = "mk", 
                                                 tie_preference = "island", 
                                                 earliest_col = FALSE)

onychorhynchus_island_tbl <- extract_island_species(
  phylod = onychorhynchus_phylod_asr, 
  extraction_method = "asr")

ars_plot(onychorhynchus_phylod_asr, node_pies = TRUE)
onychorhynchus_island_tbl
onychorhynchus_island_tbl@island_tbl$branching_times
#  branching_times 0.29 min_age 0.074

## terenotriccus
terenotriccus_tree <- ape::read.nexus("trees/oxyruncidae/terenotriccus_mcct.tre")


terenotriccus_tree <- as(terenotriccus_tree, "phylo4")
phylobase::plot(terenotriccus_tree, cex = 0.1)

# select individuals on the island and its status
terenotriccus_status <- data.frame(tip_labels = c( "Terenotriccus_erythrurus_FJ175953_Bel",
                                                   "Terenotriccus_erythrurus_FJ175952_Bel",
                                                   "Terenotriccus_erythrurus_FJ175951_Bel"),
                                   
                                   tip_endemicity_status = c("nonendemic",
                                                             "nonendemic",
                                                             "nonendemic"))

# set the endemicity status in the tree
terenotriccus_endemicity <- create_endemicity_status(
  phylo =terenotriccus_tree,
  island_species =terenotriccus_status)

terenotriccus_phylo <- phylobase::phylo4d(terenotriccus_tree,terenotriccus_endemicity)

#to save in RDS format
saveRDS(terenotriccus_phylo, file = "trees/oxyruncidae/terenotriccus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
terenotriccus_phylod <- readRDS("trees/oxyruncidae/terenotriccus_rds.rds")
terenotriccus_phylod_asr <- add_asr_node_states(phylod =terenotriccus_phylod, 
                                                asr_method = "mk", 
                                                tie_preference = "island", 
                                                earliest_col = FALSE)

terenotriccus_island_tbl <- extract_island_species(
  phylod =terenotriccus_phylod_asr, 
  extraction_method = "asr")

ars_plot(terenotriccus_phylod_asr, node_pies = TRUE)
terenotriccus_island_tbl
terenotriccus_island_tbl@island_tbl$branching_times
# min_age 0.1475093 col_time 0.2159673 

# Family Parulidae
# Basileuterus culicuvorus
basileuterus_tree <- ape::read.nexus("trees/parulidae/basileuterus_mcct.tree")
basileuterus_tree <- as(basileuterus_tree, "phylo4")
phylobase::plot(basileuterus_tree, cex = 0.1)

# select individuals on the island and its status
basileuterus_status <- data.frame(tip_labels = c( #"Basileuterus_culicivorus_GU189094_El_Salvador"
  #,
  
  #"Basileuterus_culicivorus_GU189110_El_Salvador"
  "Basileuterus_culicivorus_GU189096_Campeche"
  
),
tip_endemicity_status = c("nonendemic"))

# set the endemicity status in the tree
basileuterus_endemicity <- create_endemicity_status(
  phylo = basileuterus_tree,
  island_species = basileuterus_status)

basileuterus_phylo <- phylobase::phylo4d(basileuterus_tree, basileuterus_endemicity)

#to save in RDS format
saveRDS(basileuterus_phylo, file = "trees/parulidae/basileuterus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
basileuterus_phylod <- readRDS("trees/parulidae/basileuterus_rds.rds")
basileuterus_phylod_asr <- add_asr_node_states(phylod = basileuterus_phylod, 
                                               asr_method = "mk", 
                                               tie_preference = "island", 
                                               earliest_col = FALSE)

basileuterus_island_tbl <- extract_island_species(
  phylod = basileuterus_phylod_asr, 
  extraction_method = "asr")

ars_plot(basileuterus_phylod_asr, node_pies = TRUE)
basileuterus_island_tbl
basileuterus_island_tbl@island_tbl$branching_times
# col_time 0.17

# Geothlypis poliocephala
geothlypis_tree <- ape::read.nexus("trees/parulidae/geothlypis_mcct.tree")
geothlypis_tree <- as(geothlypis_tree, "phylo4")
phylobase::plot(geothlypis_tree, cex = 0.1)

# select individuals on the island and its status
geothlypis_status <- data.frame(tip_labels = c(#"Geothlypis_poliocephala_FJ605327_Yucatan_Mexico"
  "Geothlypis_poliocephala_FJ605329_Honduras"),
  tip_endemicity_status = c("nonendemic"))

# set the endemicity status in the tree
geothlypis_endemicity <- create_endemicity_status(
  phylo = geothlypis_tree,
  island_species = geothlypis_status)

geothlypis_phylo <- phylobase::phylo4d(geothlypis_tree, geothlypis_endemicity)

#to save in RDS format
saveRDS(geothlypis_phylo, file = "trees/parulidae/geothlypis_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
geothlypis_phylod <- readRDS("trees/parulidae/geothlypis_rds.rds")
geothlypis_phylod_asr <- add_asr_node_states(phylod = geothlypis_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

geothlypis_island_tbl <- extract_island_species(
  phylod = geothlypis_phylod_asr, 
  extraction_method = "asr")

ars_plot(geothlypis_phylod_asr, node_pies = TRUE)
geothlypis_island_tbl
geothlypis_island_tbl@island_tbl$branching_times
# branching_times 0.13

# Family Passeridae
# Arremonops chloronotus
arremonops_tree <- ape::read.nexus("trees/passeridae/arremonops_mcct.tree")
arremonops_tree <- as(arremonops_tree, "phylo4")
phylobase::plot(arremonops_tree, cex = 0.1)

# select individuals on the island and its status
arremonops_status <- data.frame(tip_labels = c("Arremonops_chloronotus_FJ547295_Campeche_endemic"),
                                tip_endemicity_status = c( "endemic"  ))

# set the endemicity status in the tree
arremonops_endemicity <- create_endemicity_status(
  phylo = arremonops_tree,
  island_species = arremonops_status)
arremonops_phylo <- phylobase::phylo4d(arremonops_tree, arremonops_endemicity)

#to save in RDS format
saveRDS(arremonops_phylo, file = "trees/passeridae/arremonops_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
arremonops_phylod <- readRDS("trees/passeridae/arremonops_rds.rds")
arremonops_phylod_asr <- add_asr_node_states(phylod = arremonops_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

arremonops_island_tbl <- extract_island_species(
  phylod = arremonops_phylod_asr, 
  extraction_method = "asr")

ars_plot(arremonops_phylod_asr, node_pies = TRUE)
arremonops_island_tbl
arremonops_island_tbl@island_tbl$branching_times
#

#arremonops cytb 
arremonops_conc_tree <- ape::read.nexus("trees/passeridae/arremonops_cytb_mcct.tre")
arremonops_conc_tree <- as(arremonops_conc_tree, "phylo4")
phylobase::plot(arremonops_conc_tree, cex = 0.1)

# select individuals on the island and its status
arremonops_conc_status <- data.frame(tip_labels = c("Arremonops_chloronotus_Campeche"),
                                     tip_endemicity_status = c( "endemic"  ))

# set the endemicity status in the tree
arremonops_conc_endemicity <- create_endemicity_status(
  phylo = arremonops_conc_tree,
  island_species = arremonops_conc_status)

arremonops_conc_phylo <- phylobase::phylo4d(arremonops_conc_tree, arremonops_conc_endemicity)

#to save in RDS format
saveRDS(arremonops_conc_phylo, file = "trees/passeridae/arremonops_cytb_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
arremonops_conc_phylod <- readRDS("trees/passeridae/arremonops_cytb_rds.rds")
arremonops_conc_phylod_asr <- add_asr_node_states(phylod = arremonops_conc_phylod, 
                                                  asr_method = "mk", 
                                                  tie_preference = "island", 
                                                  earliest_col = FALSE)

arremonops_conc_island_tbl <- extract_island_species(
  phylod = arremonops_conc_phylod_asr, 
  extraction_method = "asr")

ars_plot(arremonops_conc_phylod_asr, node_pies = TRUE)
arremonops_conc_island_tbl
arremonops_conc_island_tbl@island_tbl$branching_times
# col_time 2.54

# Family Polioptilidae
# Polioptila albiventris
polioptila_tree <- ape::read.nexus("trees/polioptilidae/polioptila_mcct.tree")
polioptila_tree <- as(polioptila_tree, "phylo4")
phylobase::plot(polioptila_tree, cex = 0.1)

# select individuals on the island and its status
polioptila_status <- data.frame(tip_labels = c("Polioptila_albiloris_MG903006_Yucatan_Mexico",
                                               "Polioptila_albiloris_MG903007_Yucatan_Mexico",
                                               "Polioptila_albiloris_MG903008_Yucatan_Mexico",
                                               "Polioptila_caerulea_MG903054_Quintana_Roo_Mexico",
                                               "Polioptila_caerulea_MG903055_Quintana_Roo_Mexico",
                                               "Polioptila_caerulea_MG903056_Quintana_Roo_Mexico",
                                               "Polioptila_caerulea_MG903057_Quintana_Roo_Mexico",
                                               "Polioptila_caerulea_MG903059_Quintana_Roo_Mexico",
                                               "Polioptila_caerulea_MG903064_Campeche_Mexico",
                                               "Polioptila_caerulea_MG903065_Campeche_Mexico",
                                               "Polioptila_caerulea_MG903066_Campeche_Mexico",
                                               "Polioptila_caerulea_MG903067_Yucatan_Mexico",
                                               "Polioptila_caerulea_MG903068_Yucatan_Mexico",
                                               "Polioptila_plumbea_MG903175_Campeche_Mex",
                                               "Polioptila_plumbea_MG903176_Quintana_Roo_Mex",
                                               "Polioptila_plumbea_MG903177_Veracruz_Mex",
                                               "Polioptila_caerulea_MG903063_Tabasco_Mexico"
                                               ),
                                
                                tip_endemicity_status = c("nonendemic",  "nonendemic",
                                                          "nonendemic",  "nonendemic",
                                                          "nonendemic",  "nonendemic",
                                                          "nonendemic",  "nonendemic",
                                                          "nonendemic",  "nonendemic",
                                                          "nonendemic",  "nonendemic",
                                                          "nonendemic",  "nonendemic",
                                                          "nonendemic", "nonendemic",
                                                          "nonendemic"))

# set the endemicity status in the tree
polioptila_endemicity <- create_endemicity_status(
  phylo = polioptila_tree,
  island_species = polioptila_status)

polioptila_phylo <- phylobase::phylo4d(polioptila_tree, polioptila_endemicity)

#to save in RDS format
saveRDS(polioptila_phylo, file = "trees/polioptilidae/polioptila_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
polioptila_phylod <- readRDS("trees/polioptilidae/polioptila_rds.rds")
polioptila_phylod_asr <- add_asr_node_states(phylod = polioptila_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

polioptila_island_tbl <- extract_island_species(
  phylod = polioptila_phylod_asr, 
  extraction_method = "asr")

ars_plot(polioptila_phylod_asr, node_pies = TRUE)
polioptila_island_tbl
polioptila_island_tbl@island_tbl$branching_times
# Polioptila albiloris   branching_times   min_age 0.15
# Polioptila caerulea   branching_times 0.39  min_age 0.33
# Polioptila plumbea    branching_times  0.17 min_age min_age 0.12

# Ramphocaenus melanurus
ramphocaenus_tree <- ape::read.nexus("trees/polioptilidae/ramphocaenus_mcct.tree")
ramphocaenus_tree <- as(ramphocaenus_tree, "phylo4")
phylobase::plot(ramphocaenus_tree, cex = 0.1)

# select individuals on the island and its status
ramphocaenus_status <- data.frame(tip_labels = c("Ramphocaenus_melanurus_MG903241_Campeche_Mexico",
                                                 "Ramphocaenus_melanurus_MG903242_Campeche_Mexico",
                                                 "Ramphocaenus_melanurus_MG903243_Yucatan_Mexico",
                                                 "Ramphocaenus_melanurus_FJ176020_Belice",
                                                 "Ramphocaenus_melanurus_FJ176021_Belice",
                                                 "Ramphocaenus_melanurus_MG903274_Honduras",
                                                 "Ramphocaenus_melanurus_MG903273_Honduras"
),

tip_endemicity_status = c( "nonendemic", "nonendemic", 
                           "nonendemic", "nonendemic", 
                           "nonendemic",
                           "nonendemic", "nonendemic" 
))

# set the endemicity status in the tree
ramphocaenus_endemicity <- create_endemicity_status(
  phylo = ramphocaenus_tree,
  island_species = ramphocaenus_status)

ramphocaenus_phylo <- phylobase::phylo4d(ramphocaenus_tree, ramphocaenus_endemicity)

#to save in RDS format
saveRDS(ramphocaenus_phylo, file = "trees/polioptilidae/ramphocaenus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
ramphocaenus_phylod <- readRDS("trees/polioptilidae/ramphocaenus_rds.rds")
ramphocaenus_phylod_asr <- add_asr_node_states(phylod = ramphocaenus_phylod, 
                                               asr_method = "mk", 
                                               tie_preference = "island", 
                                               earliest_col = FALSE)

ramphocaenus_island_tbl <- extract_island_species(
  phylod = ramphocaenus_phylod_asr, 
  extraction_method = "asr")

ars_plot(ramphocaenus_phylod_asr, node_pies = TRUE)
ramphocaenus_island_tbl
ramphocaenus_island_tbl@island_tbl$branching_times
# col_time 0.22	min_age 0.1603

# Family thamnophilidae
# Thamnophilus doliatus
thamnophilus_tree <- ape::read.nexus("trees/thamnophilidae/thamnophilus_mcct.tree")
thamnophilus_tree <- as(thamnophilus_tree, "phylo4")
phylobase::plot(thamnophilus_tree, cex = 0.1)

# select individuals on the island and its status
thamnophilus_status <- data.frame(tip_labels = c("Thamnophilus_doliatus_FJ175857",
                                                 "Thamnophilus_doliatus_FJ175858",
                                                 "Thamnophilus_doliatus_FJ175859",
                                                 "Thamnophilus_doliatus_FJ175860",
                                                 "Thamnophilus_doliatus_FJ175861",
                                                 "Thamnophilus_doliatus_FJ175862",
                                                 "Thamnophilus_doliatus_FJ175863",
                                                 "Thamnophilus_doliatus_FJ175864",
                                                 "Thamnophilus_doliatus_FJ175865",
                                                 "Thamnophilus_doliatus_FJ175866"),
                                  
                                  tip_endemicity_status = c("nonendemic", "nonendemic",
                                                            "nonendemic", "nonendemic",
                                                            "nonendemic", "nonendemic",
                                                            "nonendemic", "nonendemic",
                                                            "nonendemic", "nonendemic"))

# set the endemicity status in the tree
thamnophilus_endemicity <- create_endemicity_status(
  phylo = thamnophilus_tree,
  island_species = thamnophilus_status)

thamnophilus_phylo <- phylobase::phylo4d(thamnophilus_tree, thamnophilus_endemicity)

#to save in RDS format
saveRDS(thamnophilus_phylo, file = "trees/thamnophilidae/thamnophilus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
thamnophilus_phylod <- readRDS("trees/thamnophilidae/thamnophilus_rds.rds")
thamnophilus_phylod_asr <- add_asr_node_states(phylod = thamnophilus_phylod, 
                                               asr_method = "mk", 
                                               tie_preference = "island", 
                                               earliest_col = FALSE)

thamnophilus_island_tbl <- extract_island_species(
  phylod = thamnophilus_phylod_asr, 
  extraction_method = "asr")

ars_plot(thamnophilus_phylod_asr, node_pies = TRUE)
thamnophilus_island_tbl
thamnophilus_island_tbl@island_tbl$branching_times
# branching_times 0.94 min_age 0.09

## Microrhopias quixensis
microrhopias_tree <- ape::read.nexus("trees/thamnophilidae/microrhopias_mcct.tre")
microrhopias_tree <- as(microrhopias_tree, "phylo4")
phylobase::plot(microrhopias_tree, cex = 0.1)

# select individuals on the island and its status
microrhopias_status <- data.frame(tip_labels = c("Microrhopias_quixensis_FJ175882_Belize",
                                                 "Microrhopias_quixensis_FJ175881_Belize"
                                                 #,
                                                # "Microrhopias_quixensis_MW353039_Pan",
                                                # "Microrhopias_quixensis_FJ175885_Pan"
                                                 # "Microrhopias_quixensis_EF640027_Peru"
),

tip_endemicity_status = c(#"nonendemic", "nonendemic",
                          "nonendemic", "nonendemic"))

# set the endemicity status in the tree
microrhopias_endemicity <- create_endemicity_status(
  phylo =microrhopias_tree,
  island_species =microrhopias_status)

microrhopias_phylo <- phylobase::phylo4d(microrhopias_tree,microrhopias_endemicity)

#to save in RDS format
saveRDS(microrhopias_phylo, file = "trees/thamnophilidae/microrhopias_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
microrhopias_phylod <- readRDS("trees/thamnophilidae/microrhopias_rds.rds")
microrhopias_phylod_asr <- add_asr_node_states(phylod =microrhopias_phylod, 
                                               asr_method = "mk", 
                                               tie_preference = "island", 
                                               earliest_col = FALSE)

microrhopias_island_tbl <- extract_island_species(
  phylod =microrhopias_phylod_asr, 
  extraction_method = "asr")

ars_plot(microrhopias_phylod_asr, node_pies = TRUE)
microrhopias_island_tbl
microrhopias_island_tbl@island_tbl$col_time
# Endemic_MaxAge_MinAge	0.98,0.53

# Cercomacroides tyrannina
cercomacroides_tree <- ape::read.nexus("trees/thamnophilidae/cercomacroides_mcct.tree")
cercomacroides_tree <- as(cercomacroides_tree, "phylo4")
phylobase::plot(cercomacroides_tree, cex = 0.1)

# select individuals on the island and its status
cercomacroides_status <- data.frame(tip_labels = c("Cercomacroides_tyrannina_FJ175897_Bel",
                                                   "Cercomacroides_tyrannina_FJ175896_Bel",
                                                   "Cercomacroides_tyrannina_FJ175895_Bel",
                                                   "Cercomacroides_tyrannina_FJ175894_Bel",
                                                   "Cercomacroides_tyrannina_FJ175893_Bel",
                                                   "Cercomacroides_tyrannina_FJ175892_Bel",
                                                   "Cercomacroides_tyrannina_FJ175891_Bel",
                                                   "Cercomacroides_tyrannina_FJ175890_Bel"),
                                    
                                    tip_endemicity_status = c("nonendemic", "nonendemic",
                                                              "nonendemic", "nonendemic",
                                                              "nonendemic", "nonendemic",
                                                              "nonendemic", "nonendemic"))

# set the endemicity status in the tree
cercomacroides_endemicity <- create_endemicity_status(
  phylo =cercomacroides_tree,
  island_species =cercomacroides_status)

cercomacroides_phylo <- phylobase::phylo4d(cercomacroides_tree,cercomacroides_endemicity)

#to save in RDS format
saveRDS(cercomacroides_phylo, file = "trees/thamnophilidae/cercomacroides_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
cercomacroides_phylod <- readRDS("trees/thamnophilidae/cercomacroides_rds.rds")
cercomacroides_phylod_asr <- add_asr_node_states(phylod =cercomacroides_phylod, 
                                                 asr_method = "mk", 
                                                 tie_preference = "island", 
                                                 earliest_col = FALSE)

cercomacroides_island_tbl <- extract_island_species(
  phylod =cercomacroides_phylod_asr, 
  extraction_method = "asr")

ars_plot(cercomacroides_phylod_asr, node_pies = TRUE)
cercomacroides_island_tbl
cercomacroides_island_tbl@island_tbl$branching_times
# col_time 0.63  min_age 0.09 

## Dysithamnus mentalis
dysithamnus_tree <- ape::read.nexus("trees/thamnophilidae/dysithamnus_mcct.tre")
dysithamnus_tree <- as(dysithamnus_tree, "phylo4")
phylobase::plot(dysithamnus_tree, cex = 0.1)

# select individuals on the island and its status
dysithamnus_status <- data.frame(tip_labels = c("Dysithamnus_mentalis_AY676948"),
                                 
                                 tip_endemicity_status = c("nonendemic"))

# set the endemicity status in the tree
dysithamnus_endemicity <- create_endemicity_status(
  phylo =dysithamnus_tree,
  island_species =dysithamnus_status)

dysithamnus_phylo <- phylobase::phylo4d(dysithamnus_tree,dysithamnus_endemicity)

#to save in RDS format
saveRDS(dysithamnus_phylo, file = "trees/thamnophilidae/dysithamnus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
dysithamnus_phylod <- readRDS("trees/thamnophilidae/dysithamnus_rds.rds")
dysithamnus_phylod_asr <- add_asr_node_states(phylod =dysithamnus_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

dysithamnus_island_tbl <- extract_island_species(
  phylod =dysithamnus_phylod_asr, 
  extraction_method = "asr")

ars_plot(dysithamnus_phylod_asr, node_pies = TRUE)
dysithamnus_island_tbl
dysithamnus_island_tbl@island_tbl$col_time
# Max_age 0.85

# Family Thraupidae
# Coereba flaveola
coereba_tree <- ape::read.nexus("trees/thraupidae/coereba_mcct.tree")
coereba_tree <- as(coereba_tree, "phylo4")
phylobase::plot(coereba_tree, cex = 0.1)

# select individuals on the island and its status
coereba_status <- data.frame(tip_labels = c("Coreba_flaveola_FJ176050_Belize",
                                            "Coreba_flaveola_FJ176051_Belize",
                                            "Coreba_flaveola_FJ176052_Belize",
                                            "Coreba_flaveola_FJ176053_Belize",
                                            "Coreba_flaveola_EF567867_Belize", 
                                            "Coreba_flaveola_EF567868_Belize",
                                            "Coreba_flaveola_EF567893_Quintana_Roo"),
                             
                             tip_endemicity_status = c("nonendemic", "nonendemic",
                                                       "nonendemic", "nonendemic",
                                                       "nonendemic", "nonendemic", 
                                                       "nonendemic" ))

# set the endemicity status in the tree
coereba_endemicity <- create_endemicity_status(
  phylo = coereba_tree,
  island_species = coereba_status)

coereba_phylo <- phylobase::phylo4d(coereba_tree, coereba_endemicity)

#to save in RDS format
saveRDS(coereba_phylo, file = "trees/thraupidae/coereba_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
coereba_phylod <- readRDS("trees/thraupidae/coereba_rds.rds")
coereba_phylod_asr <- add_asr_node_states(phylod = coereba_phylod, 
                                          asr_method = "mk", 
                                          tie_preference = "island", 
                                          earliest_col = FALSE)

coereba_island_tbl <- extract_island_species(
  phylod = coereba_phylod_asr, 
  extraction_method = "asr")

ars_plot(coereba_phylod_asr, node_pies = TRUE)
coereba_island_tbl
coereba_island_tbl@island_tbl$branching_times
# 2 times
# col_time 0.2633625 and 2.28

# Chlorophanes spiza
chlorophanesnd2_tree <- ape::read.nexus("trees/thraupidae/chlorophanesnd2_mcct.tre")
chlorophanesnd2_tree <- as(chlorophanesnd2_tree, "phylo4")
phylobase::plot(chlorophanesnd2_tree, cex = 0.1)

# select individuals on the island and its status
chlorophanesnd2_status <- data.frame(tip_labels = c("Chlorophanes_spiza_FJ176120_Bel",
                                                    "Chlorophanes_spiza_FJ176119_Bel"
),

tip_endemicity_status = c("nonendemic", "nonendemic"))

# set the endemicity status in the tree
chlorophanesnd2_endemicity <- create_endemicity_status(
  phylo =chlorophanesnd2_tree,
  island_species =chlorophanesnd2_status)

chlorophanesnd2_phylo <- phylobase::phylo4d(chlorophanesnd2_tree,chlorophanesnd2_endemicity)

#to save in RDS format
saveRDS(chlorophanesnd2_phylo, file = "trees/thraupidae/chlorophanesnd2_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
chlorophanesnd2_phylod <- readRDS("trees/thraupidae/chlorophanesnd2_rds.rds")
chlorophanesnd2_phylod_asr <- add_asr_node_states(phylod =chlorophanesnd2_phylod, 
                                                  asr_method = "mk", 
                                                  tie_preference = "island", 
                                                  earliest_col = FALSE)

chlorophanesnd2_island_tbl <- extract_island_species(
  phylod =chlorophanesnd2_phylod_asr, 
  extraction_method = "asr")

ars_plot(chlorophanesnd2_phylod_asr, node_pies = TRUE)
chlorophanesnd2_island_tbl
chlorophanesnd2_island_tbl@island_tbl$branching_times
# col_time 0.1755 min_age 0.0819

# Cyanerpes cyaneus
cyanerpes_tree <- ape::read.nexus("trees/thraupidae/cyanerpes_mcct.tree")
cyanerpes_tree <- as(cyanerpes_tree, "phylo4")
which(cyanerpes_tree$edge.length < 0)
# 8
cyanerpes_tree$edge.length[8] <- 0
phylobase::plot(cyanerpes_tree, cex = 0.6)

# select individuals on the island and its status
cyanerpes_status <- data.frame(tip_labels = c(
  "Cyanerpes_cyaneus_FJ176125_Bel",
  "Cyanerpes_cyaneus_FJ176124_Bel",
  "Cyanerpes_cyaneus_FJ176126_Bel",
  "Cyanerpes_cyaneus_FJ176127_Bel",
  "Cyanerpes_cyaneus_FJ176128_Bel",
  "Cyanerpes_cyaneus_FJ176129_Bel",
  "Cyanerpes_cyaneus_FJ176130_Bel",
  "Cyanerpes_cyaneus_GU215405", # Pan
  "Cyanerpes_cyaneus_FJ176136", # Pan
  "Cyanerpes_cyaneus_FJ176135", # Pan
  "Cyanerpes_cyaneus_FJ176139", # Pan
  "Cyanerpes_cyaneus_FJ176133", # Pan
  "Cyanerpes_cyaneus_FJ176134", # Pan
  "Cyanerpes_cyaneus_MH822288", # Colombia
  "Cyanerpes_cyaneus_EF529846"
  #	"Cyanerpes_cyaneus_MH822291", #Colombia
  #	"Cyanerpes_cyaneus_JN810450" # Brazil
),
tip_endemicity_status = c("nonendemic",  "nonendemic", "nonendemic",
                          "nonendemic",  "nonendemic", "nonendemic",
                          "nonendemic",  "nonendemic", "nonendemic",
                          "nonendemic",  "nonendemic", "nonendemic",
                          "nonendemic",  "nonendemic", "nonendemic"
))

# set the endemicity status in the tree
cyanerpes_endemicity <- create_endemicity_status(
  phylo = cyanerpes_tree,
  island_species = cyanerpes_status)

cyanerpes_phylo <- phylobase::phylo4d(cyanerpes_tree, cyanerpes_endemicity)

#to save in RDS format
saveRDS(cyanerpes_phylo, file = "trees/thraupidae/cyanerpes_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
cyanerpes_phylod <- readRDS("trees/thraupidae/cyanerpes_rds.rds")
cyanerpes_phylod_asr <- add_asr_node_states(phylod = cyanerpes_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

cyanerpes_island_tbl <- extract_island_species(
  phylod = cyanerpes_phylod_asr, 
  extraction_method = "asr")

ars_plot(cyanerpes_phylod_asr, node_pies = TRUE)
cyanerpes_island_tbl@island_tbl
# col_time 0.12

# Eucometis penicillata
eucometis_tree <- ape::read.nexus("trees/thraupidae/eucometis_mcct.tree")
eucometis_tree <- as(eucometis_tree, "phylo4")
phylobase::plot(eucometis_tree, cex = 0.1)

# select individuals on the island and its status
eucometis_status <- data.frame(tip_labels = c("Eucometis_penicillata_FJ176069_Bel",
                                              "Eucometis_penicillata_FJ176070_Bel",
                                              "Eucometis_penicillata_FJ176071_Bel",
                                              "Eucometis_penicillata_FJ176072_Bel",
                                              "Eucometis_penicillata_FJ176073_Bel",
                                              "Eucometis_penicillata_FJ176074_Bel",
                                              "Eucometis_penicillata_FJ176075_Bel",
                                              "Eucometis_penicillata_FJ176076_Bel",
                                              "Eucometis_penicillata_FJ176077_Bel",
                                              "Eucometis_penicillata_FJ176078_Bel"),
                               tip_endemicity_status = c("nonendemic","nonendemic", "nonendemic",
                                                         "nonendemic","nonendemic", "nonendemic",
                                                         "nonendemic","nonendemic", "nonendemic",
                                                         "nonendemic"))

# set the endemicity status in the tree
eucometis_endemicity <- create_endemicity_status(
  phylo = eucometis_tree,
  island_species = eucometis_status)

eucometis_phylo <- phylobase::phylo4d(eucometis_tree, eucometis_endemicity)

#to save in RDS format
saveRDS(eucometis_phylo, file = "trees/thraupidae/eucometis_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
eucometis_phylod <- readRDS("trees/thraupidae/eucometis_rds.rds")
eucometis_phylod_asr <- add_asr_node_states(phylod = eucometis_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

eucometis_island_tbl <- extract_island_species(
  phylod = eucometis_phylod_asr, 
  extraction_method = "asr")

ars_plot(eucometis_phylod_asr, node_pies = TRUE)
eucometis_island_tbl
eucometis_island_tbl@island_tbl$branching_times
# col_time 0.71     min_age  0.39

## Lanio aurantius
lanio_tree <- ape::read.nexus("trees/thraupidae/lanio_mcct.tre")


lanio_tree <- as(lanio_tree, "phylo4")
phylobase::plot(lanio_tree, cex = 0.1)

# select individuals on the island and its status
lanio_status <- data.frame(tip_labels = c(#"Lanio_aurantius_EF529850_Hon",
                                          #"Lanio_aurantius_JN810477_Hon",
                                          "Lanio_leucothorax_JN810478"
),

tip_endemicity_status = c("nonendemic"#, "nonendemic","nonendemic"
))

# set the endemicity status in the tree
lanio_endemicity <- create_endemicity_status(
  phylo =lanio_tree,
  island_species =lanio_status)

lanio_phylo <- phylobase::phylo4d(lanio_tree,lanio_endemicity)

#to save in RDS format
saveRDS(lanio_phylo, file = "trees/thraupidae/lanio_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
lanio_phylod <- readRDS("trees/thraupidae/lanio_rds.rds")
lanio_phylod_asr <- add_asr_node_states(phylod =lanio_phylod, 
                                        asr_method = "mk", 
                                        tie_preference = "island", 
                                        earliest_col = FALSE)

lanio_island_tbl <- extract_island_species(
  phylod =lanio_phylod_asr, 
  extraction_method = "asr")

ars_plot(lanio_phylod_asr, node_pies = TRUE)
lanio_island_tbl
lanio_island_tbl@island_tbl$branching_times
# max_age 0.7582 

# Saltator maximus
saltatorm_tree <- ape::read.nexus("trees/thraupidae/saltatorm_mcct.tre")
saltatorm_tree <- as(saltatorm_tree, "phylo4")
phylobase::plot(saltatorm_tree, cex = 0.1)

# select individuals on the island and its status
saltatorm_status <- data.frame(tip_labels = c("Saltator_atriceps_KF012747_Mexico_cam",
                                              "Saltator_atriceps_KF012748_Mexico_cam",
                                              "Saltator_maximus_FJ176169_Belize",
                                              "Saltator_maximus_FJ176168_Belize",
                                              
                                              "Saltator_maximus_FJ176167_Belize",
                                            "Saltator_maximus_FJ176166_Belize",
                                              "Saltator_maximus_FJ176165_Belize",
                                              "Saltator_maximus_FJ176164_Belize",
                                               "Saltator_maximus_FJ176163_Belize",
                                              "Saltator_maximus_FJ176162_Belize",
                                              "Saltator_maximus_FJ176161_Belize",
                                              "Saltator_maximus_KF012699_Belize",
                                              "Saltator_maximus_KF012695_Honduras",
                                              "Saltator_maximus_KF012696_Honduras",
                                              "Saltator_maximus_KF012697_Nicaragua"
                                              ,"Saltator_maximus_KF012673_Panama"
),
tip_endemicity_status = c("nonendemic", "nonendemic",
                          "nonendemic", "nonendemic",
                          "nonendemic", "nonendemic",
                          "nonendemic", "nonendemic",
                          "nonendemic", "nonendemic",
                            "nonendemic", "nonendemic",
                          "nonendemic",  "nonendemic",
                          "nonendemic",
                          "nonendemic"))

# set the endemicity status in the tree
saltatorm_endemicity <- create_endemicity_status(
  phylo = saltatorm_tree,
  island_species = saltatorm_status)

saltatorm_phylo <- phylobase::phylo4d(saltatorm_tree, saltatorm_endemicity)

#to save in RDS format
saveRDS(saltatorm_phylo, file = "trees/thraupidae/saltatorm_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
saltatorm_phylod <- readRDS("trees/thraupidae/saltatorm_rds.rds")
saltatorm_phylod_asr <- add_asr_node_states(phylod = saltatorm_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

saltatorm_island_tbl <- extract_island_species(
  phylod = saltatorm_phylod_asr, 
  extraction_method = "asr")

ars_plot(saltatorm_phylod_asr, node_pies = TRUE)
saltatorm_island_tbl
# Saltator_atriceps col_time  0.115    min_age  0.07
# Saltator_maximus  col_time  0.17    min_age  0.09

# Saltator coerulescens/grandis
saltatorc_tree <- ape::read.nexus("trees/thraupidae/saltatorc_mcct.tre")
saltatorc_tree <- as(saltatorc_tree, "phylo4")
phylobase::plot(saltatorc_tree, cex = 0.1)

# select individuals on the island and its status
saltatorc_status <- data.frame(tip_labels = c("Saltator_coerulescens_KF012722_Mexico_cam"),
                               tip_endemicity_status = c("nonendemic"))

# set the endemicity status in the tree
saltatorc_endemicity <- create_endemicity_status(
  phylo = saltatorc_tree,
  island_species = saltatorc_status)

saltatorc_phylo <- phylobase::phylo4d(saltatorc_tree, saltatorc_endemicity)

#to save in RDS format
saveRDS(saltatorc_phylo, file = "trees/thraupidae/saltatorc_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
saltatorc_phylod <- readRDS("trees/thraupidae/saltatorc_rds.rds")
saltatorc_phylod_asr <- add_asr_node_states(phylod = saltatorc_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

saltatorc_island_tbl <- extract_island_species(
  phylod = saltatorc_phylod_asr, 
  extraction_method = "asr")

ars_plot(saltatorc_phylod_asr, node_pies = TRUE)
saltatorc_island_tbl
saltatorc_island_tbl@island_tbl$branching_times
# col_time  0.04 

#sporophila2 El Salvador
sporophila2_tree <- ape::read.nexus("trees/thraupidae/sporophila_mcct.tree")
sporophila2_tree <- as(sporophila2_tree, "phylo4")
phylobase::plot(sporophila2_tree, cex = 0.1)

# select individuals on the island and its status
sporophila2_status <- data.frame(tip_labels = c(
  "Sporophila_torqueola_KM894382_El_Salvador",
  "Sporophila_torqueola_KM894383_El_Salvador",
  "Sporophila_torqueola_KM894384_El_Salvador"),
  tip_endemicity_status = c( "nonendemic","nonendemic","nonendemic"  ))

# set the endemicity status in the tree
sporophila2_endemicity <- create_endemicity_status(
  phylo = sporophila2_tree,
  island_species = sporophila2_status)

sporophila2_phylo <- phylobase::phylo4d(sporophila2_tree, sporophila2_endemicity)

#to save in RDS format
saveRDS(sporophila2_phylo, file = "trees/thraupidae/sporophila2_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
sporophila2_phylod <- readRDS("trees/thraupidae/sporophila2_rds.rds")
sporophila2_phylod_asr <- add_asr_node_states(phylod = sporophila2_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

sporophila2_island_tbl <- extract_island_species(
  phylod = sporophila2_phylod_asr, 
  extraction_method = "asr")

ars_plot(sporophila2_phylod_asr, node_pies = TRUE)
sporophila2_island_tbl
sporophila2_island_tbl@island_tbl$branching_times
# Max_time 0.53

# Thraupis episcopus
thraupis_tree <- ape::read.nexus("trees/thraupidae/thraupis_mcct.tree")
thraupis_tree <- as(thraupis_tree, "phylo4")
phylobase::plot(thraupis_tree, cex = 0.1)

# select individuals on the island and its status
thraupis_status <- data.frame(tip_labels = c("Thraupis_episcopus_FJ176111_Bel",
                                             "Thraupis_episcopus_FJ176112",
                                             "Thraupis_episcopus_EF529879",
                                             
                                             "Thraupis_episcopus_FJ176110_Bel",
                                             "Thraupis_episcopus_FJ176113",
                                             "Thraupis_episcopus_FJ176114",
                                             
                                             "Thraupis_episcopus_AY329440"
),
tip_endemicity_status = c( "nonendemic", "nonendemic", "nonendemic", 
                           "nonendemic", "nonendemic", "nonendemic", "nonendemic" 
))

# set the endemicity status in the tree
thraupis_endemicity <- create_endemicity_status(
  phylo = thraupis_tree,
  island_species = thraupis_status)

thraupis_phylo <- phylobase::phylo4d(thraupis_tree, thraupis_endemicity)

#to save in RDS format
saveRDS(thraupis_phylo, file = "trees/thraupidae/thraupis_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
# To read the RDS format
thraupis_phylod <- readRDS("trees/thraupidae/thraupis_rds.rds")
thraupis_phylod_asr <- add_asr_node_states(phylod = thraupis_phylod, 
                                           asr_method = "mk", 
                                           tie_preference = "island", 
                                           earliest_col = FALSE)

thraupis_island_tbl <- extract_island_species(
  phylod = thraupis_phylod_asr, 
  extraction_method = "asr")

ars_plot(thraupis_phylod_asr, node_pies = TRUE)
thraupis_island_tbl
thraupis_island_tbl@island_tbl$branching_times
# Non_endemic_MaxAge_MinAge	0	0.29, 0.04

# Thraupis abbas
thraupisabbas_tree <- ape::read.nexus("trees/thraupidae/thraupisabbas_mcct.tree")
thraupisabbas_tree <- as(thraupisabbas_tree, "phylo4")
phylobase::plot(thraupisabbas_tree, cex = 0.1)

# select individuals on the island and its status
thraupisabbas_status <- data.frame(tip_labels = c("Thraupis_abbas_EF529857",
                                                  "Thraupis_abbas_EU648099_Honduras"),
                                   tip_endemicity_status = c("nonendemic", "nonendemic"))

# set the endemicity status in the tree
thraupisabbas_endemicity <- create_endemicity_status(
  phylo =thraupisabbas_tree,
  island_species =thraupisabbas_status)

thraupisabbas_phylo <- phylobase::phylo4d(thraupisabbas_tree,thraupisabbas_endemicity)

#to save in RDS format
saveRDS(thraupisabbas_phylo, file = "trees/thraupidae/thraupisabbas_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
thraupisabbas_phylod <- readRDS("trees/thraupidae/thraupisabbas_rds.rds")
thraupisabbas_phylod_asr <- add_asr_node_states(phylod =thraupisabbas_phylod, 
                                                asr_method = "mk", 
                                                tie_preference = "island", 
                                                earliest_col = FALSE)

thraupisabbas_island_tbl <- extract_island_species(
  phylod =thraupisabbas_phylod_asr, 
  extraction_method = "asr")

ars_plot(thraupisabbas_phylod_asr, node_pies = TRUE)
thraupisabbas_island_tbl
thraupisabbas_island_tbl@island_tbl$branching_times
# Max_age 1.34

# Tiaris olivaceus
tiaris_tree <- ape::read.nexus("trees/thraupidae/tiaris_mcct.tree")
tiaris_tree <- as(tiaris_tree, "phylo4")
phylobase::plot(tiaris_tree, cex = 0.1)

# select individuals on the island and its status
tiaris_status <- data.frame(tip_labels = c("Tiaris_olivaceus_FJ176156_Bel",
                                           "Tiaris_olivaceus_FJ176157_Bel"),
                            tip_endemicity_status = c( "nonendemic",   "nonendemic"))

# set the endemicity status in the tree
tiaris_endemicity <- create_endemicity_status(
  phylo = tiaris_tree,
  island_species = tiaris_status)

tiaris_phylo <- phylobase::phylo4d(tiaris_tree, tiaris_endemicity)

#to save in RDS format
saveRDS(tiaris_phylo, file = "trees/thraupidae/tiaris_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
tiaris_phylod <- readRDS("trees/thraupidae/tiaris_rds.rds")
tiaris_phylod_asr <- add_asr_node_states(phylod = tiaris_phylod, 
                                         asr_method = "mk", 
                                         tie_preference = "island", 
                                         earliest_col = FALSE)

tiaris_island_tbl <- extract_island_species(
  phylod = tiaris_phylod_asr, 
  extraction_method = "asr")

ars_plot(tiaris_phylod_asr, node_pies = TRUE)
tiaris_island_tbl
tiaris_island_tbl@island_tbl$branching_times
# branching_times 0.19  min_age 0.05

# Volatinia jacarina
volatinia_tree <- ape::read.nexus("trees/thraupidae/volatinia_mcct.tree")
volatinia_tree <- as(volatinia_tree, "phylo4")
phylobase::plot(volatinia_tree, cex = 0.1)

# select individuals on the island and its status
volatinia_status <- data.frame(tip_labels = c(#"Volatinia_jacarina_FJ176140",
  "Volatinia_jacarina_FJ176141"#,
  #"Volatinia_jacarina_FJ176142"
),
tip_endemicity_status = c("nonendemic" #, "nonendemic", "nonendemic"  
))

# set the endemicity status in the tree
volatinia_endemicity <- create_endemicity_status(
  phylo = volatinia_tree,
  island_species = volatinia_status)

volatinia_phylo <- phylobase::phylo4d(volatinia_tree, volatinia_endemicity)

#to save in RDS format
saveRDS(volatinia_phylo, file = "trees/thraupidae/volatinia_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
volatinia_phylod <- readRDS("trees/thraupidae/volatinia_rds.rds")
volatinia_phylod_asr <- add_asr_node_states(phylod = volatinia_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

volatinia_island_tbl <- extract_island_species(
  phylod = volatinia_phylod_asr, 
  extraction_method = "asr")

ars_plot(volatinia_phylod_asr, node_pies = TRUE)
volatinia_island_tbl
volatinia_island_tbl@island_tbl$branching_times
# col_time 0.28

#Family Tityridae
# Pachyramphus aglaiae
pachyramphus_tree <- ape::read.nexus("trees/tityridae/pachyramphus_mcct.tree")
pachyramphus_tree <- as(pachyramphus_tree, "phylo4")
phylobase::plot(pachyramphus_tree, cex = 0.1)

# select individuals on the island and its status
pachyramphus_status <- data.frame(tip_labels = c("Pachyramphus_aglaiae_KM894356_El_Salvador",
                                                 "Pachyramphus_aglaiae_KM894357_El_Salvador",
                                                 "Pachyramphus_aglaiae_KM894358_El_Salvador",
                                                 "Pachyramphus_aglaiae_KM894359_El_Salvador",
                                                 "Pachyramphus_aglaiae_KM894360_El_Salvador",
                                                 "Pachyramphus_aglaiae_JN801346_Costa_Rica"),
                                  tip_endemicity_status = c("nonendemic", "nonendemic",
                                                            "nonendemic", "nonendemic",
                                                            "nonendemic", "nonendemic"))

# set the endemicity status in the tree
pachyramphus_endemicity <- create_endemicity_status(
  phylo = pachyramphus_tree,
  island_species = pachyramphus_status)

pachyramphus_phylo <- phylobase::phylo4d(pachyramphus_tree, pachyramphus_endemicity)

#to save in RDS format
saveRDS(pachyramphus_phylo, file = "trees/tityridae/pachyramphus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
pachyramphus_phylod <- readRDS("trees/tityridae/pachyramphus_rds.rds")
pachyramphus_phylod_asr <- add_asr_node_states(phylod = pachyramphus_phylod, 
                                               asr_method = "mk", 
                                               tie_preference = "island", 
                                               earliest_col = FALSE)

pachyramphus_island_tbl <- extract_island_species(
  phylod = pachyramphus_phylod_asr, 
  extraction_method = "asr")

ars_plot(pachyramphus_phylod_asr, node_pies = TRUE)
pachyramphus_island_tbl
pachyramphus_island_tbl@island_tbl$branching_times
# max_age 1.03

# Tityra semifasciata
tityra_tree <- ape::read.nexus("trees/tityridae/tityra_mcct.tree")
tityra_tree <- as(tityra_tree, "phylo4")
phylobase::plot(tityra_tree, cex = 0.1)

# select individuals on the island and its status
tityra_status <- data.frame(tip_labels = c("Tityra_semifasciata_KM081364_Belize",
                                           "Tityra_semifasciata_KM081390_Campeche_Mexico",
                                           "Tityra_semifasciata_KM081386_SLP_Mexico",
                                           "Tityra_semifasciata_KM081375_Panama"
),
tip_endemicity_status = c( "nonendemic", "nonendemic", 
                           "nonendemic", "nonendemic"
))

# set the endemicity status in the tree
tityra_endemicity <- create_endemicity_status(
  phylo = tityra_tree,
  island_species = tityra_status)

tityra_phylo <- phylobase::phylo4d(tityra_tree, tityra_endemicity)

#to save in RDS format
saveRDS(tityra_phylo, file = "trees/tityridae/tityra_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
tityra_phylod <- readRDS("trees/tityridae/tityra_rds.rds")
tityra_phylod_asr <- add_asr_node_states(phylod = tityra_phylod, 
                                         asr_method = "mk", 
                                         tie_preference = "island", 
                                         earliest_col = FALSE)

tityra_island_tbl <- extract_island_species(
  phylod = tityra_phylod_asr, 
  extraction_method = "asr")

ars_plot(tityra_phylod_asr, node_pies = TRUE)
tityra_island_tbl
tityra_island_tbl@island_tbl$branching_times
# col_times 0.05

# Troglodytidae
# Campylorhynchus yucatanicus
campylorhynchus_tree <- ape::read.nexus("trees/troglodytidae/campylorhynchus_mcct.tree")
campylorhynchus_tree <- as(campylorhynchus_tree, "phylo4")
phylobase::plot(campylorhynchus_tree, cex = 0.1)

# select individuals on the island and its status
campylorhynchus_status <- data.frame(tip_labels = c("Campylorhynchus_yucatanicus_MZ380473",
                                                    "Campylorhynchus_yucatanicus_MZ380474",
                                                    "Campylorhynchus_yucatanicus_MZ380475",
                                                    "Campylorhynchus_yucatanicus_MZ380476"),
                                     tip_endemicity_status = c( "endemic", "endemic","endemic","endemic"  ))

# set the endemicity status in the tree
campylorhynchus_endemicity <- create_endemicity_status(
  phylo = campylorhynchus_tree,
  island_species = campylorhynchus_status)

campylorhynchus_phylo <- phylobase::phylo4d(campylorhynchus_tree, campylorhynchus_endemicity)

#to save in RDS format
saveRDS(campylorhynchus_phylo, file = "trees/troglodytidae/campylorhynchus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
campylorhynchus_phylod <- readRDS("trees/troglodytidae/campylorhynchus_rds.rds")
campylorhynchus_phylod_asr <- add_asr_node_states(phylod = campylorhynchus_phylod, 
                                                  asr_method = "mk", 
                                                  tie_preference = "island", 
                                                  earliest_col = FALSE)

campylorhynchus_island_tbl <- extract_island_species(
  phylod = campylorhynchus_phylod_asr, 
  extraction_method = "asr")

ars_plot(campylorhynchus_phylod_asr, node_pies = TRUE)
campylorhynchus_island_tbl
# min_age 0.04

# Troglodytes aedon
troglodytes_tree <- ape::read.nexus("trees/troglodytidae/troglodytes_mcct.tree")
troglodytes_tree <- as(troglodytes_tree, "phylo4")
phylobase::plot(troglodytes_tree, cex = 0.1)

# select individuals on the island and its status
troglodytes_status <- data.frame(tip_labels = c("Troglodytes_aedon_MW353112_Belice"),
                                 tip_endemicity_status = c( "nonendemic"  ))

# set the endemicity status in the tree
troglodytes_endemicity <- create_endemicity_status(
  phylo = troglodytes_tree,
  island_species = troglodytes_status)

troglodytes_phylo <- phylobase::phylo4d(troglodytes_tree, troglodytes_endemicity)

#to save in RDS format
saveRDS(troglodytes_phylo, file = "trees/troglodytidae/troglodytes_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
troglodytes_phylod <- readRDS("trees/troglodytidae/troglodytes_rds.rds")
troglodytes_phylod_asr <- add_asr_node_states(phylod = troglodytes_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

troglodytes_island_tbl <- extract_island_species(
  phylod = troglodytes_phylod_asr, 
  extraction_method = "asr")

ars_plot(troglodytes_phylod_asr, node_pies = TRUE)
troglodytes_island_tbl
troglodytes_island_tbl@island_tbl$branching_times
# branching_times 1.60

# Henicorhina_leucosticta
henicorhina_tree <- ape::read.nexus("trees/troglodytidae/henicorhina_mcct.tre")


henicorhina_tree <- as(henicorhina_tree, "phylo4")
phylobase::plot(henicorhina_tree, cex = 0.1)

# select individuals on the island and its status
henicorhina_status <- data.frame(tip_labels = c("Henicorhina_leucosticta_EU983490_Hon",
                                                "Henicorhina_leucosticta_EU983488_Hon",
                                                "Henicorhina_leucosticta_EU983487_Hon",
                                                "Henicorhina_leucosticta_EU983482_Bel",
                                                "Henicorhina_leucosticta_EU983481_Bel",
                                                "Henicorhina_leucosticta_EU983480_Bel",
                                                "Henicorhina_leucosticta_EU983479_Bel",
                                                "Henicorhina_leucosticta_EU983478_Bel"
),

tip_endemicity_status = c("nonendemic" , "nonendemic",
                          "nonendemic", "nonendemic",
                          "nonendemic", 
                          "nonendemic",
                          "nonendemic", "nonendemic"
))

# set the endemicity status in the tree
henicorhina_endemicity <- create_endemicity_status(
  phylo =henicorhina_tree,
  island_species =henicorhina_status)

henicorhina_phylo <- phylobase::phylo4d(henicorhina_tree,henicorhina_endemicity)

#to save in RDS format
saveRDS(henicorhina_phylo, file = "trees/troglodytidae/henicorhina_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
henicorhina_phylod <- readRDS("trees/troglodytidae/henicorhina_rds.rds")
henicorhina_phylod_asr <- add_asr_node_states(phylod =henicorhina_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

henicorhina_island_tbl <- extract_island_species(
  phylod =henicorhina_phylod_asr, 
  extraction_method = "asr")

ars_plot(henicorhina_phylod_asr, node_pies = TRUE)
henicorhina_island_tbl
henicorhina_island_tbl@island_tbl$branching_times
# col_time 0.65   min_age 0.17

# thryothorus
thryothorus_tree <- ape::read.nexus("trees/troglodytidae/thryothorus_mcct.tree")
thryothorus_tree <- as(thryothorus_tree, "phylo4")
phylobase::plot(thryothorus_tree, cex = 0.1)

# select individuals on the island and its status
thryothorus_status <- data.frame(tip_labels = c( "Thryothorus_ludovicianus_AY465893_Campeche"
                                                 # "Thryothorus_ludovicianus_AY460225",
                                                 #  "Thryothorus_ludovicianus_AY460224",
                                                 # "Thryothorus_ludovicianus_AY465894_USA"
),
tip_endemicity_status = c(#"nonendemic",
  #"nonendemic",
  #"nonendemic",
  "endemic"))

# set the endemicity status in the tree
thryothorus_endemicity <- create_endemicity_status(
  phylo = thryothorus_tree,
  island_species = thryothorus_status)

thryothorus_phylo <- phylobase::phylo4d(thryothorus_tree, thryothorus_endemicity)

#to save in RDS format
saveRDS(thryothorus_phylo, file = "trees/troglodytidae/thryothorus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
thryothorus_phylod <- readRDS("trees/troglodytidae/thryothorus_rds.rds")
thryothorus_phylod_asr <- add_asr_node_states(phylod = thryothorus_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

thryothorus_island_tbl <- extract_island_species(
  phylod = thryothorus_phylod_asr, 
  extraction_method = "asr")

ars_plot(thryothorus_phylod_asr, node_pies = TRUE)
thryothorus_island_tbl
thryothorus_island_tbl@island_tbl$branching_times
# Max_age 2.6

# Turdidae
#turdus
turdus_tree <- ape::read.nexus("trees/turdidae/turdus_mcct.tree")
turdus_tree <- as(turdus_tree, "phylo4")
phylobase::plot(turdus_tree, cex = 0.1)

# select individuals on the island and its status
turdus_status <- data.frame(tip_labels = c(#"Turdus_grayi_EU442302_Guatemala",
  "Turdus_grayi_KM894396_El_Salvador",
  "Turdus_grayi_KM894397_El_Salvador",
  "Turdus_grayi_KM894398_El_Salvador",
  "Turdus_grayi_HM208686_Campeche"#,
  #"Turdus_grayi_HM208687_Campeche"
),
tip_endemicity_status = c(#"nonendemic",
  "nonendemic", "nonendemic",
  "nonendemic", #"nonendemic",
  "nonendemic"))

# set the endemicity status in the tree
turdus_endemicity <- create_endemicity_status(
  phylo = turdus_tree,
  island_species = turdus_status)

turdus_phylo <- phylobase::phylo4d(turdus_tree, turdus_endemicity)

#to save in RDS format
saveRDS(turdus_phylo, file = "trees/turdidae/turdus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
turdus_phylod <- readRDS("trees/turdidae/turdus_rds.rds")
turdus_phylod_asr <- add_asr_node_states(phylod = turdus_phylod, 
                                         asr_method = "mk", 
                                         tie_preference = "island", 
                                         earliest_col = FALSE)

turdus_island_tbl <- extract_island_species(
  phylod = turdus_phylod_asr, 
  extraction_method = "asr")

ars_plot(turdus_phylod_asr, node_pies = TRUE)
turdus_island_tbl
turdus_island_tbl@island_tbl$branching_times

# col_time min_max 0.51,0.27

# Family Tyrannidae
# Attila spadiceus
attila2_tree <- ape::read.nexus("trees/tyranidae/attila_mcct.tree")
attila2_tree <- as(attila2_tree, "phylo4")
phylobase::plot(attila2_tree, cex = 0.1)

# select individuals on the island and its status
attila2_status <- data.frame(tip_labels = c(#"Attila_spadiceus_KM079734_Campeche_Mexico", 
                                            "Attila_spadiceus_KM079723_Veracruz_Mexico",
                                            "Attila_spadiceus_KM079733_Oaxaca_Mexico",
                                            
                                            "Attila_spadiceus_KM079722_Campeche_Mexico",
                                            "Attila_spadiceus_KM079727_Quintana_Roo_Mexico", 
                                            "Attila_spadiceus_KM079729_Quintana_Roo_Mexico",
                                            "Attila_spadiceus_KM079719_Campeche_Mexico",
                                            "Attila_spadiceus_KM079720_Campeche_Mexico",
                                            "Attila_spadiceus_KM079721_Campeche_Mexico",
                                            "Attila_spadiceus_KM079735_Campeche_Mexico",
                                            
                                            "Attila_spadiceus_KM079728_Panama"
),
tip_endemicity_status = c( "nonendemic","nonendemic",
                           "nonendemic","nonendemic",
                           "nonendemic","nonendemic",
                           "nonendemic", "nonendemic",
                           "nonendemic", "nonendemic"))

# set the endemicity status in the tree
attila2_endemicity <- create_endemicity_status(
  phylo = attila2_tree,
  island_species = attila2_status)

attila2_phylo <- phylobase::phylo4d(attila2_tree, attila2_endemicity)

#to save in RDS format
saveRDS(attila2_phylo, file = "trees/tyranidae/attila2_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
attila2_phylod <- readRDS("trees/tyranidae/attila2_rds.rds")
attila2_phylod_asr <- add_asr_node_states(phylod = attila2_phylod, 
                                          asr_method = "mk", 
                                          tie_preference = "island", 
                                          earliest_col = FALSE)

attila2_island_tbl <- extract_island_species(
  phylod = attila2_phylod_asr, 
  extraction_method = "asr")

ars_plot(attila2_phylod_asr, node_pies = TRUE)
attila2_island_tbl
attila2_island_tbl@island_tbl$branching_times
# col_time 0.088, 0.096, 0.036

# Elaenia flavogaster
elaenia_tree <- ape::read.nexus("trees/trees/tyranidae/elaenia_mcct.tree")
elaenia_tree <- as(elaenia_tree, "phylo4")
phylobase::plot(elaenia_tree, cex = 0.1)

# select individuals on the island and its status
elaenia_status <- data.frame(tip_labels = c("Elaenia_flavogaster_KM894321_El_Salvador",
                                            "Elaenia_flavogaster_KM894322_El_Salvador"),
                             tip_endemicity_status = c( "nonendemic", "nonendemic"  ))

# set the endemicity status in the tree
elaenia_endemicity <- create_endemicity_status(
  phylo = elaenia_tree,
  island_species = elaenia_status)

elaenia_phylo <- phylobase::phylo4d(elaenia_tree, elaenia_endemicity)

#to save in RDS format
saveRDS(elaenia_phylo, file = "trees/tyranidae/elaenia_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
elaenia_phylod <- readRDS("trees/tyranidae/elaenia_rds.rds")
elaenia_phylod_asr <- add_asr_node_states(phylod = elaenia_phylod, 
                                          asr_method = "mk", 
                                          tie_preference = "island", 
                                          earliest_col = FALSE)

elaenia_island_tbl <- extract_island_species(
  phylod = elaenia_phylod_asr, 
  extraction_method = "asr")

ars_plot(elaenia_phylod_asr, node_pies = TRUE)
elaenia_island_tbl
elaenia_island_tbl@island_tbl$branching_times
# max_age 0.716

# Myiarchus tyrannulus
myarchus_tree <- ape::read.nexus("trees/tyranidae/myarchus_mcct.tree")
myarchus_tree <- as(myarchus_tree, "phylo4")
phylobase::plot(myarchus_tree, cex = 0.1)

# select individuals on the island and its status
myarchus_status <- data.frame(tip_labels = c(
  "Myiarchus_tyrannulus_JQ004375_Yucatan_Mexico",
  "Myiarchus_tyrannulus_JQ004376_Yucatan_Mexico",
  "Myiarchus_yucatanensis_JQ004391_ENDEMIC_Mexico",
  "Myiarchus_yucatanensis_JQ004392_ENDEMIC_Mexico"
),
tip_endemicity_status = c( 
  "nonendemic","nonendemic",
  
  "nonendemic","nonendemic"))

# set the endemicity status in the tree
myarchus_endemicity <- create_endemicity_status(
  phylo = myarchus_tree,
  island_species = myarchus_status)

myarchus_phylo <- phylobase::phylo4d(myarchus_tree, myarchus_endemicity)

#to save in RDS format
saveRDS(myarchus_phylo, file = "trees/tyranidae/myarchus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
myarchus_phylod <- readRDS("trees/tyranidae/myarchus_rds.rds")
myarchus_phylod_asr <- add_asr_node_states(phylod = myarchus_phylod, 
                                           asr_method = "mk", 
                                           tie_preference = "island", 
                                           earliest_col = FALSE)

myarchus_island_tbl <- extract_island_species(
  phylod = myarchus_phylod_asr, 
  extraction_method = "asr")

ars_plot(myarchus_phylod_asr, node_pies = TRUE)
myarchus_island_tbl
myarchus_island_tbl@island_tbl$branching_times
#  Myiarchus_tyrannulus    0.68
# Myiarchus yucatanensis  1.86

# Myiarchus tuberculifer
# data * check  manually  date: 0.1026 and 0.2208 divergence from Panama
myarchus_tuber_tree <- ape::read.nexus("trees/tyranidae/myarchus_mcct.tree")
myarchus_tuber_tree <- as(myarchus_tuber_tree, "phylo4")
phylobase::plot(myarchus_tuber_tree, cex = 0.1)

# select individuals on the island and its status
myarchus_tuber_status <- data.frame(tip_labels = c("Myiarchus_tuberculifer_FJ175969_Belize",
                                                   "Myiarchus_tuberculifer_FJ175970_Belize",
                                                   "Myiarchus_tuberculifer_FJ175971_Belize",
                                                   "Myiarchus_tuberculifer_JQ004396_Yucatan_Mexico"
                                                   #	"Myiarchus_tuberculifer_FJ175972_Panama"
                                                   
),
tip_endemicity_status = c( "nonendemic",
                           "nonendemic", 
                           "nonendemic", "nonendemic"
))

# set the endemicity status in the tree
myarchus_tuber_endemicity <- create_endemicity_status(
  phylo = myarchus_tuber_tree,
  island_species = myarchus_tuber_status)

myarchus_tuber_phylo <- phylobase::phylo4d(myarchus_tuber_tree, myarchus_tuber_endemicity)

#to save in RDS format
saveRDS(myarchus_tuber_phylo, file = "trees/tyranidae/myarchus_tuber_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
myarchustub_phylod <- readRDS("trees/tyranidae/myarchus_tuber_rds.rds")
myarchustub_phylod_asr <- add_asr_node_states(phylod = myarchustub_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

myarchustub_island_tbl <- extract_island_species(
  phylod = myarchustub_phylod_asr, 
  extraction_method = "asr")

ars_plot(myarchustub_phylod_asr, node_pies = TRUE)
myarchustub_island_tbl
myarchustub_island_tbl@island_tbl$branching_times
# branching_times   0.2435 min_age 0.11 

# Myiodynastes luteiventris
myiodynastes_tree <- ape::read.nexus("trees/tyranidae/myiodynastes_mcct.tree")
myiodynastes_tree <- as(myiodynastes_tree, "phylo4")
phylobase::plot(myiodynastes_tree, cex = 0.1)

# select individuals on the island and its status
myiodynastes_status <- data.frame(tip_labels = c(
  # "Myiodynastes_luteiventris_KM894344_El_Salvador",
  #  "Myiodynastes_luteiventris_KM894345_El_Salvador",
  #  "Myiodynastes_luteiventris_KM894346_El_Salvador",
  #  "Myiodynastes_luteiventris_KM894347_El_Salvador",
  "Myiodynastes_luteiventris_DQ433040_Peru"
  #,
  #  "Myiodynastes_luteiventris_DQ433835_Nicaragua"
),
tip_endemicity_status = c(#"nonendemic","nonendemic","nonendemic",
  "nonendemic"#,"nonendemic","nonendemic"
))

# set the endemicity status in the tree
myiodynastes_endemicity <- create_endemicity_status(
  phylo = myiodynastes_tree,
  island_species = myiodynastes_status)

myiodynastes_phylo <- phylobase::phylo4d(myiodynastes_tree, myiodynastes_endemicity)

#to save in RDS format
saveRDS(myiodynastes_phylo, file = "trees/tyranidae/myiodynastes_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
myiodynastes_phylod <- readRDS("trees/tyranidae/myiodynastes_rds.rds")
myiodynastes_phylod_asr <- add_asr_node_states(phylod = myiodynastes_phylod, 
                                               asr_method = "mk", 
                                               tie_preference = "island", 
                                               earliest_col = FALSE)

myiodynastes_island_tbl <- extract_island_species(
  phylod = myiodynastes_phylod_asr, 
  extraction_method = "asr")

ars_plot(myiodynastes_phylod_asr, node_pies = TRUE)
myiodynastes_island_tbl
myiodynastes_island_tbl@island_tbl$branching_times
# col_time 0.16

# Myiopagis viridicata
myiopagis_tree <- ape::read.nexus("trees/tyranidae/myiopagis_mcct.tree")
myiopagis_tree <- as(myiopagis_tree, "phylo4")
phylobase::plot(myiopagis_tree, cex = 0.1)

# select individuals on the island and its status
myiopagis_status <- data.frame(tip_labels = c("Myiopagis_viridicata_EU310986_Campeche",
                                              
                                              "Myiopagis_viridicata_FJ175933_Belize"),
                               tip_endemicity_status = c( "nonendemic", "nonendemic"  ))

# set the endemicity status in the tree
myiopagis_endemicity <- create_endemicity_status(
  phylo = myiopagis_tree,
  island_species = myiopagis_status)

myiopagis_phylo <- phylobase::phylo4d(myiopagis_tree, myiopagis_endemicity)

#to save in RDS format
saveRDS(myiopagis_phylo, file = "trees/tyranidae/myiopagis_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
myiopagis_phylod <- readRDS("trees/tyranidae/myiopagis_rds.rds")
myiopagis_phylod_asr <- add_asr_node_states(phylod = myiopagis_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

myiopagis_island_tbl <- extract_island_species(
  phylod = myiopagis_phylod_asr, 
  extraction_method = "asr")

ars_plot(myiopagis_phylod_asr, node_pies = TRUE)
myiopagis_island_tbl
myiopagis_island_tbl@island_tbl$branching_times
# col_time 0.07   min_age 0.029

# Pitangus sulphuratus
pitangus_tree <- ape::read.nexus("trees/tyranidae/pitangus_mcct.tree")
pitangus_tree <- as(pitangus_tree, "phylo4")
phylobase::plot(pitangus_tree, cex = 0.1)

# select individuals on the island and its status
pitangus_status <- data.frame(tip_labels = c("Pitangus_sulphuratus_KM894369_El_Salvador",
                                             "Pitangus_sulphuratus_KM894370_El_Salvador",
                                             "Pitangus_sulphuratus_KM894371_El_Salvador",
                                             "Pitangus_sulphuratus_JN801364_Costa_Rica",
                                             "Pitangus_sulphuratus_JN801363_Panama"),
                              tip_endemicity_status = c("nonendemic", "nonendemic", "nonendemic", "nonendemic", "nonendemic"    ))

# set the endemicity status in the tree
pitangus_endemicity <- create_endemicity_status(
  phylo = pitangus_tree,
  island_species = pitangus_status)

pitangus_phylo <- phylobase::phylo4d(pitangus_tree, pitangus_endemicity)

#to save in RDS format
saveRDS(pitangus_phylo, file = "trees/tyranidae/pitangus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
pitangus_phylod <- readRDS("trees/tyranidae/pitangus_rds.rds")
pitangus_phylod_asr <- add_asr_node_states(phylod = pitangus_phylod, 
                                           asr_method = "mk", 
                                           tie_preference = "island", 
                                           earliest_col = FALSE)

pitangus_island_tbl <- extract_island_species(
  phylod = pitangus_phylod_asr, 
  extraction_method = "asr")

ars_plot(pitangus_phylod_asr, node_pies = TRUE)
pitangus_island_tbl
pitangus_island_tbl@island_tbl$branching_times
# branching_times 0.094 

# Tolmomyias sulphurescens
tolmomyias_tree <- ape::read.nexus("trees/tyranidae/tolmomyias_mcct.tree")
tolmomyias_tree <- as(tolmomyias_tree, "phylo4")
phylobase::plot(tolmomyias_tree, cex = 0.1)

# select individuals on the island and its status
tolmomyias_status <- data.frame(tip_labels = c("Tolmomyias_sulphurescens_JQ176523_Guyana",
                                               "Tolmomyias_sulphurescens_JQ176522_Guyana",
                                               "Tolmomyias_sulphurescens_KJ013278_Guyana"),
                                tip_endemicity_status = c( "nonendemic","nonendemic",
                                                           "nonendemic"))

# set the endemicity status in the tree
tolmomyias_endemicity <- create_endemicity_status(
  phylo = tolmomyias_tree,
  island_species = tolmomyias_status)

tolmomyias_phylo <- phylobase::phylo4d(tolmomyias_tree, tolmomyias_endemicity)

#to save in RDS format
saveRDS(tolmomyias_phylo, file = "trees/tyranidae/tolmomyias_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
tolmomyias_phylod <- readRDS("trees/tyranidae/tolmomyias_rds.rds")
tolmomyias_phylod_asr <- add_asr_node_states(phylod = tolmomyias_phylod, 
                                             asr_method = "mk", 
                                             tie_preference = "island", 
                                             earliest_col = FALSE)

tolmomyias_island_tbl <- extract_island_species(
  phylod = tolmomyias_phylod_asr, 
  extraction_method = "asr")

ars_plot(tolmomyias_phylod_asr, node_pies = TRUE)
tolmomyias_island_tbl
tolmomyias_island_tbl@island_tbl$branching_times
# Max 1.410318

# Poecilotriccus sylvia
poecilotriccus_tree <- ape::read.nexus("trees/tyranidae/poecilotriccus_mcct.tree")
poecilotriccus_tree <- as(poecilotriccus_tree, "phylo4")
phylobase::plot(poecilotriccus_tree, cex = 0.1)

# select individuals on the island and its status
poecilotriccus_status <- data.frame(tip_labels = c(#"Poecilotriccus_sylvia_FJ175935_Belize",
  #"Poecilotriccus_sylvia_FJ175936_Belize",
  #"Poecilotriccus_sylvia_FJ175937_Belize",
  #"Poecilotriccus_sylvia_FJ175938_Belize",
  "Poecilotriccus_sylvia_FJ175939_Belize"),
  tip_endemicity_status = c(#"nonendemic", "nonendemic","nonendemic",
    #"nonendemic",
    "nonendemic"))

# set the endemicity status in the tree
poecilotriccus_endemicity <- create_endemicity_status(
  phylo = poecilotriccus_tree,
  island_species = poecilotriccus_status)

poecilotriccus_phylo <- phylobase::phylo4d(poecilotriccus_tree, poecilotriccus_endemicity)

#to save in RDS format
saveRDS(poecilotriccus_phylo, file = "trees/tyranidae/poecilotriccus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)


# To read the RDS format
poecilotriccus_phylod <- readRDS("trees/tyranidae/poecilotriccus_rds.rds")
poecilotriccus_phylod_asr <- add_asr_node_states(phylod = poecilotriccus_phylod, 
                                                 asr_method = "mk", 
                                                 tie_preference = "island", 
                                                 earliest_col = FALSE)

poecilotriccus_island_tbl <- extract_island_species(
  phylod = poecilotriccus_phylod_asr, 
  extraction_method = "asr")

ars_plot(poecilotriccus_phylod_asr, node_pies = TRUE)
poecilotriccus_island_tbl
poecilotriccus_island_tbl@island_tbl$branching_times
# branching_times 0.39  min_age 0.10

# Todirostrum cinereum
todirostrum_tree <- ape::read.nexus("trees/tyranidae/todirostrum_mcct.tree")
todirostrum_tree <- as(todirostrum_tree, "phylo4")
phylobase::plot(todirostrum_tree, cex = 0.1)

# select individuals on the island and its status
todirostrum_status <- data.frame(tip_labels = c(
  "Todirostrum_cinereum_KM894392_El_Salvador",
  "Todirostrum_cinereum_KM894391_El_Salvador",
  "Todirostrum_cinereum_JQ176507_Panama"),
  tip_endemicity_status = c("nonendemic", "nonendemic", "nonendemic" ))

# set the endemicity status in the tree
todirostrum_endemicity <- create_endemicity_status(
  phylo = todirostrum_tree,
  island_species = todirostrum_status)

todirostrum_phylo <- phylobase::phylo4d(todirostrum_tree, todirostrum_endemicity)

#to save in RDS format
saveRDS(todirostrum_phylo, file = "trees/tyranidae/todirostrum_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
todirostrum_phylod <- readRDS("trees/tyranidae/todirostrum_rds.rds")
todirostrum_phylod_asr <- add_asr_node_states(phylod = todirostrum_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

todirostrum_island_tbl <- extract_island_species(
  phylod = todirostrum_phylod_asr, 
  extraction_method = "asr")

ars_plot(todirostrum_phylod_asr, node_pies = TRUE)
todirostrum_island_tbl
todirostrum_island_tbl@island_tbl$branching_times
# Max_age 0.89

# Tyrannus melancholicus
tyrannus_tree <- ape::read.nexus("trees/tyranidae/tyrannus_mcct.tree")
tyrannus_tree <- as(tyrannus_tree, "phylo4")
phylobase::plot(tyrannus_tree, cex = 0.1)

# select individuals on the island and its status
tyrannus_status <- data.frame(tip_labels = c(#"Tyrannus_melancholicus_KM894399_El_Salvador",
  #"Tyrannus_melancholicus_KM894400_El_Salvador",
  #"Tyrannus_melancholicus_KM894401_El_Salvador",
  "Tyrannus_dominicensis_DQ434207",
  "Tyrannus_couchii_DQ433246_Mexico_Yucatan"),
  tip_endemicity_status = c(# "nonendemic","nonendemic",
    "nonendemic",#"nonendemic" ,
    "nonendemic" ))

# set the endemicity status in the tree
tyrannus_endemicity <- create_endemicity_status(
  phylo = tyrannus_tree,
  island_species = tyrannus_status)

tyrannus_phylo <- phylobase::phylo4d(tyrannus_tree, tyrannus_endemicity)

#to save in RDS format
saveRDS(tyrannus_phylo, file = "trees/tyranidae/tyrannus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
tyrannus_phylod <- readRDS("trees/tyranidae/tyrannus_rds.rds")
tyrannus_phylod_asr <- add_asr_node_states(phylod = tyrannus_phylod, 
                                           asr_method = "mk", 
                                           tie_preference = "island", 
                                           earliest_col = FALSE)

tyrannus_island_tbl <- extract_island_species(
  phylod = tyrannus_phylod_asr, 
  extraction_method = "asr")

ars_plot(tyrannus_phylod_asr, node_pies = TRUE)
tyrannus_island_tbl
tyrannus_island_tbl@island_tbl$branching_times
# Tyrannus_couchii        col_time  0.17
# Tyrannus_dominicensis/Tyrannus melancholicus max_age 1.02

# Myiobius sulphureipygius
myiobius_tree <- ape::read.nexus("trees/tyranidae/myiobius_mcct.tree")
myiobius_tree <- as(myiobius_tree, "phylo4")
phylobase::plot(myiobius_tree, cex = 0.1)

# select individuals on the island and its status
myiobius_status <- data.frame(tip_labels = c("Myiobius_sulphureipygius_FJ175957_Belize",
                                             "Myiobius_sulphureipygius_FJ175958_Belize",			
                                             "Myiobius_sulphureipygius_FJ175959_Belize"),
                              tip_endemicity_status = c("nonendemic", "nonendemic", "nonendemic"   ))

# set the endemicity status in the tree
myiobius_endemicity <- create_endemicity_status(
  phylo = myiobius_tree,
  island_species = myiobius_status)

myiobius_phylo <- phylobase::phylo4d(myiobius_tree, myiobius_endemicity)

#to save in RDS format
saveRDS(myiobius_phylo, file = "trees/tyranidae/myiobius_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
# To read the RDS format
myiobius_phylod <- readRDS("trees/tyranidae/myiobius_rds.rds")
myiobius_phylod_asr <- add_asr_node_states(phylod = myiobius_phylod, 
                                           asr_method = "mk", 
                                           tie_preference = "island", 
                                           earliest_col = FALSE)

myiobius_island_tbl <- extract_island_species(
  phylod = myiobius_phylod_asr, 
  extraction_method = "asr")

ars_plot(myiobius_phylod_asr, node_pies = TRUE)
myiobius_island_tbl
myiobius_island_tbl@island_tbl$branching_times
# branching_times 0.740121 min_age 0.14

# Megarynchus pitangua
megarynchus_tree <- ape::read.nexus("trees/tyranidae/megarhyn_myioz_mcct.tree")
megarynchus_tree <- as(megarynchus_tree, "phylo4")
phylobase::plot(megarynchus_tree, cex = 0.1)

# select individuals on the island and its status
megarynchus_status <- data.frame(tip_labels = c("Megarynchus_pitangua_FJ175974_Bel"),
                                 tip_endemicity_status = c("nonendemic"#,"nonendemic"
                                 ))

# set the endemicity status in the tree
megarynchus_endemicity <- create_endemicity_status(
  phylo =megarynchus_tree,
  island_species =megarynchus_status)

megarynchus_phylo <- phylobase::phylo4d(megarynchus_tree,megarynchus_endemicity)

#to save in RDS format
saveRDS(megarynchus_phylo, file = "trees/tyranidae/megarynchus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
megarynchus_phylod <- readRDS("trees/tyranidae/megarynchus_rds.rds")
megarynchus_phylod_asr <- add_asr_node_states(phylod =megarynchus_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

megarynchus_island_tbl <- extract_island_species(
  phylod =megarynchus_phylod_asr, 
  extraction_method = "asr")

ars_plot(megarynchus_phylod_asr, node_pies = TRUE)
megarynchus_island_tbl
megarynchus_island_tbl@island_tbl$branching_times
#  branching_times  0.035

# Myiozetetes similis
myiozetetes_tree <- ape::read.nexus("trees/tyranidae/megarhyn_myioz_mcct.tree")
myiozetetes_tree <- as(myiozetetes_tree, "phylo4")
phylobase::plot(myiozetetes_tree, cex = 0.1)

# select individuals on the island and its status
myiozetetes_status <- data.frame(tip_labels = c( "Myiozetetes_similis_FJ175978_Bel",
                                                 "Myiozetetes_similis_FJ175977_Bel"),
                                 tip_endemicity_status = c("nonendemic", "nonendemic"))

# set the endemicity status in the tree
myiozetetes_endemicity <- create_endemicity_status(
  phylo =myiozetetes_tree,
  island_species =myiozetetes_status)

myiozetetes_phylo <- phylobase::phylo4d(myiozetetes_tree,myiozetetes_endemicity)

#to save in RDS format
saveRDS(myiozetetes_phylo, file = "trees/tyranidae/myiozetetes_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
myiozetetes_phylod <- readRDS("trees/tyranidae/myiozetetes_rds.rds")
myiozetetes_phylod_asr <- add_asr_node_states(phylod =myiozetetes_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

myiozetetes_island_tbl <- extract_island_species(
  phylod =myiozetetes_phylod_asr, 
  extraction_method = "asr")

ars_plot(myiozetetes_phylod_asr, node_pies = TRUE)
myiozetetes_island_tbl
myiozetetes_island_tbl@island_tbl$branching_times
# branching_times 1.743 min_age  0.090

# Platyrinchus cancrominus
platyrinchus_tree <- ape::read.nexus("trees/tyranidae/platyrinchus_mcct.tree")
platyrinchus_tree <- as(platyrinchus_tree, "phylo4")
phylobase::plot(platyrinchus_tree, cex = 0.1)

# select individuals on the island and its status
platyrinchus_status <- data.frame(tip_labels = c("Platyrinchus_cancrominus_JQ175894_Pan",
                                                 "Platyrinchus_cancrominus_JQ175892_Pan",
                                                 "Platyrinchus_cancrominus_JQ175893_Pan"),
                                  tip_endemicity_status = c("nonendemic",
                                                            "nonendemic",
                                                            "nonendemic"))

# set the endemicity status in the tree
platyrinchus_endemicity <- create_endemicity_status(
  phylo =platyrinchus_tree,
  island_species =platyrinchus_status)

platyrinchus_phylo <- phylobase::phylo4d(platyrinchus_tree,platyrinchus_endemicity)

#to save in RDS format
saveRDS(platyrinchus_phylo, file = "trees/tyranidae/platyrinchus_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
platyrinchus_phylod <- readRDS("trees/tyranidae/platyrinchus_rds.rds")
platyrinchus_phylod_asr <- add_asr_node_states(phylod =platyrinchus_phylod, 
                                               asr_method = "mk", 
                                               tie_preference = "island", 
                                               earliest_col = FALSE)

platyrinchus_island_tbl <- extract_island_species(
  phylod =platyrinchus_phylod_asr, 
  extraction_method = "asr")

ars_plot(platyrinchus_phylod_asr, node_pies = TRUE)
platyrinchus_island_tbl
platyrinchus_island_tbl@island_tbl$branching_times
# max_age 1.46

# Mionectes
mionectes_tree <- ape::read.nexus("trees/tyranidae/mionectes_mcct.tree")
mionectes_tree <- as(mionectes_tree, "phylo4")
phylobase::plot(mionectes_tree, cex = 0.1)

# select individuals on the island and its status
mionectes_status <- data.frame(tip_labels = c(
  "Mionectes_oleagineus_EF110725_Belice",
  "Mionectes_oleagineus_EF110730_Belice"
  #,
  #"Mionectes_oleagineus_EF110719_Ver"
),
tip_endemicity_status = c(#"nonendemic", 
  "nonendemic", "nonendemic"))

# set the endemicity status in the tree
mionectes_endemicity <- create_endemicity_status(
  phylo = mionectes_tree,
  island_species = mionectes_status)

mionectes_phylo <- phylobase::phylo4d(mionectes_tree, mionectes_endemicity)

#to save in RDS format
saveRDS(mionectes_phylo, file = "trees/tyranidae/mionectes_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format 
mionectes_phylod <- readRDS("trees/tyranidae/mionectes_rds.rds")
mionectes_phylod_asr <- add_asr_node_states(phylod = mionectes_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

mionectes_island_tbl <- extract_island_species(
  phylod = mionectes_phylod_asr, 
  extraction_method = "asr")

ars_plot(mionectes_phylod_asr, node_pies = TRUE)
mionectes_island_tbl
mionectes_island_tbl@island_tbl$branching_times
# col_time   0.06      min_age   0.028

# Family Virionidae
# Cyclarhis gujanensis
cyclarhis_tree <- ape::read.nexus("trees/virionidae/cyclarhis_mcct.tree")
cyclarhis_tree <- as(cyclarhis_tree, "phylo4")
phylobase::plot(cyclarhis_tree, cex = 0.1)

# select individuals on the island and its status
cyclarhis_status <- data.frame(tip_labels = c("Cyclarhis_gujanensis_KM115204_Yucatan_Mexico",
                                              "Cyclarhis_gujanensis_KM115205_Yucatan_Mexico",
                                              "Cyclarhis_gujanensis_KM115206_Campeche_Mexico"),
                               tip_endemicity_status = c("nonendemic","nonendemic","nonendemic"   ))

# set the endemicity status in the tree
cyclarhis_endemicity <- create_endemicity_status(
  phylo = cyclarhis_tree,
  island_species = cyclarhis_status)

cyclarhis_phylo <- phylobase::phylo4d(cyclarhis_tree, cyclarhis_endemicity)

#to save in RDS format
saveRDS(cyclarhis_phylo, file = "trees/virionidae/cyclarhis_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
cyclarhis_phylod <- readRDS("trees/virionidae/cyclarhis_rds.rds")
cyclarhis_phylod_asr <- add_asr_node_states(phylod = cyclarhis_phylod, 
                                            asr_method = "mk", 
                                            tie_preference = "island", 
                                            earliest_col = FALSE)

cyclarhis_island_tbl <- extract_island_species(
  phylod = cyclarhis_phylod_asr, 
  extraction_method = "asr")

ars_plot(cyclarhis_phylod_asr, node_pies = TRUE)
cyclarhis_island_tbl
cyclarhis_island_tbl@island_tbl$branching_times
# branching_times 1.67  min_age  0.32

# Pachysylvia decurtata
pachysylvia_tree <- ape::read.nexus("trees/virionidae/pachysylvia_mcct.tree")
pachysylvia_tree <- as(pachysylvia_tree, "phylo4")
phylobase::plot(pachysylvia_tree, cex = 0.1)

# select individuals on the island and its status
pachysylvia_status <- data.frame(tip_labels = c("Hylophilus_decurtatus_FJ176014_Belize",
                                                "Hylophilus_decurtatus_FJ176015_Belize",
                                                "Hylophilus_decurtatus_FJ176016_Belize",
                                                "Hylophilus_decurtatus_KM115225_Belize"),
                                 tip_endemicity_status = c( "nonendemic", "nonendemic", "nonendemic", "nonendemic"))

# set the endemicity status in the tree
pachysylvia_endemicity <- create_endemicity_status(
  phylo = pachysylvia_tree,
  island_species = pachysylvia_status)

pachysylvia_phylo <- phylobase::phylo4d(pachysylvia_tree, pachysylvia_endemicity)

#to save in RDS format
saveRDS(pachysylvia_phylo, file = "trees/virionidae/pachysylvia_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
pachysylvia_phylod <- readRDS("trees/virionidae/pachysylvia_rds.rds")
pachysylvia_phylod_asr <- add_asr_node_states(phylod = pachysylvia_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

pachysylvia_island_tbl <- extract_island_species(
  phylod = pachysylvia_phylod_asr, 
  extraction_method = "asr")

ars_plot(pachysylvia_phylod_asr, node_pies = TRUE)
pachysylvia_island_tbl
pachysylvia_island_tbl@island_tbl$branching_times
# branching_times   0.19   min_age  0.13

# Tunchiornis ochraceiceps
tunchiornis_tree <- ape::read.nexus("trees/virionidae/tunchiornis_mcct.tree")
tunchiornis_tree <- as(tunchiornis_tree, "phylo4")
phylobase::plot(tunchiornis_tree, cex = 0.1)

# select individuals on the island and its status
tunchiornis_status <- data.frame(tip_labels = c("Hylophilus_ochraceiceps_KM115242_Belize",
                                                "Hylophilus_ochraceiceps_KM115259_Panama"),
                                 tip_endemicity_status = c( "nonendemic", "nonendemic"))

# set the endemicity status in the tree
tunchiornis_endemicity <- create_endemicity_status(
  phylo = tunchiornis_tree,
  island_species = tunchiornis_status)

tunchiornis_phylo <- phylobase::phylo4d(tunchiornis_tree, tunchiornis_endemicity)

#to save in RDS format
saveRDS(tunchiornis_phylo, file = "trees/virionidae/tunchiornis_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
tunchiornis_phylod <- readRDS("trees/virionidae/tunchiornis_rds.rds")
tunchiornis_phylod_asr <- add_asr_node_states(phylod = tunchiornis_phylod, 
                                              asr_method = "mk", 
                                              tie_preference = "island", 
                                              earliest_col = FALSE)

tunchiornis_island_tbl <- extract_island_species(
  phylod = tunchiornis_phylod_asr, 
  extraction_method = "asr")

ars_plot(tunchiornis_phylod_asr, node_pies = TRUE)
tunchiornis_island_tbl
tunchiornis_island_tbl@island_tbl$branching_times
# 0.038

# Vireo magister
vireom_tree <- ape::read.nexus("trees/virionidae/vireom_mcct.tree")
vireom_tree <- as(vireom_tree, "phylo4")
phylobase::plot(vireom_tree, cex = 0.1)

# select individuals on the island and its status
vireom_status <- data.frame(tip_labels = c("Vireo_magister_KM115351_Endemic_Mexico"),
                            tip_endemicity_status = c(  "endemic" ))

# set the endemicity status in the tree
vireom_endemicity <- create_endemicity_status(
  phylo = vireom_tree,
  island_species = vireom_status)

vireom_phylo <- phylobase::phylo4d(vireom_tree, vireom_endemicity)

#to save in RDS format
saveRDS(vireom_phylo, file = "trees/virionidae/vireom_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
vireom_phylod <- readRDS("trees/virionidae/vireom_rds.rds")
vireom_phylod_asr <- add_asr_node_states(phylod = vireom_phylod, 
                                         asr_method = "mk", 
                                         tie_preference = "island", 
                                         earliest_col = FALSE)

vireom_island_tbl <- extract_island_species(
  phylod = vireom_phylod_asr, 
  extraction_method = "asr")

ars_plot(vireom_phylod_asr, node_pies = TRUE)
vireom_island_tbl
vireom_island_tbl@island_tbl$branching_times
# branching_times   2.17

# Vireo pallens
vireop_tree <- ape::read.nexus("trees/virionidae/vireop_mcct.tree")
vireop_tree <- as(vireop_tree, "phylo4")
phylobase::plot(vireop_tree, cex = 0.1)

# select individuals on the island and its status
vireop_status <- data.frame(tip_labels = c("Vireo_pallens_KM115381_Yucatan_Mexico",
                                           "Vireo_pallens_KM115382_Yucatan_Mexico"),
                            tip_endemicity_status = c("nonendemic", "nonendemic"   ))

# set the endemicity status in the tree
vireop_endemicity <- create_endemicity_status(
  phylo = vireop_tree,
  island_species = vireop_status)

vireop_phylo <- phylobase::phylo4d(vireop_tree, vireop_endemicity)

#to save in RDS format
saveRDS(vireop_phylo, file = "trees/virionidae/vireop_rds.rds", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

# To read the RDS format
vireop_phylod <- readRDS("trees/virionidae/vireop_rds.rds")
vireop_phylod_asr <- add_asr_node_states(phylod = vireop_phylod, 
                                         asr_method = "mk", 
                                         tie_preference = "island", 
                                         earliest_col = FALSE)

vireop_island_tbl <- extract_island_species(
  phylod = vireop_phylod_asr, 
  extraction_method = "asr")

ars_plot(vireop_phylod_asr, node_pies = TRUE)
vireop_island_tbl
vireop_island_tbl@island_tbl$branching_times
# branching_times   2.69   min_age 0.13


#####################################################################
# function for plotting
ars_plot <- function (phylod, node_pies = FALSE) 
{
  node_pie_data <- all(c("nonendemic_prob", "not_present_prob") %in% 
                         names(phylobase::tdata(phylod)))
  if (node_pies && !node_pie_data) {
    stop("To plot probabilities in at the nodes they must be in phylod")
  }
  endemicity_status <- NULL
  rm(endemicity_status)
  island_status <- NULL
  rm(island_status)
  p <- suppressMessages(ggtree::ggtree(phylod) + ggtree::theme_tree2() + 
                          ggtree::geom_tiplab(as_ylab = TRUE))
  p <- suppressMessages(ggtree::revts(treeview = p) + ggplot2::scale_x_continuous(labels = abs) + 
                          ggplot2::xlab("Time (Million years ago)"))
  if (!is.null(phylobase::nodeData(phylod)$endemicity_status)) {
    p <- p + ggtree::geom_tippoint(ggplot2::aes(colour = endemicity_status), 
                                   size = 3, ) + ggplot2::labs(colour = "Endemicity status")
  }
  if (isTRUE(node_pies)) {
    if ("endemic_prob" %in% names(phylobase::tdata(phylod))) {
      states <- c("endemic_prob", "nonendemic_prob", "not_present_prob")
    }
    else {
      states <- c("nonendemic_prob", "not_present_prob")
    }
    node_pies <- phylobase::nodeData(phylod)[, states]
    node_pies <- cbind(node_pies, node = rownames(node_pies))
    pies <- ggtree::nodepie(node_pies, cols = 1:2)
    p <- p + ggtree::geom_inset(insets = pies, width = 0.02, 
                                height = 0.02, )
  }
  else if (!is.null(phylobase::nodeData(phylod)$island_status)) {
    p <- p + ggtree::geom_nodepoint(ggplot2::aes(colour = island_status), 
                                    size = 1)
  }
  p
}

dump("ars_plot", file = "ars_plot.R")

