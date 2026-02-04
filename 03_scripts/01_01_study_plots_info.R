rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading target data ####

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv", 
                         header = T, sep = ",") %>% select(-X) %>% 
  mutate(site = substr(plot_id, 1, 3))
# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3)) # PC office

clean_target$pair_id <- ifelse(grepl("NAV|PEL", clean_target$plot_id) == T, "Mad-Pinpine",
                               ifelse(grepl("GUA", clean_target$plot_id) == T, "Mad-Pinsylv",
                                      ifelse(grepl("ADO|TRA|ALU", clean_target$plot_id) == T, "Gua-Pinsylv",
                                             ifelse(grepl("COR|CED", clean_target$plot_id) == T, "Ter-Pinsylv",
                                                    ifelse(grepl("RON|URZ", clean_target$plot_id) == T, "Nav-Pinsylv",
                                                           ifelse(grepl("BAS|SAR", clean_target$plot_id) == T, "Nav-Abialba",
                                                                  ifelse(grepl("FAG|OZA", clean_target$plot_id) == T, "Hue-Abialba",
                                                                         "z")))))))

# 2.- Number of plots per species ####

psylv <- clean_target %>% 
  select(c(plot_id, sp_id)) %>% 
  filter(sp_id == "Pinsylv") %>% 
  unique()
ppine <- clean_target %>% 
  select(c(plot_id, sp_id)) %>% 
  filter(sp_id == "Pinpine") %>% 
  unique()
aalba <- clean_target %>% 
  select(c(plot_id, sp_id)) %>% 
  filter(sp_id == "Abialba") %>% 
  unique() 
# We have 52 P. sylvestris, 17 P. pinea and 23 A. alba plots :)

# 3.- Soil data ####
# Soil data will be retrieved per site so it can be added to the manuscript

soils <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/01_raw_data/01_07_raw_soils.csv") %>% 
  mutate(plot_id = substring(tree_id, 1, 5),
         CaCO3_perc = ifelse(CaCO3_perc == "<L.D.(0,058)", 0, CaCO3_perc)) %>% 
  dplyr::select(-c(soil_sample, USDA_class, description, tree_id))
# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/01_raw_data/01_07_raw_soils.csv"",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3)) # PC office

soils$CaCO3_perc <- as.numeric(soils$CaCO3_perc)

# Adding site info from clean_target

sites <- clean_target %>% dplyr::select(c(tree_number, pair_id,
                                          spot_status))

soils_sites <- full_join(soils, sites, by = "tree_number")

# 4.- Summarising soil data ####

soils_mean <- soils_sites %>% group_by(pair_id, spot_status) %>% 
  summarise(across(clay_perc:Mg_ppm, ~mean(.x, na.rm=T)))

soils_min <- soils_sites %>% group_by(pair_id, spot_status) %>% 
  summarise(across(clay_perc:Mg_ppm, ~quantile(.x, .025, na.rm=T)))

soils_max <- soils_sites %>% group_by(pair_id, spot_status) %>% 
  summarise(across(clay_perc:Mg_ppm, ~quantile(.x, .975, na.rm=T)))

soils_sd <- soils_sites %>% group_by(pair_id, spot_status) %>% 
  summarise(across(clay_perc:Mg_ppm, ~sd(.x, na.rm=T)))
