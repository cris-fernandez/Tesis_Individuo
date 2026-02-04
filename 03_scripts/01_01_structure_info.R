# Carlota's TFM Data Export

# Date: 26/02/2025
# This script contains variables extracted at plot level in our field sampling
# campaign in IB-ForRes
# Data were taken between Feb. 2023 and Sep. 2023

rm(list=ls()) # Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] # new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading tree data ####

# clean_tree <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/02_clean_data/02_02_clean_tree.csv", header = T)
clean_tree <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/02_clean_data/02_02_clean_tree.csv", header = T, sep = ",") # PC office

clean_tree <- clean_tree %>% unique() #Some repeated observations...

# Adding pair_id:

clean_tree$pair_id <- ifelse(grepl("NAV|PEL", clean_tree$plot_id) == T, "Mad-Pinpine",
                               ifelse(grepl("GUA", clean_tree$plot_id) == T, "Mad-Pinsylv",
                                      ifelse(grepl("ADO|TRA|ALU", clean_tree$plot_id) == T, "Gua-Pinsylv",
                                             ifelse(grepl("COR|CED", clean_tree$plot_id) == T, "Ter-Pinsylv",
                                                    ifelse(grepl("RON|URZ", clean_tree$plot_id) == T, "Nav-Pinsylv",
                                                           ifelse(grepl("BAS|SAR", clean_tree$plot_id) == T, "Nav-Abialba",
                                                                  ifelse(grepl("FAG|OZA", clean_tree$plot_id) == T, "Hue-Abialba",
                                                                         "z")))))))

# Extracting pair list:

pairs <- clean_tree %>% dplyr::select(c(plot_id, pair_id)) %>% unique()

# 3.- BA values ####

clean_tree$ba <- pi * (clean_tree$dbh/2)^2

basim <- clean_tree %>% 
  group_by(plot_id) %>% 
  summarise(abs_ba = sum(ba)) %>% 
  mutate(abs_ba_ha = abs_ba/(pi * (17^2)))

basim <- full_join(pairs, basim, by = "plot_id")

# 4.- Mean, sd, min, max ####

ba_mean <- basim %>% group_by(pair_id) %>% 
  summarise(mean_ba_ha = mean(abs_ba_ha, na.rm = T))
ba_sd <- basim %>% group_by(pair_id) %>% 
  summarise(sd_ba_ha = sd(abs_ba_ha, na.rm = T))
ba_min <- basim %>% group_by(pair_id) %>% 
  summarise(min_ba_ha = quantile(abs_ba_ha, .025, na.rm = T))
ba_max <- basim %>% group_by(pair_id) %>% 
  summarise(max_ba_ha = quantile(abs_ba_ha, .975, na.rm = T))

# 5.- Density values ####

density <- clean_tree %>% 
  group_by(plot_id) %>% 
  summarise(tree_number = n()) %>% 
  mutate(stand_dens = (tree_number * 10000)/(pi * (17^2)))

density <- full_join(pairs, density, by = "plot_id")

# 6.- Mean, sd, min, max ####

dens_mean <- density %>% group_by(pair_id) %>% 
  summarise(mean_dens = mean(stand_dens, na.rm = T))
dens_sd <- density %>% group_by(pair_id) %>% 
  summarise(sd_dens = sd(stand_dens, na.rm = T))
dens_min <- density %>% group_by(pair_id) %>% 
  summarise(min_dens = quantile(stand_dens, .025, na.rm = T))
dens_max <- density %>% group_by(pair_id) %>% 
  summarise(max_dens = quantile(stand_dens, .975, na.rm = T))