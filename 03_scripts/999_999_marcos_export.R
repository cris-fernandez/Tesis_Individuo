rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "ggplot2",
        "FactoMineR", "factoextra", "png", "maps", "sf", "rnaturalearth",
        "rnaturalearthdata", "mapSpain", "ggspatial", "cowplot") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

# install.packages("rnaturalearthdata")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Sites coordinates ####

sites <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_01_result_plot.csv", 
                  header = T, sep = ",") %>% 
  dplyr::select(c(plot_id, site, geo_lat, geo_lon, sp_id, spot_status, pair_id)) %>% 
  mutate(sp_id = ifelse(sp_id == "Pinpine", "Pinuspinea",
                        ifelse(sp_id == "Abialba", "Abiesalba", 
                               "Pinussylvestris")),
         sp_id = gsub("Pinussylvestris", "Pinsylv", sp_id)) %>% 
  filter(sp_id == "Pinsylv") 

write.csv(sites, "02_clean_data/coordenadas_psylvestris.csv")

