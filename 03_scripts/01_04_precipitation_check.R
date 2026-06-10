rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading plots ####

plots <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/02_clean_data/02_01_clean_plot.csv", 
                  header = T, sep = ",")

# Since I am just interested on the coordinates, I will select the needed columns

plots <- plots %>% dplyr::select(c(plot_id, spot_status, sp_name,
                                   region)) %>% 
  mutate(sp_id = ifelse(sp_name == "Abies alba", "Abialba",
                        ifelse(sp_name == "Pinus sylvestris", "Pinsylv", "Pinpine")),
         region_id = substr(region, 1, 3),
         pair_id = paste0(region_id, "-", sp_id))

# 2.- Reading climate ####

climate_data_series <- read.csv("02_clean_data/02_00_climate_series.csv") %>% 
  dplyr::select(-X)

# After review comment by Antonio, some decreases in final precipitation year 
# might be explained by the incompleteness of that same year, as data from 
# 2022 gives way too low precipitation in the pyrenees

load("C:/Users/recup/Desktop/TESIS/PAPERS/01_INDIVIDUO/DATA/workspace_easyclimate.RData")
precipitation$date <- as.Date(precipitation$date, format =  "%Y-%m-%d")
precipitation$year <- year(precipitation$date)
precipitation$month <- month(precipitation$date)

prec_21_22 <- precipitation %>% group_by(lat, lon, year, month) %>% summarise(preci = sum(Prcp))
