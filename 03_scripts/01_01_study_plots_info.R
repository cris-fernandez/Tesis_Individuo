rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd("C:/Users/recup/Desktop/TESIS/FOREST_SYSTEMS")
setwd('..')
getwd()

IBF_data <- read.csv("FOREST_SYSTEMS/00_IB_ForRes_plot_data.csv", header = T) %>% 
  dplyr::select(-X)
summary(IBF_data$dominance)