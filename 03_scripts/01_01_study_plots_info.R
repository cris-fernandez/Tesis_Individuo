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

