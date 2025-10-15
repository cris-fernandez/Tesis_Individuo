rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "lme4", "lmerTest", "emmeans") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading all model2 data ####

model2_all <- read.csv("02_clean_data/40_01_models_2way.csv") %>% 
  dplyr::select(c(variable, p_val_hot)) %>% 
  rename(p_all = p_val_hot)

model2_aa <- read.csv("02_clean_data/40_02_models_2way_aa.csv") %>% 
  dplyr::select(c(variable, p_val_hot)) %>% 
  rename(p_aa = p_val_hot)

model2_ps <- read.csv("02_clean_data/40_03_models_2way_ps.csv") %>% 
  dplyr::select(c(variable, p_val_hot)) %>% 
  rename(p_ps = p_val_hot)

model2_pp <- read.csv("02_clean_data/40_04_models_2way_pp.csv") %>% 
  dplyr::select(c(variable, p_val_hot)) %>% 
  rename(p_pp = p_val_hot)

# 2.- Reading all model3 data ####

model3_all <- read.csv("02_clean_data/40_05_models_3way.csv") %>% 
  dplyr::select(c(variable, p_val_damaged)) %>% 
  rename(p_all = p_val_damaged)

model3_aa <- read.csv("02_clean_data/40_06_models_3way_aa.csv") %>% 
  dplyr::select(c(variable, p_val_damaged)) %>% 
  rename(p_aa = p_val_damaged)

model3_ps <- read.csv("02_clean_data/40_07_models_3way_ps.csv") %>% 
  dplyr::select(c(variable, p_val_damaged)) %>% 
  rename(p_ps = p_val_damaged)

model3_pp <- read.csv("02_clean_data/40_08_models_3way_pp.csv") %>% 
  dplyr::select(c(variable, p_val_damaged)) %>% 
  rename(p_pp = p_val_damaged)

# 3.- Joining ####

model2_df <- full_join(model2_all, model2_aa, by = "variable")
model2_df <- full_join(model2_df, model2_ps, by = "variable")
model2_df <- full_join(model2_df, model2_pp, by = "variable")

model3_df <- full_join(model3_all, model3_aa, by = "variable")
model3_df <- full_join(model3_df, model3_ps, by = "variable")
model3_df <- full_join(model3_df, model3_pp, by = "variable")

# 4.- Exporting excel ####

write.xlsx(model2_df, "02_clean_data/40_09_models2_excel.xlsx")
write.xlsx(model3_df, "02_clean_data/40_09_models3_excel.xlsx")
