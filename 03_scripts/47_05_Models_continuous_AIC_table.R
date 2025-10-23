rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "lme4", "lmerTest", "emmeans", "xlsx") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading all model2 data ####

model2_aa <- read.csv("02_clean_data/47_02_Models_continuous_AIC_aa.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_aa = delta_aic)

model2_ps <- read.csv("02_clean_data/47_03_Models_continuous_AIC_ps.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_ps = delta_aic)

model2_pp <- read.csv("02_clean_data/47_04_Models_continuous_AIC_pp.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_pp = delta_aic)

# 3.- Joining ####

model2_df <- full_join(model2_aa, model2_ps, by = "variable")
model2_df <- full_join(model2_df, model2_pp, by = "variable")

#4.- Exporting csv ####

write.csv(model2_df, "02_clean_data/47_05_Models_continuous_aic.csv")

# 4.- Exporting excel ####
# 
# write.xlsx(model2_df, "02_clean_data/47_05_Models_continuous_aic.xlsx")