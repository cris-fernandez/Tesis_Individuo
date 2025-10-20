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

model2_all <- read.csv("02_clean_data/46_01_Models_discrete2_AIC.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_all = delta_aic)

model2_aa <- read.csv("02_clean_data/46_02_Models_discrete2_AIC_aa.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_aa = delta_aic)

model2_ps <- read.csv("02_clean_data/46_03_Models_discrete2_AIC_ps.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_ps = delta_aic)

model2_pp <- read.csv("02_clean_data/46_04_Models_discrete2_AIC_pp.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_pp = delta_aic)

# 2.- Reading all model3 data ####

model3_all <- read.csv("02_clean_data/46_05_Models_discrete3_AIC.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_all = delta_aic)

model3_aa <- read.csv("02_clean_data/46_06_Models_discrete3_AIC_aa.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_aa = delta_aic)

model3_ps <- read.csv("02_clean_data/46_07_Models_discrete3_AIC_ps.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_ps = delta_aic)

model3_pp <- read.csv("02_clean_data/46_08_Models_discrete3_AIC_pp.csv") %>% 
  dplyr::select(c(variable, delta_aic)) %>% 
  rename(delta_aic_pp = delta_aic)

# 3.- Joining ####

model2_df <- full_join(model2_all, model2_aa, by = "variable")
model2_df <- full_join(model2_df, model2_ps, by = "variable")
model2_df <- full_join(model2_df, model2_pp, by = "variable")

model3_df <- full_join(model3_all, model3_aa, by = "variable")
model3_df <- full_join(model3_df, model3_ps, by = "variable")
model3_df <- full_join(model3_df, model3_pp, by = "variable")

# 4.- Exporting excel ####

write.xlsx(model2_df, "02_clean_data/46_09_Models2_aic.xlsx")
write.xlsx(model3_df, "02_clean_data/46_09_Models3_aic.xlsx")
