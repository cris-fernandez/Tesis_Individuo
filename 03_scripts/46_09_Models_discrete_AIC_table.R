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

# model2_all <- read.csv("02_clean_data/46_01_Models_discrete2_AIC.csv") %>% 
#   dplyr::select(c(variable, delta_aic)) %>% 
#   rename(delta_aic_all = delta_aic)

model2_aa <- read.csv("02_clean_data/46_02_Models_discrete2_AIC_aa.csv") %>% 
  dplyr::select(-X) %>% 
  mutate(sp_id = "aa")

model2_ps <- read.csv("02_clean_data/46_03_Models_discrete2_AIC_ps.csv") %>% 
  dplyr::select(-X) %>% 
  mutate(sp_id = "ps")

model2_pp <- read.csv("02_clean_data/46_04_Models_discrete2_AIC_pp.csv") %>% 
  dplyr::select(-X) %>% 
  mutate(sp_id = "pp")

# 2.- Reading all model3 data ####

# model3_all <- read.csv("02_clean_data/46_05_Models_discrete3_AIC.csv") %>% 
#   dplyr::select(c(variable, delta_aic)) %>% 
#   rename(delta_aic_all = delta_aic)

model3_aa <- read.csv("02_clean_data/46_06_Models_discrete3_AIC_aa.csv") %>% 
  dplyr::select(-X) %>% 
  mutate(sp_id = "aa")

model3_ps <- read.csv("02_clean_data/46_07_Models_discrete3_AIC_ps.csv") %>% 
  dplyr::select(-X) %>% 
  mutate(sp_id = "ps")

model3_pp <- read.csv("02_clean_data/46_08_Models_discrete3_AIC_pp.csv") %>% 
  dplyr::select(-X) %>% 
  mutate(sp_id = "pp")

# 3.- Joining ####

model2_df <- rbind(model2_aa, model2_ps, model2_pp)
model3_df <- rbind(model3_aa, model3_ps, model3_pp)

# 4.- Exporting csv ####

write.csv(model2_df, "02_clean_data/46_09_Models2_discrete_aic.csv") 
write.csv(model3_df, "02_clean_data/46_09_Models3_discrete_aic.csv") 

# 4.- Exporting excel ####

# write.xlsx(model2_df, "02_clean_data/46_09_Models2_aic.xlsx")
# write.xlsx(model3_df, "02_clean_data/46_09_Models3_aic.xlsx")
