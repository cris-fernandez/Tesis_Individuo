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

# 1.- Reading model outputs ####

model_df <- read.csv("02_clean_data/40_01_models_2way.csv") %>% 
  dplyr::select(-X)

model_df_long <- model_df %>% 
  pivot_longer(cols = -variable,
               names_to = c(".value", "status"),  # .value: parte compartida del nombre
               names_pattern = "(.*)_(cold|hot)")

# 2.- Plotting height ####

model_df2 <- model_df_long %>% filter(variable == "height")
height_plot <- ggplot(model_df2) +
  geom_point(aes(x = status, y = estimate), size = 4) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper)) +
  theme_classic()
