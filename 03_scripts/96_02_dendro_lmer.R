rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "vegan", "stats", "devtools", "lavaan", "tidySEM", "lmerTest") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading data ####

# climate_data <- read.csv("02_clean_data/02_00_climate_series.csv") %>% dplyr::select(-X)
dendro_data <- read.csv("02_clean_data/02_02_dendro_series.csv") %>% dplyr::select(-X)
dendro_data$site <- substr(dendro_data$plot_id, start = 1, stop = 3)


dendro_data80 <- dendro_data %>% filter(year > 1979)

# 2.- Models ####
## 2.1.- Abies alba - Huesca ####

abies_hue <- dendro_data80 %>% 
  filter(site == "FAG" | site ==  "OZA")

# mean_abies_hue <- abies_hue %>% 
#   group_by(year, spot_status) %>% 
#   summarise(mean_bai = mean(bai, na.rm = T),
#             se_bai = sd(bai, na.rm = T) / sqrt(sum(!is.na(bai))))

abies_hue_model <- lmerTest::lmer(bai ~ spot_status + (1 | plot_id / tree_number),
                              data = abies_hue, na.action = na.omit)
abies_hue_model2 <- lmerTest::lmer(bai ~ spot_status + (1 | tree_number),
                                  data = abies_hue, na.action = na.omit)

AIC(abies_hue_model3, abies_hue_model2) # Small differences, delta < 3

summary(abies_hue_model) # p = 0.060852

## 2.2.- Abies alba - Navarra ####

abies_nav <- dendro_data80 %>% 
  filter(site == "BAS" | site ==  "SAR")

abies_nav_model <- lmerTest::lmer(bai ~ spot_status + (1 | plot_id / tree_number),
                                  data = abies_nav, na.action = na.omit)

summary(abies_nav_model) # p = 2.8e-06

## 2.3.- Pinus sylvestris - Navarra ####

psylv_nav <- dendro_data80 %>% 
  filter(site == "RON" | site ==  "URZ")

psylv_nav_model <- lmerTest::lmer(bai ~ spot_status + (1 | plot_id / tree_number),
                                  data = psylv_nav, na.action = na.omit)

summary(psylv_nav_model) # p = 4.9e-12

## 2.4.- Pinus sylvestris - Teruel ####

psylv_ter <- dendro_data80 %>% 
  filter(site == "COR" | site ==  "CED")

psylv_ter_model <- lmerTest::lmer(bai ~ spot_status + (1 | plot_id / tree_number),
                                  data = psylv_ter, na.action = na.omit)

summary(psylv_ter_model) # p = 0.0214

## 2.5.- Pinus sylvestris - Guadalajara ####

psylv_gua <- dendro_data80 %>% 
  filter(site == "ADO" | site ==  "ALU" | site == "TRA")

psylv_gua_model <- lmerTest::lmer(bai ~ spot_status + (1 | plot_id / tree_number),
                                  data = psylv_gua, na.action = na.omit)

summary(psylv_gua_model) # p = 0.00988

## 2.6.- Pinus sylvestris - Madrid ####

psylv_mad <- dendro_data80 %>% 
  filter(site == "GUA")

psylv_mad_model <- lmerTest::lmer(bai ~ spot_status + (1 | plot_id / tree_number),
                                  data = psylv_mad, na.action = na.omit)

summary(psylv_mad_model) # p = 0.00343

## 2.7.- Pinus pinea - Madrid ####

ppine_mad <- dendro_data80 %>% 
  filter(site == "NAV" | site == "PEL")

ppine_mad_model <- lmerTest::lmer(bai ~ spot_status + (1 | plot_id / tree_number),
                                  data = ppine_mad, na.action = na.omit)

summary(ppine_mad_model) # p = 0.5

