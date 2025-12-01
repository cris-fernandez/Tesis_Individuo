rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "lme4", "lmerTest", "emmeans", "mgcv", "broom.mixed", "xlsx") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading data ####

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv", 
                         header = T, sep = ",") %>% dplyr::select(-X) %>% 
  mutate(site = substr(plot_id, 1, 3))

# 2.- Clean target data tidying ####

clean_target <- clean_target %>% 
  dplyr::select(-contains("_23")) %>% 
  filter(mean_def_obs < 100)

# Adding T290 defoliation info:
clean_target <- clean_target %>% 
  mutate(mean_def_obs = ifelse(tree_number == "T290", 15, mean_def_obs))

# Additional IDs ####
clean_target$pair_id <- ifelse(grepl("NAV|PEL", clean_target$plot_id) == T, "Mad-Pinpine",
                               ifelse(grepl("GUA", clean_target$plot_id) == T, "Mad-Pinsylv",
                                      ifelse(grepl("ADO|TRA|ALU", clean_target$plot_id) == T, "Gua-Pinsylv",
                                             ifelse(grepl("COR|CED", clean_target$plot_id) == T, "Ter-Pinsylv",
                                                    ifelse(grepl("RON|URZ", clean_target$plot_id) == T, "Nav-Pinsylv",
                                                           ifelse(grepl("BAS|SAR", clean_target$plot_id) == T, "Nav-Abialba",
                                                                  ifelse(grepl("FAG|OZA", clean_target$plot_id) == T, "Hue-Abialba",
                                                                         "z")))))))

clean_target$vigor_id <- ifelse(clean_target$spot_status == "coldspot",
                                "cold_healthy",
                                ifelse(clean_target$mean_def_obs < 25,
                                       "hot_healthy", "hot_damaged")) %>% 
  as.factor()

# Data corrections #####
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 > 3000, NA,
                                       clean_target$total_chl_fw_22)
clean_target$xc_fw_22 <- ifelse(clean_target$xc_fw_22 > 2000, NA,
                                ifelse(clean_target$total_chl_fw_22 < 0, 
                                       NA, clean_target$xc_fw_22))
clean_target$chl_xc_22 <- ifelse(clean_target$chl_xc_22 < 0, NA,
                                 clean_target$chl_xc_22)
clean_target$chla_chlb_22 <- ifelse(clean_target$chla_chlb_22 < 0, NA,
                                    clean_target$chla_chlb_22)

clean_target$sp_id <- ifelse(clean_target$tree_number == "missing_1" | 
                               clean_target$tree_number == "missing_2",
                             "Pinsylv", clean_target$sp_id)

clean_target <- clean_target %>% 
  mutate(sp_id = factor(sp_id),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

# Filtering only hotspot data and by species ####

clean_target <- clean_target %>% filter(spot_status == "hotspot")

aa_target <- clean_target %>% filter(sp_id == "Abialba")
ps_target <- clean_target %>% filter(sp_id == "Pinsylv")
pp_target <- clean_target %>% filter(sp_id == "Pinpine")

# 3.- Correlations ####
## 3.1.- Height ####

cor.test(aa_target$height, aa_target$mean_def_obs)
cor.test(ps_target$height, ps_target$mean_def_obs)
cor.test(pp_target$height, pp_target$mean_def_obs)

## 3.2.- SLA ####

cor.test(aa_target$sla_22, aa_target$mean_def_obs)
cor.test(ps_target$sla_22, ps_target$mean_def_obs)
cor.test(pp_target$sla_22, pp_target$mean_def_obs)

## 3.3.- N ####

cor.test(aa_target$percent_n, aa_target$mean_def_obs)
cor.test(ps_target$percent_n, ps_target$mean_def_obs)
cor.test(pp_target$percent_n, pp_target$mean_def_obs)

## 3.4.- Chl ####

cor.test(aa_target$total_chl_fw_22, aa_target$mean_def_obs)
cor.test(ps_target$total_chl_fw_22, ps_target$mean_def_obs) # Significant
cor.test(pp_target$total_chl_fw_22, pp_target$mean_def_obs) # Significant

## 3.5.- Carotenoids ####

cor.test(aa_target$xc_fw_22, aa_target$mean_def_obs)
cor.test(ps_target$xc_fw_22, ps_target$mean_def_obs) # Significant
cor.test(pp_target$xc_fw_22, pp_target$mean_def_obs) # Significant

## 3.6.- d13C ####

cor.test(aa_target$leaf_d13c, aa_target$mean_def_obs)
cor.test(ps_target$leaf_d13c, ps_target$mean_def_obs)
cor.test(pp_target$leaf_d13c, pp_target$mean_def_obs) # Almost

## 3.7.- d18O ####

cor.test(aa_target$leaf_d18o_corrected, aa_target$mean_def_obs)
cor.test(ps_target$leaf_d18o_corrected, ps_target$mean_def_obs)
cor.test(pp_target$leaf_d18o_corrected, pp_target$mean_def_obs)

## 3.8.- BAI80 ####

cor.test(aa_target$mean_1980, aa_target$mean_def_obs)
cor.test(ps_target$mean_1980, ps_target$mean_def_obs) # Significant
cor.test(pp_target$mean_1980, pp_target$mean_def_obs) 
