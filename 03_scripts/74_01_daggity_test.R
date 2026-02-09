rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "vegan", "stats", "devtools", "lavaan", "tidySEM", "dagitty") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

# 1.- Reading target data ####

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv",
                         header = T, sep = ",") %>% dplyr::select(-X) %>%
  mutate(site = substr(plot_id, 1, 3))

# 2.- Removing 2023 data ####
# So I can have in the same column 2022 and 2023 values

clean_target <- clean_target %>% 
  dplyr::select(-contains("_23"))

# Adding T290 defoliation info:

clean_target <- clean_target %>% 
  mutate(mean_def_obs = ifelse(tree_number == "T290", 15, mean_def_obs))

# 3.- Additional IDs ####

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

# 4.- Data corrections #####

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
  mutate(sp_id = factor(sp_id))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine"),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"),
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot")) %>% 
  filter(mean_def_obs < 100)


# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(height, total_chl_fw_22, percent_n, leaf_d13c, leaf_d18o_corrected,
                  sla_22, xc_fw_22,mean_1980, mean_def_obs, tree_number, sp_id, spot_status, vigor_id))

summary(clean_target)
levels(clean_target$spot_status) # Coldspot first

# 6.- Defoliation adjustment ####

# Defoliation must be 0 in non-declining sites:

clean_target <- clean_target %>% 
  mutate(mean_def_obs = ifelse(spot_status == "coldspot", 0, mean_def_obs))

# 7.- SEM structure ####

# Since I am trying Daggity, we will not use the multigroup yet

sem_model <- '
mean_1980 ~ height + sla_22
leaf_d13c ~ sla_22 + height + mean_1980
mean_def_obs ~ sla_22 + mean_1980 + height
mean_def_obs ~~ leaf_d13c
'

# 8.- Dagitty structure ####

# SEM must be translated to dagitty so it can be understood, instead of 
# this symbol "~", it uses arrows

dag_model <- dagitty("dag {
  height -> mean_1980
  sla_22 -> mean_1980

  height -> leaf_d13c
  sla_22 -> leaf_d13c
  mean_1980 -> leaf_d13c

  height -> mean_def_obs
  sla_22 -> mean_def_obs
  mean_1980 -> mean_def_obs

  leaf_d13c <-> mean_def_obs
}
")

# 9.- Plotting ####

plot(graphLayout(dag_model))

# 10.- Testable implications ####
# These are the assumption the SEM makes that require testing...  I guess??

print(impliedConditionalIndependencies(dag_model)) 

# 11.- Testing for paths ####
# One by one... 

## 11.1.- h --> BAI ####
exposures(dag_model) <- "height"
outcomes(dag_model)  <- "mean_1980"
adjustmentSets(dag_model)
# Output:  {} --> No adjustment needed :)

## 11.2.- SLA --> BAI ####
exposures(dag_model) <- "sla_22"
outcomes(dag_model)  <- "mean_1980"
adjustmentSets(dag_model)
# Output:  {} --> No adjustment needed :)

## 11.3.- h --> d13C ####
exposures(dag_model) <- "height"
outcomes(dag_model)  <- "leaf_d13c"
adjustmentSets(dag_model)
# Output:  {} --> No adjustment needed :)

## 11.4.- BAI --> d13C ####
exposures(dag_model) <- "mean_1980"
outcomes(dag_model)  <- "leaf_d13c"
adjustmentSets(dag_model)
# Output:  { height, sla_22 } --> It means I do need to check the upstream 
# causes of BAI, i.e., height and SLA: I do that :)

## 11.5.- SLA --> d13C ####
exposures(dag_model) <- "sla_22"
outcomes(dag_model)  <- "leaf_d13c"
adjustmentSets(dag_model)
# Output:  {} --> No adjustment needed :)

## 11.6.- SLA --> defoliation ####
exposures(dag_model) <- "sla_22"
outcomes(dag_model)  <- "mean_def_obs"
adjustmentSets(dag_model)
# Output:  {} --> No adjustment needed :)

## 11.7.- BAI --> defoliation ####
exposures(dag_model) <- "mean_1980"
outcomes(dag_model)  <- "mean_def_obs"
adjustmentSets(dag_model)
# Output:  { height, sla_22 } --> It means I do need to check the upstream 
# causes of BAI, i.e., height and SLA: I do that :)

## 11.8.- height --> defoliation ####
exposures(dag_model) <- "height"
outcomes(dag_model)  <- "mean_def_obs"
adjustmentSets(dag_model)
# Output:  {} --> No adjustment needed :)