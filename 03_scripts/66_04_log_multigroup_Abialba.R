rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "vegan", "stats", "devtools", "lavaan", "tidySEM") #list of packages
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

# Filtering per species:

clean_target <- clean_target %>% filter(sp_id == "Abialba")

clean_target <- clean_target %>% 
  mutate(log_defo = log(mean_def_obs + 1),  # If 0s were present
    log_sla = log(sla_22),
    log_bai = log(mean_1980),
    log_height = log(height))

# 6.- SEM structure ####

sem_model <- '
log_bai ~ log_height + log_sla
leaf_d13c ~ log_sla + log_height + log_bai
log_defo ~ c(0, b1)*log_sla + c(0, b2)*log_bai + c(0, b3)*log_height
log_defo ~~ c(0, b4)*leaf_d13c
'

# 7.- Multigroup SEM #
# The arguments provide the standardized coefficients (useful to compare) and 
# the R2 values

## 7.1.- Standardized data ####

norm_target <- clean_target %>% 
  mutate(across(where(is.numeric), scale))

## 7.2.-  SEM with standardized data ####

free_sem <- sem(sem_model,
                norm_target,
                group = "spot_status")

summary(free_sem, standardized = TRUE, fit.measures = TRUE)

## 7.3.- FIML? ####

# Poor model fit, and too many missing values in BAI, which makes the usable 
# observations by lavaan really drop... FIML may help as it apparently uses all available
# data.

fiml_sem <- sem(sem_model,
                norm_target,
                group = "spot_status",
                missing = "fiml")

summary(fiml_sem, standardized = TRUE, fit.measures = TRUE)

## 7.4.- Fixed x? ####

# Poor model fit, and too many missing values in BAI, which makes the usable 
# observations by lavaan really drop... Fixed.x may help as it apparently uses all available
# data.

fixx_sem <- sem(sem_model,
                norm_target,
                group = "spot_status",
                missing = "fiml",
                fixed.x = T)

summary(fixx_sem, standardized = TRUE, fit.measures = TRUE)

# 8.- No growth ####

sem_model <- '
leaf_d13c ~ sla_22 + height
mean_def_obs ~ c(0, b1)*sla_22 + c(0, b2)*height
mean_def_obs ~~ c(0, b3)*leaf_d13c
'
free_sem <- sem(sem_model,
                norm_target,
                group = "spot_status")

summary(free_sem, standardized = TRUE, fit.measures = TRUE)