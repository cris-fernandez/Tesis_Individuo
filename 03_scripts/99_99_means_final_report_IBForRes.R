rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

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

# Outlayers deletion:

clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 4783, NA, clean_target$mean_1980)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 60 & clean_target$sp_id == "Abialba",
                                    NA, clean_target$mean_def_obs)
clean_target$sla_22 <- ifelse(clean_target$sla_22 > 99 & clean_target$sp_id == "Pinsylv",
                              NA, clean_target$sla_22)
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 < 75 & clean_target$sp_id == "Pinsylv",
                                       NA, clean_target$total_chl_fw_22)
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 < 40 & clean_target$sp_id == "Pinpine",
                                       NA, clean_target$total_chl_fw_22)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 58 & clean_target$sp_id == "Pinpine",
                                    NA, clean_target$mean_def_obs)
clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 3000 & clean_target$sp_id == "Abialba" & clean_target$spot_status == "hotspot",
                                 NA, clean_target$mean_1980)

clean_target <- clean_target %>% 
  mutate(sp_id = factor(sp_id))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine"),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"),
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot"))

# 5.- Plots and trees number ####

## 5.1.- Plot number ####

numbers_plot <- clean_target %>% 
  dplyr::select(c(plot_id, site)) %>% 
  unique() %>% 
  count(site)

## 5.2.- Target number ####

numbers_target <- clean_target %>% 
  dplyr::select(c(plot_id, tree_id, site)) %>% 
  unique() %>% 
  count(site)

# 6.- Traits means ####

## 6.1.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(pair_spot = paste0(pair_id, "-", spot_status),
         sp_spot = paste0(sp_id, "-", spot_status)) %>% 
  dplyr::select(c(sp_spot, height, mean_1980, sla_22, percent_n, 
                  leaf_d13c, leaf_d18o_corrected, total_chl_fw_22, xc_fw_22))

## 6.2.- Summarising ####

means_target <- clean_target %>% 
  group_by(sp_spot) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

sd_target <- clean_target %>% 
  group_by(sp_spot) %>% 
  summarise(across(where(is.numeric), ~ sd(.x, na.rm = TRUE)))

cv_target <- sd_target

for (i in 1:6) {
  for (j in 1:8) {
    cv_target[i, j+1] <- sd_target[i, j+1]/means_target[i, j+1]
    print(paste0(i, ":", j))
  }
}

min_target <- clean_target %>% 
  group_by(sp_spot) %>% 
  summarise(across(where(is.numeric), ~ quantile(.x, 0.025, na.rm = T)))

max_target <- clean_target %>% 
  group_by(sp_spot) %>% 
  summarise(across(where(is.numeric), ~ quantile(.x, 0.975, na.rm = T)))

# 7.- Elevation ####
# Reading clean plot and adding extra IDs:

clean_plot <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/02_clean_data/02_01_clean_plot.csv", 
                         header = T, sep = ",") %>% dplyr::select(-X) %>% 
  mutate(site = substr(plot_id, 1, 3))

clean_plot$pair_id <- ifelse(grepl("NAV|PEL", clean_plot$plot_id) == T, "Mad-Pinpine",
                               ifelse(grepl("GUA", clean_plot$plot_id) == T, "Mad-Pinsylv",
                                      ifelse(grepl("ADO|TRA|ALU", clean_plot$plot_id) == T, "Gua-Pinsylv",
                                             ifelse(grepl("COR|CED", clean_plot$plot_id) == T, "Ter-Pinsylv",
                                                    ifelse(grepl("RON|URZ", clean_plot$plot_id) == T, "Nav-Pinsylv",
                                                           ifelse(grepl("BAS|SAR", clean_plot$plot_id) == T, "Nav-Abialba",
                                                                  ifelse(grepl("FAG|OZA", clean_plot$plot_id) == T, "Hue-Abialba",
                                                                         "z")))))))
clean_plot$pair_spot <- paste0(clean_plot$pair_id, "-", clean_plot$spot_status)

# Calculating mean

means_elev <- clean_plot %>% 
  group_by(pair_spot) %>% 
  summarise(elev = mean(geo_GPScm_UTM_elev, na.rm = TRUE))

sd_elev <- clean_plot %>% 
  group_by(pair_spot) %>% 
  summarise(elev = sd(geo_GPScm_UTM_elev, na.rm = TRUE))

cv_elev <- sd_elev

for (i in 1:14) {
    cv_elev[i, 2] <- sd_elev[i, 2]/means_elev[i, 2]
    print(i)
  }

min_elev <- clean_plot %>% 
  group_by(pair_spot) %>% 
  summarise(elev = quantile(geo_GPScm_UTM_elev, 0.025, na.rm = T))

max_elev <- clean_plot %>% 
  group_by(pair_spot) %>% 
  summarise(elev = quantile(geo_GPScm_UTM_elev, 0.975, na.rm = T))
