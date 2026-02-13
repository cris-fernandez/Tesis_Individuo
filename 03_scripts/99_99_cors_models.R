rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "Hmisc") #list of packages
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

clean_target <- clean_target %>% 
  mutate(sp_id = factor(sp_id))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine"),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"),
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot"))

# 5.- Correlations ####

aa_hot <- clean_target %>% filter(spot_status == "hotspot" & sp_id == "Abialba")

ps_hot <- clean_target %>% filter(spot_status == "hotspot" & sp_id == "Pinsylv")

pp_hot <- clean_target %>% filter(spot_status == "hotspot" & sp_id == "Pinpine")

# 6.- Models ####

vars_list <- c("height", "mean_1980", "sla_22", "percent_n", "total_chl_fw_22",
               "xc_fw_22", "leaf_d13c", "leaf_d18o_corrected")

aa_list <- list()
ps_list <- list()
pp_list <- list()

# In the loop: using just vars_list[i] does not work because it recognises it as a 
# character and not as a formula, hence the need to transform it (as.formula())
for (i in 1:length(vars_list)) {
  aa_model <- lm(as.formula(paste(vars_list[i], "~ mean_def_obs")), data = aa_hot)  
  ps_model <- lm(as.formula(paste(vars_list[i], "~ mean_def_obs")), data = ps_hot)
  pp_model <- lm(as.formula(paste(vars_list[i], "~ mean_def_obs")), data = pp_hot)
  
  aa_list[[i]] <- summary(aa_model)$coefficients[2,]
  ps_list[[i]] <- summary(ps_model)$coefficients[2,]
  pp_list[[i]] <- summary(pp_model)$coefficients[2,]
  
  print(i)
}

for (i in 1:length(vars_list)) {
  aa_list[[i]] <- as.data.frame(aa_list[[i]]) %>% t()
  ps_list[[i]] <- as.data.frame(ps_list[[i]]) %>% t()
  pp_list[[i]] <- as.data.frame(pp_list[[i]]) %>% t()
  print(i)
}
# From list to df:

aa_df <- do.call(rbind.data.frame, aa_list) 
ps_df <- do.call(rbind.data.frame, ps_list)
pp_df <- do.call(rbind.data.frame, pp_list)

# Adding variables:

aa_df$var <- vars_list
ps_df$var <- vars_list
pp_df$var <- vars_list

summary(pp_model)