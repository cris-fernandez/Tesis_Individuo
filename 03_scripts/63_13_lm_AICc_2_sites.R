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

# 1.- Reading target data ####

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv",
                         header = T, sep = ",") %>% dplyr::select(-X) %>%
  mutate(site = substr(plot_id, 1, 3))

# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3))

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

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target$cn <- clean_target$percent_c / clean_target$percent_n

clean_target <- clean_target %>% filter(mean_def_obs < 100)

clean_target$site <- as.factor(clean_target$site)

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


# Transforming spot status into a factor so it can be modellised:

clean_target$spot_status <- as.factor(clean_target$spot_status)

# Filtering by site: 

# nav_aa <- clean_target %>% filter(pair_id == "Nav-Abialba")
# hue_aa <- clean_target %>% filter(pair_id == "Hue-Abialba")
# nav_ps <- clean_target %>% filter(pair_id == "Nav-Pinsylv")
# ter_ps <- clean_target %>% filter(pair_id == "Ter-Pinsylv")
# gua_ps <- clean_target %>% filter(pair_id == "Gua-Pinsylv")
# mad_ps <- clean_target %>% filter(pair_id == "Mad-Pinsylv")
# mad_pp <- clean_target %>% filter(pair_id == "Mad-Pinpine")


site_list <- split(clean_target, clean_target$pair_id)

# 5.- Variable selection ####

var_list <- c("height", "mean_1980", "sla_22", "percent_n",
              "leaf_d13c", "leaf_d18o_corrected",
              "total_chl_fw_22", "xc_fw_22")


# 6.- Lm estimates ####

model_df <- list()
null_df <- list()
model_list <- list()
null_list <- list()

for (s in 1:length(site_list)) {
  model_list[[s]] <- list() #If I attempt to do this inside the loop it will overwrite with every
  # iteration
  model_df[[s]] <- data.frame()
  null_list[[s]] <- list()
  null_df[[s]] <- data.frame()
  
  for (i in 1:length(var_list)) {
   model_formula <- as.formula(paste(var_list[i], "~ spot_status"))
   site_list2 <- site_list[[s]] %>% filter(!is.na(var_list[i]))
   model_list[[s]][[i]] <- lm(model_formula, data = site_list2)
   coefs_model <- broom.mixed::tidy(model_list[[s]][[i]]) %>%
     mutate(variable = var_list[i])
   model_df[[s]] <- bind_rows(model_df[[s]], coefs_model)
   
   null_formula <- as.formula(paste(var_list[i], "~ 1"))
   null_list[[s]][[i]] <- lm(null_formula, data = site_list2)
   coefs_null <- broom.mixed::tidy(null_list[[s]][[i]]) %>%
     mutate(variable = var_list[i])
   null_df[[s]] <- bind_rows(model_df[[s]], coefs_model)
   
   print(paste0(s, ",", i)) 
  }}
  

# 7.- AICc's ####

models_aicc <- list()

for (s in 1:length(site_list)) {
  
  models_aicc[[s]] <- data.frame(matrix(nrow = length(var_list), ncol = 3))
  
  for (i in 1:length(var_list)) {

    models_aicc[[s]][i, 1] <- var_list[i]
    models_aicc[[s]][i, 2] <- AICc(model_list[[s]][[i]])
    models_aicc[[s]][i, 3] <- AICc(null_list[[s]][[i]])
    colnames(models_aicc[[s]]) <- c("variable", "aicc_full", "aicc_null")
    
    print(paste0(s, ",", i)) 
    
  }
  
  models_aicc[[s]] <- models_aicc[[s]] %>% 
    mutate(delta = aicc_full - aicc_null,
           significant = ifelse(delta < -2, "yes", "no"))
  
  }

 
# 8.- Summary with C.I. 95% ####

ci_list <- list()

for (s in 1:length(site_list)) {
  
  ci_list[[s]] <- data.frame()
  
  for (i in 1:length(var_list)) {
    ci <- summary(emmeans(model_list[[s]][[i]], ~ spot_status)) %>% mutate(variable = var_list[i])
    ci_list[[s]] <- rbind(ci_list[[s]], ci)
    
    print(paste0(s, ",", i))
}
}


# 9.- Joining ####

# We are only interested on whether they are significant or not and delta AICc, 
# so we will just select those:

significant_list <- list()

for (s in 1:length(site_list)) {
  significant_list[[s]] <- models_aicc[[s]] %>% dplyr::select(c(variable, significant, delta))
  print(s)
}
 
# Joining:

for (s in 1:length(site_list)) {
  ci_list[[s]] <- left_join(ci_list[[s]], significant_list[[s]], by = "variable")
  print(s)
} 

# Adding a column with site:

for (s in 1:length(site_list)) {
  ci_list[[s]] <- ci_list[[s]] %>% mutate(pair_id = names(site_list[s]))
  print(s)
}

# Collapsing all the elements of the list into a single df:

final_df <- bind_rows(ci_list)

# 10.- Export ####

write.csv(final_df, "02_clean_data/63_13_AICc_discrete2_site.csv")