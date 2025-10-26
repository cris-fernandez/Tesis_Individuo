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
  mutate(sp_id = factor(sp_id),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target$cn <- clean_target$percent_c / clean_target$percent_n

clean_target <- clean_target %>% filter(mean_def_obs < 100)

clean_target$site <- as.factor(clean_target$site)

# Transforming spot status into a factor so it can be modellised:

clean_target$spot_status <- as.factor(clean_target$spot_status)

# Filtering by species: 

clean_target <- clean_target %>% 
  filter(sp_id == "Abialba") %>% 
  mutate(d13c_centered = 1 + (leaf_d13c - min(leaf_d13c, na.rm = TRUE)) / 
                  (max(leaf_d13c, na.rm = TRUE) - min(leaf_d13c, na.rm = TRUE)),
         d15n_centered = 1 + (leaf_d15n - min(leaf_d15n, na.rm = TRUE)) / 
           (max(leaf_d15n, na.rm = TRUE) - min(leaf_d15n, na.rm = TRUE)),
         d18o_centered = 1 + (leaf_d18o_corrected - min(leaf_d18o_corrected, na.rm = TRUE)) / 
           (max(leaf_d18o_corrected, na.rm = TRUE) - min(leaf_d18o_corrected, na.rm = TRUE)))

# 6.- Lmer list ####

var_list <- c("height", "dbh", "hegyi_index", "wc_22", "percent_c", "percent_n",
              "cn", "sla_22", "age", 
              "chlor_a_fw_22", "chlor_b_fw_22", "total_chl_fw_22", "xc_fw_22", 
              "chla_chlb_22", "chl_xc_22", "d13c_centered", "d15n_centered", 
              "d18o_centered",
              "mean_1980", "mean_05", "Rt12", "Rt17", "Rt22", "Rs12", "Rs17")

lmer_df <- data.frame()
model_list_lmer <- list()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    "~ mean_def_obs + (1|site)"))
  
  clean_target2 <- clean_target %>% filter(!is.na(var_list[i]))
  model_list_lmer[[i]] <- lmer(model_formula, data = clean_target2, REML = F)
  r2 <- r.squaredGLMM(model_list_lmer[[i]])
  coefs <- broom.mixed::tidy(model_list_lmer[[i]], effects = "fixed")%>% 
    mutate(variable = var_list[1],
           r2m = r2[1],
           r2c = r2[2],
           n_obs = nobs(model_list_lmer[[i]]))
  
  lmer_df <- bind_rows(lmer_df, coefs)
  print(i)
}

# 7.- GAMMA list ####

gamma_df <- data.frame()
model_list_gamma <- list()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    "~ mean_def_obs + (1|site)"))
  
  clean_target2 <- clean_target %>% filter(!is.na(var_list[i]))
  model_list_gamma[[i]] <- glmmTMB::glmmTMB(model_formula, data = clean_target2,
                                      family = Gamma(link = "log"), REML = F)
  r2 <- r.squaredGLMM(model_list_gamma[[i]])
  coefs <- broom.mixed::tidy(model_list_gamma[[i]], effects = "fixed") %>% 
    mutate(variable = var_list[1],
           r2m = r2[1],
           r2c = r2[2],
           n_obs = nobs(model_list_gamma[[i]]))
  
  gamma_df <- bind_rows(gamma_df, coefs)
  print(i)
}

# 8.- GAMM list ####

gamm_df <- data.frame()
model_list_gamm <- list()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    '~ s(mean_def_obs) + s(site, bs = "re")'))
  
  clean_target2 <- clean_target %>% filter(!is.na(var_list[i]))
  model_list_gamm[[i]] <- mgcv::gam(model_formula, data = clean_target, 
                               method= "ML")
  coefs <- broom.mixed::tidy(model_list_gamm[[i]], effects = "fixed") %>% 
    mutate(variable = var_list[1],
           r2 = summary(model_list_gamm[[i]])$r.sq) # Why just one R2?
  gamm_df <- bind_rows(gamm_df, coefs)
  print(i)
}

# 9.- AICs ####

aic_df <- data.frame()

for (i in 1:length(var_list)) {
  AICs <- AIC(model_list_lmer[[i]],
              model_list_gamma[[i]],
              model_list_gamm[[i]]) %>% 
    dplyr::select(AIC)
  AICs$model <- c("lmer", "gamma", "gamm")
  AICs$variable <- var_list[i]
  
  aic_df <- bind_rows(aic_df, AICs)
  print(i)
}

aic_df <- pivot_wider(aic_df, names_from = "model", values_from = "AIC")

write.xlsx(aic_df, "02_clean_data/60_01_aa_disc2_aics.xlsx")
