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

# Transforming spot status into a factor so it can be modellised:

clean_target$spot_status <- as.factor(clean_target$spot_status)

# Removing coldspot species and factor category:

clean_target <- clean_target %>% filter(!vigor_id == "cold_healthy") %>% 
  mutate(vigor_id = droplevels(vigor_id))

# Filtering species:

clean_target <- clean_target %>% filter(sp_id == "Abialba")

# 5.- Selecting variables ####

var_list <- c("height", "dbh", "hegyi_index", "wc_22", "percent_c", "percent_n",
              "cn", "sla_22", "age", 
              "chlor_a_fw_22", "chlor_b_fw_22", "total_chl_fw_22", "xc_fw_22", 
              "chla_chlb_22", "chl_xc_22", "leaf_d13c", "leaf_d15n", "leaf_d18o_corrected",
              "mean_1980", "mean_05", "Rt12", "Rt17", "Rt22", "Rs12", "Rs17")

# 6.- Creating empty dataframe

AIC_df <- data.frame(matrix(NA, nrow = length(var_list), ncol = 8))
colnames(AIC_df) <- c("variable", "delta_aic", "estimate_healthy", "estimate_damaged",
                      "ci_upper_damaged", "ci_upper_healthy", "ci_lower_healthy", "ci_lower_damaged")

# 7.- Models and delta AIC ####

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    "~ vigor_id + (1|site)"))
  null_formula <- as.formula(paste(var_list[i], 
                                   "~ 1 + (1|site)"))
  
  model_var <- lmer(model_formula, data = clean_target, REML = F)
  model_null <- lmer(null_formula, data = clean_target, REML = F)
  
  AIC_df$variable[i] <- var_list[i]
  AIC_df$delta_aic[i] <- AIC(model_null, model_var)[1,2] - AIC(model_null, model_var)[2,2]
  AIC_df$estimate_healthy[i] <- summary(model_var)$coefficients[1, 1]
  AIC_df$estimate_damaged[i] <- summary(model_var)$coefficients[2, 1]
  print(i)
}

# 8.- CI 95% ####

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    "~ vigor_id + (1|site)"))
  
  model_var <- lmer(model_formula, data = clean_target)
  
  ci <- summary(emmeans(model_var, ~ vigor_id))
  AIC_df$ci_upper_healthy[i] <- ci$upper.CL[1]
  AIC_df$ci_upper_damaged[i] <- ci$upper.CL[2]
  AIC_df$ci_lower_healthy[i] <- ci$lower.CL[1]
  AIC_df$ci_lower_damaged[i] <- ci$lower.CL[2]
  
  print(i)
}

# 9.- Exporting ####

write.csv(AIC_df, "02_clean_data/46_06_Models_discrete3_AIC_aa.csv")