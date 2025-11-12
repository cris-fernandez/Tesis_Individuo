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

clean_target <- clean_target %>% filter(mean_def_obs < 60)

clean_target$site <- as.factor(clean_target$site)

# Transforming spot status into a factor so it can be modellised:

clean_target$spot_status <- as.factor(clean_target$spot_status)

# Filtering by species: 

aa_target <- clean_target %>% 
  filter(sp_id == "Abialba")
ps_target <- clean_target %>% 
  filter(sp_id == "Pinsylv")
pp_target <- clean_target %>% 
  filter(sp_id == "Pinpine") %>% 
  filter(mean_def_obs < 60)

# 5.- Variable selection ####

var_list <- c("height", "dbh", "hegyi_index", "wc_22", "percent_c", "percent_n",
              "cn", "sla_22", "age", 
              "chlor_a_fw_22", "chlor_b_fw_22", "total_chl_fw_22", "xc_fw_22", 
              "chla_chlb_22", "chl_xc_22", "leaf_d13c", "leaf_d15n", 
              "leaf_d18o_corrected",
              "mean_1980", "mean_05", "Rt12", "Rt17", "Rt22", "Rs12", "Rs17")

# 6.- Lm estimates ####
## 6.1.- Abies alba ####

model_df_aa <- data.frame()
null_df_aa <- data.frame()
model_list_aa <- list()
null_list_aa <- list()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], "~ spot_status"))
  aa_target2 <- aa_target %>% filter(!is.na(var_list[i]))
  model_list_aa[[i]] <- lm(model_formula, data = aa_target2)
  coefs_model <- broom.mixed::tidy(model_list_aa[[i]]) %>% 
    mutate(variable = var_list[i]) 
  model_df_aa <- bind_rows(model_df_aa, coefs_model)
  
  null_formula <- as.formula(paste(var_list[i], "~ 1"))
  null_list_aa[[i]] <- lm(null_formula, data = aa_target2)
  coefs_null <- broom.mixed::tidy(null_list_aa[[i]]) %>%
    mutate(variable = var_list[i])
  null_df_aa <- bind_rows(null_df_aa, coefs_null)
  
  print(i)
}


## 6.2.- Pinus sylvestris ####

model_df_ps <- data.frame()
null_df_ps <- data.frame()
model_list_ps <- list()
null_list_ps <- list()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], "~ spot_status"))
  ps_target2 <- ps_target %>% filter(!is.na(var_list[i]))
  model_list_ps[[i]] <- lm(model_formula, data = ps_target2)
  coefs_model <- broom.mixed::tidy(model_list_ps[[i]]) %>% 
    mutate(variable = var_list[i]) 
  model_df_ps <- bind_rows(model_df_ps, coefs_model)
  
  null_formula <- as.formula(paste(var_list[i], "~ 1"))
  null_list_ps[[i]] <- lm(null_formula, data = ps_target2)
  coefs_null <- broom.mixed::tidy(null_list_ps[[i]]) %>%
    mutate(variable = var_list[i])
  null_df_ps <- bind_rows(null_df_ps, coefs_null)
  
  print(i)
}

## 6.3.- Pinus pinea ####

model_df_pp <- data.frame()
null_df_pp <- data.frame()
model_list_pp <- list()
null_list_pp <- list()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], "~ spot_status"))
  pp_target2 <- pp_target %>% filter(!is.na(var_list[i]))
  model_list_pp[[i]] <- lm(model_formula, data = pp_target2)
  coefs_model <- broom.mixed::tidy(model_list_pp[[i]]) %>% 
    mutate(variable = var_list[i]) 
  model_df_pp <- bind_rows(model_df_pp, coefs_model)
  
  null_formula <- as.formula(paste(var_list[i], "~ 1"))
  null_list_pp[[i]] <- lm(null_formula, data = pp_target2)
  coefs_null <- broom.mixed::tidy(null_list_pp[[i]]) %>%
    mutate(variable = var_list[i])
  null_df_pp <- bind_rows(null_df_pp, coefs_null)
  
  print(i)
}

# 7.- AICc's ####

aa_aicc <- data.frame(matrix(nrow = length(var_list), ncol = 3))
ps_aicc <- data.frame(matrix(nrow = length(var_list), ncol = 3))
pp_aicc <- data.frame(matrix(nrow = length(var_list), ncol = 3))

for (i in 1:length(var_list)) {
  aa_aicc[i, 1] <- var_list[i]
  aa_aicc[i, 2] <- AICc(model_list_aa[[i]])
  aa_aicc[i, 3] <- AICc(null_list_aa[[i]])
  
  ps_aicc[i, 1] <- var_list[i]
  ps_aicc[i, 2] <- AICc(model_list_ps[[i]])
  ps_aicc[i, 3] <- AICc(null_list_ps[[i]])
  
  pp_aicc[i, 1] <- var_list[i]
  pp_aicc[i, 2] <- AICc(model_list_pp[[i]])
  pp_aicc[i, 3] <- AICc(null_list_pp[[i]])
}

colnames(aa_aicc) <- c("variable", "aa_aicc_full", "aa_aicc_null")
colnames(ps_aicc) <- c("variable", "ps_aicc_full", "ps_aicc_null")
colnames(pp_aicc) <- c("variable", "pp_aicc_full", "pp_aicc_null")

aa_aicc$delta <- aa_aicc$aa_aicc_full - aa_aicc$aa_aicc_null
ps_aicc$delta <- ps_aicc$ps_aicc_full - ps_aicc$ps_aicc_null
pp_aicc$delta <- pp_aicc$pp_aicc_full - pp_aicc$pp_aicc_null

aa_aicc$significant <- ifelse(aa_aicc$delta < -3, "yes", "no")
ps_aicc$significant <- ifelse(ps_aicc$delta < -3, "yes", "no")
pp_aicc$significant <- ifelse(pp_aicc$delta < -3, "yes", "no")

# 8.- Summary with C.I. 95% ####

aa_ci_df <- data.frame()
ps_ci_df <- data.frame()
pp_ci_df <- data.frame()

for (i in 1:length(var_list)) {
  aa_ci <- summary(emmeans(model_list_aa[[i]], ~ spot_status)) %>% mutate(variable = var_list[i])
  ps_ci <- summary(emmeans(model_list_ps[[i]], ~ spot_status)) %>% mutate(variable = var_list[i])
  pp_ci <- summary(emmeans(model_list_pp[[i]], ~ spot_status)) %>% mutate(variable = var_list[i])
  
  aa_ci_df <- rbind(aa_ci_df, aa_ci)
  ps_ci_df <- rbind(ps_ci_df, ps_ci)
  pp_ci_df <- rbind(pp_ci_df, pp_ci)
  
  print(i)
}

# 9.- Joining ####

# We are only interested on whether they are significant or not, so we will just 
# select those:

aa_significant <- aa_aicc %>% dplyr::select(c(variable, significant))
ps_significant <- ps_aicc %>% dplyr::select(c(variable, significant))
pp_significant <- pp_aicc %>% dplyr::select(c(variable, significant))

aa_ci_df <- left_join(aa_ci_df, aa_significant, by = "variable")
ps_ci_df <- left_join(ps_ci_df, ps_significant, by = "variable")
pp_ci_df <- left_join(pp_ci_df, pp_significant, by = "variable")

aa_ci_df$sp_id <- "Abialba"
ps_ci_df$sp_id <- "Pinsylv"
pp_ci_df$sp_id <- "Pinpine"

# Final joint

ci_df <- rbind(aa_ci_df, ps_ci_df, pp_ci_df)

# 10.- Export ####

write.csv(ci_df, "02_clean_data/63_01_AICc_discrete2.csv")