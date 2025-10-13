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

# 6.- Model list ####

model_list <- list()
var_list <- c("height", "dbh", "hegyi_index", "wc_22", "percent_c", "percent_n",
              "cn", "sla_22", "age", 
              "chlor_a_fw_22", "chlor_b_fw_22", "total_chl_fw_22", "xc_fw_22", 
              "chla_chlb_22", "chl_xc_22", "leaf_d13c", "leaf_d15n", "leaf_d18o",
              "mean_1980", "mean_05", "Rt12", "Rt17", "Rt22", "Rs12", "Rs17")

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    "~ spot_status + (1|plot_id)"))
  
  model_list[[i]] <- lmer(model_formula, data = clean_target)
  print(i)
}

# 7.- Model coefficients table ####

model_df <- data.frame(matrix(ncol = 11, nrow = length(var_list)))
colnames(model_df) <- c("variable", "estimate_cold", "estimate_hot", 
                        "std_error_cold", "std_error_hot", "df_cold", "df_hot",
                        "t_val_cold", "t_val_hot", "p_val_cold", "p_val_hot")
for (i in 1:length(var_list)) {

model_df$variable[[i]] <- var_list[[i]]
model_df$estimate_cold[i] <- 
  summary(model_list[[i]])$coefficients["(Intercept)", "Estimate"]
model_df$estimate_hot[i] <-
  summary(model_list[[i]])$coefficients["spot_statushotspot", "Estimate"]
model_df$std_error_cold[i] <-
  summary(model_list[[i]])$coefficients["(Intercept)", "Std. Error"]
model_df$std_error_hot[i] <-
  summary(model_list[[i]])$coefficients["spot_statushotspot", "Std. Error"]
model_df$df_cold[i] <- 
  summary(model_list[[i]])$coefficients["(Intercept)", "df"]
model_df$df_hot[i] <-
  summary(model_list[[i]])$coefficients["spot_statushotspot", "df"]
model_df$t_val_cold[i] <- 
  summary(model_list[[i]])$coefficients["(Intercept)", "t value"]
model_df$t_val_hot[i] <- 
  summary(model_list[[i]])$coefficients["spot_statushotspot", "t value"]
model_df$p_val_cold[i] <- 
  summary(model_list[[i]])$coefficients["(Intercept)", "Pr(>|t|)"]
model_df$p_val_hot[i] <- 
  summary(model_list[[i]])$coefficients["spot_statushotspot", "Pr(>|t|)"]
print(i)
}

# 8.- Calculating CI 95% ####

ci_list <- list()

for (i in 1:length(var_list)) {
  ci_list[[i]] <- summary(emmeans(model_list[[i]], ~ spot_status))
  print(i)
}

# 9.- Adding confidence intervals to the table ####

for (i in 1:length(var_list)) {
  model_df$ci_lower_cold[i] <- ci_list[[i]][1, "lower.CL"]
  model_df$ci_upper_cold[i] <- ci_list[[i]][1, "upper.CL"]
  model_df$ci_lower_hot[i] <- ci_list[[i]][2, "lower.CL"]
  model_df$ci_upper_hot[i] <- ci_list[[i]][2, "upper.CL"]
  print(i)
}

# 10.- Estimate ####

# In model outputs, the estimate for the second category is expressed as the difference 
# regarding the previous category

model_df <- model_df %>% 
  mutate(estimate_hot = estimate_cold + estimate_hot)

# 11.- Saving df ####

write.csv(model_df, "02_clean_data/40_01_models_2way.csv")

# 5.- Morphological variables ####

# model_h <- lmer(height ~ spot_status + (1|plot_id),
#                 data = clean_target)
# 
# model_sla <- lmer(percent_n ~ spot_status + (1|plot_id),
#                 data = clean_target)
# 
#  summary(model_list[[1]])$coefficients
# 
# # AIC(mixed_lmer_12, mixed_lmer_12_p)
# summary(model_all)
# car::Anova(model_all, 3) # III because I have an interaction
# anova(model_all)
# plot_model(model_all, type = "pred", terms = c("SPEI_ST", "tree_category"))
# # anova(model_sla)
