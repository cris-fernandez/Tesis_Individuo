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
  dplyr::select(c(height, total_chl_fw_22, percent_n, leaf_d13c, 
                  sla_22, xc_fw_22,mean_1980, mean_def_obs, tree_number, sp_id, spot_status, vigor_id))

summary(clean_target)

# 6.- Filtering per species ####
# Also normalization:

ps_target <- clean_target %>% filter(sp_id == "Pinsylv") %>% mutate(across(where(is.numeric), scale)) %>% na.omit()
summary(ps_target)

# 7.- SEM structure ####

sem_model <- '
mean_1980 ~ height + sla_22
leaf_d13c ~ sla_22 + height
mean_def_obs ~ sla_22 + mean_1980 + height
mean_def_obs ~~ leaf_d13c
'
# 8.- Free model ####
# In lavaan

ps_free_sem <- sem(sem_model, ps_target, group = "spot_status")

summary(ps_free_sem, standardized = TRUE, fit.measures = TRUE)

# So it seems we need to drop (very low beta, non-significant p-val and low std all)
# SLA 22 from all paths:

sem_model2 <- '
mean_1980 ~ height
leaf_d13c ~ sla_22 + height
mean_def_obs ~ height
'
# 9.- New model ####
# In lavaan

ps_free_sem2 <- sem(sem_model2, ps_target, group = "spot_status")
summary(ps_free_sem2, standardized = TRUE, fit.measures = TRUE)


# 10.- Constrained model ####
# Intercepts and regressions are set the same in both groups

ps_cons_sem <- sem(sem_model2, ps_target, group = "spot_status",
                   group.equal = c("intercepts", "regressions"))

# 11.- Comparing with Anova ####

anova(ps_free_sem2, ps_cons_sem) # Significantly different

# The model is significantly different from the unconstrained 
# model, so some paths could be constrained? Additionally, model performance 
# is quite bad so it needs to be improved

# 12.- Constraining ####
## 12.1.- BAI80 ~ h ####
sem_model_cons <- '
mean_1980 ~ c("b1", "b1") * height
leaf_d13c ~ sla_22 + height
mean_def_obs ~ mean_1980 + height
'
ps_cons_sem2 <- sem(sem_model_cons, ps_target, group = "spot_status")
anova(ps_free_sem2, ps_cons_sem2) # Significant --> do not constrain

## 12.2.- d13C ~ SLA ####
sem_model_cons <- '
mean_1980 ~ height
leaf_d13c ~ c("b2", "b2") * sla_22 + height
mean_def_obs ~ mean_1980 + height
'
ps_cons_sem2 <- sem(sem_model_cons, ps_target, group = "spot_status")
anova(ps_free_sem2, ps_cons_sem2) # Significant --> do not constrain

## 12.3.- d13C ~ height ####
sem_model_cons <- '
mean_1980 ~ height
leaf_d13c ~ sla_22 + c("b3", "b3") * height
mean_def_obs ~ mean_1980 + height
'
ps_cons_sem2 <- sem(sem_model_cons, ps_target, group = "spot_status")
anova(ps_free_sem2, ps_cons_sem2) # Non significant --> constrain

## 12.4.- Defo ~ BAI ####
sem_model_cons <- '
mean_1980 ~ height
leaf_d13c ~ sla_22 + height
mean_def_obs ~ c("b4", "b4") * mean_1980 + height
'
ps_cons_sem2 <- sem(sem_model_cons, ps_target, group = "spot_status")
anova(ps_free_sem2, ps_cons_sem2) # Non significant --> constrain

## 12.5.- Defo ~ height ####
sem_model_cons <- '
mean_1980 ~ height
leaf_d13c ~ sla_22 + height
mean_def_obs ~ mean_1980 + c("b5", "b5") * height
'
ps_cons_sem2 <- sem(sem_model_cons, ps_target, group = "spot_status")
anova(ps_free_sem2, ps_cons_sem2) # Non significant --> constrain

## 11.6.- Potential final model ####

sem_model_final <- '
# mean_1980 ~ height
# leaf_d13c ~ sla_22 + c("b3", "b3")*height
# mean_def_obs ~ c("b4", "b4")*mean_1980 + c("b5", "b5")*height
 mean_1980 ~ height
 leaf_d13c ~ sla_22 + height
 mean_def_obs ~ mean_1980 + height
'
ps_final_sem <- sem(sem_model_final, ps_target, group = "spot_status",
                    se = "bootstrap",
                    bootstrap = 5000)
summary(ps_final_sem, standardized = TRUE, fit.measures = TRUE)



# Plot multi-group SEM
graph_sem(model = ps_final_sem)
