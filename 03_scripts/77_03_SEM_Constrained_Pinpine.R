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

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  dplyr::select(c(height, total_chl_fw_22, percent_n, leaf_d13c, leaf_d18o_corrected,
                  sla_22, xc_fw_22,mean_1980, mean_def_obs, tree_number, sp_id, spot_status, vigor_id,
                  pair_id))

summary(clean_target)
levels(clean_target$spot_status) # Coldspot first


# 6.- Filtering and standardising ####
# Standardization by site
pp_target <- clean_target %>% filter(sp_id == "Pinpine") %>% 
  group_by(pair_id) %>% 
  mutate(across(where(is.numeric), scale))

summary(pp_target)

# 7.- SEM structure ####

sem_model <- '
mean_1980 ~ height + sla_22
leaf_d13c ~ height + mean_1980
'
# 8.- Constrained model ####
# Since my variables were generally quasi-normal, it is justified to use 
# "ML", the default estimator, instead of "MLR", the estimator of preferred use in 
# case of violation of normality. Although it is safer, it is also less flexible
# According to Antonio, the first step is to test the model with all paths constrained

locked_sem <- sem(sem_model, 
                  pp_target, 
                  group = "spot_status", 
                  group.equal = c("regressions"), 
                  missing = "fiml")

# Checking the "test statistic (Chi-square) y P-value
# (Chi-square)" of the model

summary(locked_sem, standardized = TRUE, fit.measures = TRUE)
# This model has a p = 0.000 (non-significant), so the model does not adjust well
# to my data

# 9.- Free model ####
# Since the constrained model does not adjust correctly to the data, 
# I now need to fit the free model to see if it does fit well:

free_sem <- sem(sem_model,
                pp_target,
                group = "spot_status",
                missing   = "fiml")

summary(free_sem, standardized = TRUE, fit.measures = TRUE)
# p = 0.622 --> the model adjusts to my data, so there is a model in between I can try


# 10.- Liberating paths ####
## 10.1.- Modindices ####
# It says what paths are more influential in the model's X-squared

modindices(locked_sem, sort = TRUE) 
lavTestScore(locked_sem)

# Although height -> BAI is the most influential path, we are not interested in 
# assessing that relation and can be left constrained. Moreover, this path is 
# only very influential in one of the groups, not in both. So we can jump to the #2
# and #3 path, which is BAI -> d13C

## 10.2.- Height -> d13C ####

liber_sem <- sem(sem_model,
                 pp_target,
                 group = "spot_status",
                 missing = "fiml",
                 group.equal = "regressions",
                 group.partial = c("leaf_d13c ~ mean_1980"))

summary(liber_sem, standardized = TRUE, fit.measures = TRUE)

# p = 0.012, we can further liberate paths
modindices(liber_sem, sort = T)

## 10.3.- Height -> d13C ####

liber_sem2 <- sem(sem_model,
                 pp_target,
                 group = "spot_status",
                 missing = "fiml",
                 group.equal = "regressions",
                 group.partial = c("leaf_d13c ~ mean_1980",
                                   "mean_1980 ~ height"))

summary(liber_sem2, standardized = TRUE, fit.measures = TRUE)
# All metrics have significantly improved :)
