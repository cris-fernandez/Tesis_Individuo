rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "vegan", "stats", "devtools", "lavaan") #list of packages
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

aa_target <- clean_target %>% filter(sp_id == "Abialba") %>% mutate(across(where(is.numeric), scale)) %>% na.omit()
ps_target <- clean_target %>% filter(sp_id == "Pinsylv") %>% mutate(across(where(is.numeric), scale))%>% na.omit()
pp_target <- clean_target %>% filter(sp_id == "Pinpine") %>% mutate(across(where(is.numeric), scale))%>% na.omit()

# 7.- SEM structure ####

sem_model <- '
mean_1980 ~ height + sla_22
mean_def_obs ~ mean_1980 + sla_22
leaf_d13c ~ mean_def_obs
'

# 8.- Free model ####
# In lavaan

aa_free_sem <- sem(sem_model, aa_target, group = "spot_status")
ps_free_sem <- sem(sem_model, ps_target, group = "spot_status")
pp_free_sem <- sem(sem_model, pp_target, group = "spot_status")

summary(aa_free_sem, fit.measures = T)
summary(ps_free_sem, fit.measures = T)
summary(pp_free_sem, fit.measures = T) # Not bad 

# 9.- Constrained model ####
# Intercepts and regressions are set the same in both groups

aa_cons_sem <- sem(sem_model, aa_target, group = "spot_status",
                   group.equal = c("intercepts", "regressions"))
ps_cons_sem <- sem(sem_model, ps_target, group = "spot_status",
                   group.equal = c("intercepts", "regressions"))
pp_cons_sem <- sem(sem_model, pp_target, group = "spot_status",
                   group.equal = c("intercepts", "regressions"))


# 10.- Comparing with Anova ####

anova(aa_free_sem, aa_cons_sem) # Significantly different
anova(ps_free_sem, ps_cons_sem) # Significantly different
anova(pp_free_sem, pp_cons_sem) # Significantly different

# In all species, The model is significantly different from the unconstrained 
# model, so some paths could be constrained? Additionally, model performance 
# is quite bad so it needs to be improved

# 11.- Testing for constraints ####
## 11.1.- Abies alba ####
### 11.1.1.- d13C ~ Defo ####
sem_model_aa <- '
mean_1980 ~ height + sla_22
mean_def_obs ~ mean_1980 + sla_22
leaf_d13c ~ c("b1", "b1") * mean_def_obs
'
aa_cons_sem2 <- sem(sem_model_aa, aa_target, group = "spot_status")
anova(aa_free_sem, aa_cons_sem) # Significantly different
