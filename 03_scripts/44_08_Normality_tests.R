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

clean_target <- clean_target %>% 
  mutate(sp_id = factor(sp_id))

clean_target$cn <- clean_target$percent_c / clean_target$percent_n

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine"),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"),
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot"))

# 5.- Variable selection ####

clean_target <- clean_target %>% mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(height, dbh, percent_c, percent_n, cn_ratio, sla_22, age, hegyi_index, 
                  wc_22, total_chl_fw_22, chla_chlb_22, xc_fw_22, chl_xc_22, 
                  leaf_d13c, leaf_d15n, leaf_d18o_corrected, mean_1980, mean_05, 
                  Rt12, Rt17, Rt22, Rs12, Rs17, mean_def_obs, sp_id, vigor_id))

# 6.- N per category ####

clean_target %>% count(sp_id)
clean_target %>% count(vigor_id)

# 7.- Log-transformed variables ####

aa_target <- clean_target %>% filter(sp_id == "Abialba") %>% 
  mutate(across(where(is.numeric), ~ log1p(.x - min(.x, na.rm = TRUE)))) %>% 
  dplyr::select(-c(vigor_id, sp_id))

ps_target <- clean_target %>% filter(sp_id == "Pinsylv") %>% 
  mutate(across(where(is.numeric), ~ log1p(.x - min(.x, na.rm = TRUE)))) %>% 
  dplyr::select(-c(vigor_id, sp_id))

pp_target <- clean_target %>% filter(sp_id == "Pinpine") %>% 
  mutate(across(where(is.numeric), ~ log1p(.x - min(.x, na.rm = TRUE)))) %>% 
  dplyr::select(-c(vigor_id, sp_id))

# 8.- Log-transformed Shappiro tests ####

log_normality_aa <- data.frame(matrix(NA, nrow = 24, ncol = 2))
log_normality_ps <- data.frame(matrix(NA, nrow = 24, ncol = 2))
log_normality_pp <- data.frame(matrix(NA, nrow = 24, ncol = 2))

for (i in 1:ncol(aa_target)) {
  log_normality_aa[i, 1] <- shapiro.test(aa_target[, i])$statistic
  log_normality_aa[i, 2] <- shapiro.test(aa_target[, i])$p.value
  
  log_normality_ps[i, 1] <- shapiro.test(ps_target[, i])$statistic
  log_normality_ps[i, 2] <- shapiro.test(ps_target[, i])$p.value
  
  log_normality_pp[i, 1] <- shapiro.test(ps_target[, i])$statistic
  log_normality_pp[i, 2] <- shapiro.test(ps_target[, i])$p.value
  print(i)
}

colnames(log_normality_aa) <- c("W-statistic_aa", "p-val_aa")
colnames(log_normality_ps) <- c("W-statistic_ps", "p-val_ps")
colnames(log_normality_pp) <- c("W-statistic_pp", "p-val_pp")

log_normality_aa$variable <- colnames(aa_target)
log_normality_ps$variable <- colnames(ps_target)
log_normality_pp$variable <- colnames(pp_target)

log_shapiro <- full_join(log_normality_aa, log_normality_ps, by = "variable")
log_shapiro <- full_join(log_shapiro, log_normality_pp, by = "variable")

# 9.- Raw Shappiro tests ####

aa_target <- clean_target %>% filter(sp_id == "Abialba") %>% 
  dplyr::select(-c(vigor_id, sp_id))

ps_target <- clean_target %>% filter(sp_id == "Pinsylv") %>% 
  dplyr::select(-c(vigor_id, sp_id))

pp_target <- clean_target %>% filter(sp_id == "Pinpine") %>% 
  dplyr::select(-c(vigor_id, sp_id))

raw_normality_aa <- data.frame(matrix(NA, nrow = 24, ncol = 2))
raw_normality_ps <- data.frame(matrix(NA, nrow = 24, ncol = 2))
raw_normality_pp <- data.frame(matrix(NA, nrow = 24, ncol = 2))

for (i in 1:ncol(aa_target)) {
  raw_normality_aa[i, 1] <- shapiro.test(aa_target[, i])$statistic
  raw_normality_aa[i, 2] <- shapiro.test(aa_target[, i])$p.value
  
  raw_normality_ps[i, 1] <- shapiro.test(ps_target[, i])$statistic
  raw_normality_ps[i, 2] <- shapiro.test(ps_target[, i])$p.value
  
  raw_normality_pp[i, 1] <- shapiro.test(ps_target[, i])$statistic
  raw_normality_pp[i, 2] <- shapiro.test(ps_target[, i])$p.value
  print(i)
}

colnames(raw_normality_aa) <- c("W-statistic_aa", "p-val_aa")
colnames(raw_normality_ps) <- c("W-statistic_ps", "p-val_ps")
colnames(raw_normality_pp) <- c("W-statistic_pp", "p-val_pp")

raw_normality_aa$variable <- colnames(aa_target)
raw_normality_ps$variable <- colnames(ps_target)
raw_normality_pp$variable <- colnames(pp_target)

raw_shapiro <- full_join(raw_normality_aa, raw_normality_ps, by = "variable")
raw_shapiro <- full_join(raw_shapiro, raw_normality_pp, by = "variable")

# 10.- Visualization ####

raw_shapiro
log_shapiro
