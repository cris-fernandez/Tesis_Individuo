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
                  sla_22, xc_fw_22,mean_1980, mean_def_obs, tree_number, sp_id, spot_status, vigor_id)) %>% 
  dplyr::select(sort(names(.)))

clean_target <- clean_target[complete.cases(clean_target), ]

summary(clean_target)

# 6.- Filtering per species ####

aa_target_all <- clean_target %>% filter(sp_id == "Abialba")
ps_target_all <- clean_target %>% filter(sp_id == "Pinsylv")
pp_target_all <- clean_target %>% filter(sp_id == "Pinpine")

aa_target_cold <- aa_target_all %>% filter(spot_status == "coldspot")
ps_target_cold <- ps_target_all %>% filter(spot_status == "coldspot")
pp_target_cold <- pp_target_all %>% filter(spot_status == "coldspot")

aa_target_hot <- aa_target_all %>% filter(spot_status == "hotspot")
ps_target_hot <- ps_target_all %>% filter(spot_status == "hotspot")
pp_target_hot <- pp_target_all %>% filter(spot_status == "hotspot")

# 7.- Normalization ####
# 7.1.- All ####

norm_aa_target_all <- aa_target_all %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_aa_all <- norm_aa_target_all$vigor_id
norm_aa_target_all <- norm_aa_target_all %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

norm_ps_target_all <- ps_target_all %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_ps_all <- norm_ps_target_all$vigor_id
norm_ps_target_all <- norm_ps_target_all %>% dplyr::select(c(contains("_ST"))) %>% 
  dplyr::select(-spot_status)

norm_pp_target_all <- pp_target_all %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_pp_all <- norm_pp_target_all$vigor_id
norm_pp_target_all <- norm_pp_target_all %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

## 7.2.- Coldspot ####

norm_aa_target_cold <- aa_target_cold %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_aa_cold <- norm_aa_target_cold$vigor_id
norm_aa_target_cold <- norm_aa_target_cold %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

norm_ps_target_cold <- ps_target_cold %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_ps_cold <- norm_ps_target_cold$vigor_id
norm_ps_target_cold <- norm_ps_target_cold %>% dplyr::select(c(contains("_ST"))) %>% 
  dplyr::select(-spot_status)

norm_pp_target_cold <- pp_target_cold %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_pp_cold <- norm_pp_target_cold$vigor_id
norm_pp_target_cold <- norm_pp_target_cold %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

## 7.3.- Hotspot ####

norm_aa_target_hot <- aa_target_hot %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_aa_hot <- norm_aa_target_hot$vigor_id
norm_aa_target_hot <- norm_aa_target_hot %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

norm_ps_target_hot <- ps_target_hot %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_ps_hot <- norm_ps_target_hot$vigor_id
norm_ps_target_hot <- norm_ps_target_hot %>% dplyr::select(c(contains("_ST"))) %>% 
  dplyr::select(-spot_status)

norm_pp_target_hot <- pp_target_hot %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_pp_hot <- norm_pp_target_hot$vigor_id
norm_pp_target_hot <- norm_pp_target_hot %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

# 8.- Correlations matrix ####
# The chart is needed for the PCA:

## 8.1.- All ####
correlogram_aa_all <- cor(norm_aa_target_all)
correlogram_ps_all <- cor(norm_ps_target_all)
correlogram_pp_all <- cor(norm_pp_target_all)

## 8.2.- Coldspot ####
correlogram_aa_cold <- cor(norm_aa_target_cold)
correlogram_ps_cold <- cor(norm_ps_target_cold)
correlogram_pp_cold <- cor(norm_pp_target_cold)

## 8.3.- Hotspot ####
correlogram_aa_hot <- cor(norm_aa_target_hot)
correlogram_ps_hot <- cor(norm_ps_target_hot)
correlogram_pp_hot <- cor(norm_pp_target_hot)

# 9.- PCA analysis ####
## 9.1.- All ####
pca_results_aa_all <- princomp(norm_aa_target_all)
summary(pca_results_aa_all)

pca_results_ps_all <- princomp(norm_ps_target_all)
summary(pca_results_ps_all)

pca_results_pp_all <- princomp(norm_pp_target_all)
summary(pca_results_pp_all)

## 9.2.- Coldspot ####
pca_results_aa_cold <- princomp(norm_aa_target_cold)
summary(pca_results_aa_cold)

pca_results_ps_cold <- princomp(norm_ps_target_cold)
summary(pca_results_ps_cold)

pca_results_pp_cold <- princomp(norm_pp_target_cold)
summary(pca_results_pp_cold)

## 9.3.- Hotspot ####
pca_results_aa_hot <- princomp(norm_aa_target_hot)
summary(pca_results_aa_hot)

pca_results_ps_hot <- princomp(norm_ps_target_hot)
summary(pca_results_ps_hot)

pca_results_pp_hot <- princomp(norm_pp_target_hot)
summary(pca_results_pp_hot)

# Selecting the first two components

pca_results_aa_all$loadings[, 1:2]
pca_results_ps_all$loadings[, 1:2]
pca_results_pp_all$loadings[, 1:2]

pca_results_aa_cold$loadings[, 1:2]
pca_results_ps_cold$loadings[, 1:2]
pca_results_pp_cold$loadings[, 1:2]

pca_results_aa_hot$loadings[, 1:2]
pca_results_ps_hot$loadings[, 1:2]
pca_results_pp_hot$loadings[, 1:2]

# Dataframes

pca_df_aa_all <- cbind(norm_aa_target_all, pca_results_aa_all$scores) # Scores for the points
pca_df_aa_all$vigor_id <- norm_aa_target_all$vigor_id
pca_df_ps_all <- cbind(norm_ps_target_all, pca_results_ps_all$scores)
pca_df_ps_all$vigor_id <- norm_ps_target_all$vigor_id
pca_df_pp_all <- cbind(norm_pp_target_all, pca_results_pp_all$scores)
pca_df_pp_all$vigor_id <- norm_pp_target_all$vigor_id

pca_df_aa_cold <- cbind(norm_aa_target_cold, pca_results_aa_cold$scores) # Scores for the points
pca_df_aa_cold$vigor_id <- norm_aa_target_cold$vigor_id
pca_df_ps_cold <- cbind(norm_ps_target_cold, pca_results_ps_cold$scores)
pca_df_ps_cold$vigor_id <- norm_ps_target_cold$vigor_id
pca_df_pp_cold <- cbind(norm_pp_target_cold, pca_results_pp_cold$scores)
pca_df_pp_cold$vigor_id <- norm_pp_target_cold$vigor_id

pca_df_aa_hot <- cbind(norm_aa_target_hot, pca_results_aa_hot$scores) # Scores for the points
pca_df_aa_hot$vigor_id <- norm_aa_target_hot$vigor_id
pca_df_ps_hot <- cbind(norm_ps_target_hot, pca_results_ps_hot$scores)
pca_df_ps_hot$vigor_id <- norm_ps_target_hot$vigor_id
pca_df_pp_hot <- cbind(norm_pp_target_hot, pca_results_pp_hot$scores)
pca_df_pp_hot$vigor_id <- norm_pp_target_hot$vigor_id

# 10.- Biplots ####

## 10.1.- Loadings dataframe ####

loadings_df_aa_all <- as.data.frame(pca_results_aa_all$loadings[, 1:2])
loadings_df_ps_all <- as.data.frame(pca_results_ps_all$loadings[, 1:2])
loadings_df_pp_all <- as.data.frame(pca_results_pp_all$loadings[, 1:2])

loadings_df_aa_cold <- as.data.frame(pca_results_aa_cold$loadings[, 1:2])
loadings_df_ps_cold <- as.data.frame(pca_results_ps_cold$loadings[, 1:2])
loadings_df_pp_cold <- as.data.frame(pca_results_pp_cold$loadings[, 1:2])

loadings_df_aa_hot <- as.data.frame(pca_results_aa_hot$loadings[, 1:2])
loadings_df_ps_hot <- as.data.frame(pca_results_ps_hot$loadings[, 1:2])
loadings_df_pp_hot <- as.data.frame(pca_results_pp_hot$loadings[, 1:2])

# They are not merged into pca_df as they have different row numbers

loadings_df_aa_all$variable <- rownames(loadings_df_aa_all) # So we know what variable is which
loadings_df_ps_all$variable <- rownames(loadings_df_ps_all)
loadings_df_pp_all$variable <- rownames(loadings_df_pp_all)

loadings_df_aa_cold$variable <- rownames(loadings_df_aa_cold) # So we know what variable is which
loadings_df_ps_cold$variable <- rownames(loadings_df_ps_cold)
loadings_df_pp_cold$variable <- rownames(loadings_df_pp_cold)

loadings_df_aa_hot$variable <- rownames(loadings_df_aa_hot) # So we know what variable is which
loadings_df_ps_hot$variable <- rownames(loadings_df_ps_hot)
loadings_df_pp_hot$variable <- rownames(loadings_df_pp_hot)

# Adding a column with the proper names of the variables to appear on the PCA:

varnames_all <- c("Height", "Chl.", "Car.", "N", "δ13C", "SLA", "BAI80")

loadings_df_aa_all$varnames <- varnames_all
loadings_df_ps_all$varnames <- varnames_all
loadings_df_pp_all$varnames <- varnames_all

loadings_df_aa_cold$varnames <- varnames_all
loadings_df_ps_cold$varnames <- varnames_all
loadings_df_pp_cold$varnames <- varnames_all

loadings_df_aa_hot$varnames <- varnames_all
loadings_df_ps_hot$varnames <- varnames_all
loadings_df_pp_hot$varnames <- varnames_all

## 10.2.- Scale factor ####

# Scale factor is just a constant number used to multiply the length of the vectors 
# thus allowing us to see them more clearly

scale_factor <- 13 

## 10.3.- Multiplying ####

loadings_df_aa_all <- loadings_df_aa_all %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)
loadings_df_ps_all <- loadings_df_ps_all %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)
loadings_df_pp_all <- loadings_df_pp_all %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)

loadings_df_aa_cold <- loadings_df_aa_cold %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)
loadings_df_ps_cold <- loadings_df_ps_cold %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)
loadings_df_pp_cold <- loadings_df_pp_cold %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)

loadings_df_aa_hot <- loadings_df_aa_hot %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)
loadings_df_ps_hot <- loadings_df_ps_hot %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)
loadings_df_pp_hot <- loadings_df_pp_hot %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)


# 11.- Adding defoliation ####
## 11.1.- All ####
pca_df_aa_all$mean_def_obs <- aa_target_all$mean_def_obs
pca_df_ps_all$mean_def_obs <- ps_target_all$mean_def_obs
pca_df_pp_all$mean_def_obs <- pp_target_all$mean_def_obs

## 11.2.- Coldspot ####
pca_df_aa_cold$mean_def_obs <- aa_target_cold$mean_def_obs
pca_df_ps_cold$mean_def_obs <- ps_target_cold$mean_def_obs
pca_df_pp_cold$mean_def_obs <- pp_target_cold$mean_def_obs

## 11.3.- Hotspot ####
pca_df_aa_hot$mean_def_obs <- aa_target_hot$mean_def_obs
pca_df_ps_hot$mean_def_obs <- ps_target_hot$mean_def_obs
pca_df_pp_hot$mean_def_obs <- pp_target_hot$mean_def_obs

# 12.- Plotting ####

# First I need to know the maximum level of defoliation to add a common scale:
max(clean_target$mean_def_obs, na.rm = TRUE) # So 80%

## 12.1.- Abies alba ####
### 12.1.1.- Aa all ####

biplot_aa_all <- ggplot() +
  geom_point(data = pca_df_aa_all, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_aa_all,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (35.45 %)") + 
  ylab("PC2 (19.06 %)") + 
  labs(tag = "Aa all") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_aa_all,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

### 12.1.2.- Aa cold ####

biplot_aa_cold <- ggplot() +
  geom_point(data = pca_df_aa_cold, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_aa_cold,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (35.92 %)") + 
  ylab("PC2 (23.70 %)") + 
  labs(tag = "Aa cold") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_aa_cold,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

### 12.1.3.- Aa hot ####

biplot_aa_hot <- ggplot() +
  geom_point(data = pca_df_aa_hot, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_aa_hot,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (31.02 %)") + 
  ylab("PC2 (22.49 %)") + 
  labs(tag = "Aa hot") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_aa_hot,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

## 12.2.- Pinus sylvestris ####
### 12.2.1.- Ps All ####

biplot_ps_all <- ggplot() +
  geom_point(data = pca_df_ps_all, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_ps_all,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (48.58 %)") + 
  ylab("PC2 (19.97 %)") + 
  labs(tag = "Ps all") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_ps_all,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

### 12.2.2.- Ps cold ####

biplot_ps_cold <- ggplot() +
  geom_point(data = pca_df_ps_cold, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_ps_cold,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (49.14 %)") + 
  ylab("PC2 (20.79 %)") + 
  labs(tag = "Ps cold") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_ps_cold,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

### 12.2.1.- Ps hot ####

biplot_ps_hot <- ggplot() +
  geom_point(data = pca_df_ps_hot, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_ps_hot,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (43.59 %)") + 
  ylab("PC2 (22.22 %)") + 
  labs(tag = "Ps hot") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_ps_hot,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

## 12.3.- Pinus pinea ####
### 12.3.1.- Pp all ####

biplot_pp_all <- ggplot() +
  geom_point(data = pca_df_pp_all, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_pp_all,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (32.54 %)") + 
  ylab("PC2 (21.40 %)") + 
  labs(tag = "Pp all") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_pp_all,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

### 12.3.2.- Pp cold ####
biplot_pp_cold <- ggplot() +
  geom_point(data = pca_df_pp_cold, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_pp_cold,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (37.48 %)") + 
  ylab("PC2 (24.35 %)") + 
  labs(tag = "Pp cold") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_pp_cold,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

### 12.3.3.- Pp hot ####
biplot_pp_hot <- ggplot() +
  geom_point(data = pca_df_pp_hot, aes(x = Comp.1, y = Comp.2, color = mean_def_obs), 
             size = 4, alpha = 0.95) +
  scale_color_viridis_c(option = "mako", direction = -1, name = "Defoliation (%)",
                        limits = c(0, 80)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_pp_hot,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (32.30 %)") + 
  ylab("PC2 (24.02 %)") + 
  labs(tag = "Pp hot") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_pp_hot,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")
# 13.- Saving ####

tiff("04_figures/51_03_PCA_separate_assemble.tiff", units = "mm",
     width = 900, height = 1200,
     res = 400, compression = "lzw")
biplot_aa_all + biplot_aa_cold + biplot_aa_hot + 
  biplot_ps_all + biplot_ps_cold + biplot_ps_hot +
  biplot_pp_all + biplot_pp_cold + biplot_pp_hot + 
  plot_spacer() + guide_area() + 
  plot_layout(ncol = 3, guides = "collect",
              heights = c(1, 1, 1, 0.3))
dev.off()