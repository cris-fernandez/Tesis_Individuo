rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading target data ####

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv", 
                         header = T, sep = ",") %>% select(-X) %>% 
  mutate(site = substr(plot_id, 1, 3))

# 2.- Removing 2023 data ####
# So I can have in the same column 2022 and 2023 values

clean_target <- clean_target %>% 
  select(-contains("_23"))

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
                                ifelse(clean_target$prec < 30,
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

clean_target$sp_id <- fct_relevel(clean_target$sp_id, "Abialba", "Pinsylv", "Pinpine")

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(mean_def_obs, height, dbh, chl_fw_22, xc_fw_22, chla_chlb_22,
                  chl_xc_22, percent_c, percent_n, cn_ratio, leaf_d13c, 
                  leaf_d18o, leaf_d15n, wood_d13c_17, wood_d13c_22, sla_22,
                  age, hegyi_index, mean_bai, mean_1980, mean_20, mean_15,
                  mean_10, mean_05, Rt12, Rt17, Rt22, Rs12, Rs17)) %>% 
  select(sort(names(.)))

# 6.- Data normalization ####

# However, since I do not know whether scale() does exactly the same as 
# the manual standardization, I will test both procedures

norm_target <- clean_target %>%
  mutate(defoliation_ST = (mean_def_obs - mean(mean_def_obs, na.rm = T)) / sd(mean_def_obs, na.rm = T),
         height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         dbh_ST = (dbh - mean(dbh, na.rm = T)) / sd(dbh, na.rm = T),
         chl_ST = (chl_fw_22 - mean(chl_fw_22, na.rm = T)) / sd(chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         chl_ab_ST = (chla_chlb_22 - mean(chla_chlb_22, na.rm = T)) / sd(chla_chlb_22, na.rm = T),
         chl_xc_ST = (chl_xc_22 - mean(chl_xc_22, na.rm = T)) / sd(chl_xc_22, na.rm = T),
         percent_c_ST = (percent_c - mean(percent_c, na.rm = T)) / sd(percent_c, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         cn_ratio_ST = (cn_ratio - mean(cn_ratio, na.rm = T)) / sd(cn_ratio, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         leaf_d15n_ST = (leaf_d15n - mean(leaf_d15n, na.rm = T)) / sd(leaf_d15n, na.rm = T),
         leaf_d18o_ST = (leaf_d18o - mean(leaf_d18o, na.rm = T)) / sd(leaf_d18o, na.rm = T),
         wood_d13c_17_ST = (wood_d13c_17 - mean(wood_d13c_17, na.rm = T)) / sd(wood_d13c_17, na.rm = T),
         wood_d13c_22_ST = (wood_d13c_22 - mean(wood_d13c_22, na.rm = T)) / sd(wood_d13c_22, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         age_ST = (age - mean(age, na.rm = T)) / sd(age, na.rm = T),
         hegyi_index_ST = (hegyi_index - mean(hegyi_index, na.rm = T)) / sd(hegyi_index, na.rm = T),
         bai_ST = (mean_bai - mean(mean_bai, na.rm = T)) / sd(mean_bai, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T),
         bai_20_ST = (mean_20 - mean(mean_20, na.rm = T)) / sd(mean_20, na.rm = T),
         bai_15_ST = (mean_15 - mean(mean_15, na.rm = T)) / sd(mean_15, na.rm = T),
         bai_10_ST = (mean_10 - mean(mean_10, na.rm = T)) / sd(mean_10, na.rm = T),
         bai_05_ST = (mean_05 - mean(mean_05, na.rm = T)) / sd(mean_05, na.rm = T),
         Rt12_ST = (Rt12 - mean(Rt12, na.rm = T)) / sd(Rt12, na.rm = T),
         Rs12_ST = (Rs12 - mean(Rs12, na.rm = T)) / sd(Rs12, na.rm = T),
         Rt17_ST = (Rt17 - mean(Rt17, na.rm = T)) / sd(Rt17, na.rm = T),
         Rs17_ST = (Rs17 - mean(Rs17, na.rm = T)) / sd(Rs17, na.rm = T),
         Rt22_ST = (Rt22 - mean(Rt22, na.rm = T)) / sd(Rt22, na.rm = T))

norm_target <- norm_target %>% select(contains("_ST"))

# 7.- Correlations matrix ####

# Omission of NAs
norm_target <- na.omit(norm_target)

# The chart is needed for the PCA:

correlogram <- cor(norm_target)
ggcorrplot(correlogram)

# 8.- PCA analysis ####

pca_results <- princomp(correlogram)
summary(pca_results)

# The first two components explain only 78.5% of the data variance!

pca_results$loadings[, 1:2]

# 9.- Scree plot ####

scree <- fviz_eig(pca_results, addlabels = T, 
                  barfill = "black", barcolor = "black")

tiff("04_figures/04_03_screeplot.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
  scree
dev.off()

# 10.- Biplot ####

tiff("04_figures/04_03_biplot.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
fviz_pca_var(pca_results, col.var = "black")
dev.off()

# 11.- Variable contribution ####

contrib <- fviz_cos2(pca_results, choice = "var", axes = 1:2,
                     fill = "black", color = "black")

tiff("04_figures/04_03_contribution_plot.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
contrib
dev.off()

# 12.- Correlations matrix - no wood ####
# "_nw" means "no wood"

# Omission of NAs
norm_target_nw <- clean_target %>% 
  dplyr::select(-c(wood_d13c_17, wood_d13c_22)) %>% 
  na.omit()

# The chart is needed for the PCA:

correlogram_nw <- cor(norm_target_nw)
ggcorrplot(correlogram_nw)

# 13.- PCA analysis ####

pca_results_nw <- princomp(correlogram_nw)
summary(pca_results_nw)

# The first two components explain only 78.5% of the data variance!

pca_results_nw$loadings[, 1:2]

# 14.- Scree plot ####

scree_nw <- fviz_eig(pca_results_nw, addlabels = T, 
                  barfill = "black", barcolor = "black")

tiff("04_figures/04_03_screeplot_nw.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
scree_nw
dev.off()

# 15.- Biplot ####

tiff("04_figures/04_03_biplot_nw.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
fviz_pca_var(pca_results_nw, col.var = "black")
dev.off()

# 15.- Variable contribution ####

contrib_nw <- fviz_cos2(pca_results_nw, choice = "var", axes = 1:2,
                     fill = "black", color = "black")

tiff("04_figures/04_03_contribution_plot_nw.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
contrib_nw
dev.off()




