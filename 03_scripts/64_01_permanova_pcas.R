rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "vegan", "stats", "devtools") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
?pairwise.adonis

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

aa_target <- clean_target %>% filter(sp_id == "Abialba")
ps_target <- clean_target %>% filter(sp_id == "Pinsylv")
pp_target <- clean_target %>% filter(sp_id == "Pinpine")

# 7.- Normalization ####

norm_aa_target <- aa_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_aa <- norm_aa_target$vigor_id
norm_aa_target <- norm_aa_target %>% dplyr::select(c(contains("_ST"), vigor_id)) %>% 
  dplyr::select(-spot_status)

norm_ps_target <- ps_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_ps <- norm_ps_target$vigor_id
norm_ps_target <- norm_ps_target %>% dplyr::select(c(contains("_ST"), vigor_id)) %>% 
  dplyr::select(-spot_status)


norm_pp_target <- pp_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_pp <- norm_pp_target$vigor_id
norm_pp_target <- norm_pp_target %>% dplyr::select(c(contains("_ST"), vigor_id)) %>% 
  dplyr::select(-spot_status)

# 8.- Distance matrix ####

# First, I need to extract the vigor_id column from normalised dataframes:

aa_vigor <- norm_aa_target$vigor_id
ps_vigor <- norm_ps_target$vigor_id
pp_vigor <- norm_pp_target$vigor_id

norm_aa_target2 <- norm_aa_target %>% dplyr::select(-vigor_id)
norm_ps_target2 <- norm_ps_target %>% dplyr::select(-vigor_id)
norm_pp_target2 <- norm_pp_target %>% dplyr::select(-vigor_id)

# Distance matrix

dist_aa <- dist(norm_aa_target2, method = "euclidean")
dist_ps <- dist(norm_ps_target2, method = "euclidean")
dist_pp <- dist(norm_pp_target2, method = "euclidean")

# 9.- Permanova analyses ####

permanova_aa <- adonis2(dist_aa ~ aa_vigor, permutations = 999)
permanova_ps <- adonis2(dist_ps ~ ps_vigor, permutations = 999)
permanova_pp <- adonis2(dist_pp ~ pp_vigor, permutations = 999)

# All permanova tests show that, for all species, there are differences in the 
# disposition of the points, and thus in the trait coordination across vigour groups.

# 10.- Pairwise permanova ####

# pairwise_aa <- pairwise_adonis(x = dist_aa, 
#                           groups = aa_vigor, 
#                           p.adjust.method = "bonferroni", 
#                           perm = 999)

# pairwiseAdonis(norm_aa_target, factors = aa_vigor, sim.method = "euclidean")
wawa <- pairwise.adonis(x=norm_aa_target[,1:7],factors=norm_aa_target$vigor_id,sim.function='dist',
                        sim.method='euclidean',p.adjust.m='bonferroni') #se puede usar vegdist en vez de dist

wewe <- pairwise.adonis(x=norm_ps_target[,1:7],factors=norm_ps_target$vigor_id,sim.function='vegdist',
                        sim.method='euclidean',p.adjust.m='bonferroni')

wiwi <- pairwise.adonis(x=norm_pp_target[,1:7],factors=norm_pp_target$vigor_id,sim.function='vegdist',
                        sim.method='euclidean',p.adjust.m='bonferroni')

