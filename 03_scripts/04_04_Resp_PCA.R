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
  dplyr::select(c(wc_22, sla_22,
                  chlor_a_fw_22, percent_n,
                  leaf_d13c, leaf_d15n, leaf_d18o)) %>% 
  select(sort(names(.)))

# 6.- Data normalization ####

# However, since I do not know whether scale() does exactly the same as 
# the manual standardization, I will test both procedures

norm_target <- clean_target %>%
  mutate(chl_a_ST = (chlor_a_fw_22 - mean(chlor_a_fw_22, na.rm = T)) / sd(chlor_a_fw_22, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         leaf_d15n_ST = (leaf_d15n - mean(leaf_d15n, na.rm = T)) / sd(leaf_d15n, na.rm = T),
         leaf_d18o_ST = (leaf_d18o - mean(leaf_d18o, na.rm = T)) / sd(leaf_d18o, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         wc_ST = (wc_22 - mean(wc_22, na.rm = T)) / sd(wc_22, na.rm = T))

norm_target <- norm_target %>% select(contains("_ST"))

colnames(norm_target) <- c("Chl. a", "Leaves δ13C",
                            "Leaves δ15N", "Leaves δ18O", 
                            "Leaves N content", "SLA", 
                            "Leaves WC")

# 7.- Correlations matrix ####

# Omission of NAs
norm_target <- na.omit(norm_target)

# The chart is needed for the PCA:

correlogram <- cor(norm_target)
ggcorrplot(correlogram)

# 8.- PCA analysis ####

pca_results <- prcomp(norm_target, scale = F) # I already scaled by myself
summary(pca_results)

# 9.- Scree plot ####

scree <- fviz_eig(pca_results, addlabels = T, 
                  barfill = "black", barcolor = "black")

tiff("04_figures/04_05_Resp_screeplot.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
scree
dev.off()

# 10.- Biplot ####

tiff("04_figures/04_05_Resp_biplot.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
fviz_pca_var(pca_results, col.var = "black")
dev.off()

# 11.- Variable contribution ####

contrib <- fviz_cos2(pca_results, choice = "var", axes = 1:2,
                     fill = "black", color = "black")

tiff("04_figures/04_05_Resp_contribution_plot.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
contrib
dev.off()