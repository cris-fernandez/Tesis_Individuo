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
  dplyr::select(-contains("_23"))

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
# This time we are selecting just vulnerability-related variables

clean_target <- clean_target %>% 
  rename(mean_bai = mean) %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  dplyr::select(c(wc_22, ewt_22, sla_22,
                  chl_fw_22, chlor_a_fw_22, chlor_b_fw_22, chla_chlb_22, 
                  xc_fw_22, chl_xc_22, percent_c, percent_n, cn_ratio,
                  leaf_d13c, leaf_d15n, leaf_d18o,
                  wood_d13c_17, wood_d13c_22)) %>% 
  select(sort(names(.)))

# colnames(clean_target) <- c("Chl.", "Chl. / Carotenoids ratio", 
#                             "Chl. a / Chl. b ratio", "Chl. a", "Chl. b",
#                             "Leaves C:N ratio", "EWT", "Leaves δ13C", 
#                             "Leaves δ15N", "Leaves δ18O", "Leaves C content",
#                             "Leaves N content", "SLA", "Leaves WC", 
#                             "Wood δ13C 2017", "Wood δ13C 2022",
#                             "Carotenoids")

# 6.- Correlogram ####
# First I need to remove na values from the correlogram 

clean_target2 <- na.omit(clean_target)

# Now I make the correlogram and reorder the variables in alphabetical order

correlogram <- cor(clean_target2)
orden <- sort(colnames(correlogram)) %>% rev()
correlogram <- correlogram[orden, orden]

# P-value matrix creation, also by alphabetical order

p_matrix <- cor_pmat(clean_target2)
p_matrix <- p_matrix[orden, orden]

correlogram <- ggcorrplot(correlogram, 
                          type = "lower",
                          lab = TRUE,
                          method = "circle", 
                          p.mat = p_matrix, 
                          insig = "blank")

# 7.- Plotting ####

tiff("04_figures/04_04_resp_correlogram.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
correlogram
dev.off()

# 8.- Correlogram - no wood isotopes ####

# Data availability for wood isotopes is limited, thus reducing the rows with 
# no NAs that can be utilized for the correlogram...

clean_target3 <- clean_target %>% 
  dplyr::select(-c(`Wood δ13C 2017`, `Wood δ13C 2022`)) %>% 
  na.omit() # 345 instead of 90 observations... much more

# Now I make the correlogram and reorder the variables in alphabetical order

correlogram_nowood <- cor(clean_target3)
orden_nowood <- sort(colnames(correlogram_nowood)) %>% rev()
correlogram_nowood <- correlogram_nowood[orden_nowood, orden_nowood]

# P-value matrix creation, also by alphabetical order

p_matrix_nowood <- cor_pmat(clean_target3)
p_matrix_nowood <- p_matrix_nowood[orden_nowood, orden_nowood]

correlogram_nowood <- ggcorrplot(correlogram_nowood, 
                                 type = "lower",
                                 lab = TRUE,
                                 method = "circle", 
                                 p.mat = p_matrix_nowood, 
                                 insig = "blank")

# 9.- Plotting ####

tiff("04_figures/04_04_resp_correlogram_nowood.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
correlogram_nowood
dev.off()
