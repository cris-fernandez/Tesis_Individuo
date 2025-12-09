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
                         header = T, sep = ",") %>% dplyr::select(-X) %>% 
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

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(height, sla_22, percent_n, total_chl_fw_22, xc_fw_22,  
                  leaf_d13c, leaf_d18o_corrected, mean_1980, mean_def_obs, sp_id, spot_status))

colnames(clean_target) <- c("Height", "SLA", "N content", "Chl. content", 
                            "Car. content", "Leaf δ13C", "Leaf δ18O", 
                            "BAI 1980", "Defoliation", "sp_id", "spot_status")

# Filtering per species separately

aa_target <- clean_target %>% filter(sp_id == "Abialba" & spot_status == "coldspot") %>% 
  dplyr::select(-c(sp_id, spot_status))
ps_target <- clean_target %>% filter(sp_id == "Pinsylv" & spot_status == "coldspot") %>% 
  dplyr::select(-c(sp_id, spot_status))
pp_target <- clean_target %>% filter(sp_id == "Pinpine" & spot_status == "coldspot") %>% 
  dplyr::select(-c(sp_id, spot_status))

# 6.- Correlogram ####
# First I need to remove na values from the correlogram 

aa_target <- na.omit(aa_target)
ps_target <- na.omit(ps_target)
pp_target <- na.omit(pp_target)

# Correlogram 

correlogram_aa <- cor(aa_target)
correlogram_ps <- cor(ps_target)
correlogram_pp <- cor(pp_target)

# P-value matrix creation

aa_matrix <- cor_pmat(aa_target)
ps_matrix <- cor_pmat(ps_target)
pp_matrix <- cor_pmat(pp_target)

correlogram_aa <- ggcorrplot(correlogram_aa, 
                             type = "lower",
                             lab = TRUE,
                             method = "circle", 
                             p.mat = aa_matrix, 
                             insig = "blank",
                             hc.order = FALSE)

correlogram_ps <- ggcorrplot(correlogram_ps, 
                             type = "lower",
                             lab = TRUE,
                             method = "circle", 
                             p.mat = ps_matrix, 
                             insig = "blank",
                             hc.order = FALSE)

correlogram_pp <- ggcorrplot(correlogram_pp, 
                             type = "lower",
                             lab = TRUE,
                             method = "circle", 
                             p.mat = pp_matrix, 
                             insig = "blank",
                             hc.order = FALSE)



# Adding titles to the graph: 
correlogram_aa2 <- correlogram_aa +
  labs(title = "*Abies alba*") + 
  theme(axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1, size = 12),
        axis.text.y = ggtext::element_markdown(size = 12),
        title = element_text(face = "italic"))

correlogram_ps2 <- correlogram_ps +
  labs(title = "*Pinus sylvestris*") + 
  theme(axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1, size = 12),
        axis.text.y = ggtext::element_markdown(size = 12),
        title = element_text(face = "italic"))

correlogram_pp2 <- correlogram_pp +
  labs(title = "Pinus pinea") + 
  theme(axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1, size = 12),
        axis.text.y = ggtext::element_markdown(size = 12),
        title = element_text(face = "italic"))


# 8.- Plotting ####

tiff("04_figures/30_08_correlogram_aa_cold.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
correlogram_aa2
dev.off()

tiff("04_figures/30_08_correlogram_ps_cold.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
correlogram_ps2
dev.off()

tiff("04_figures/30_08_correlogram_pp_cold.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
correlogram_pp2
dev.off()
