rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "Hmisc", "PerformanceAnalytics") #list of packages
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
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot"))

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

# 5.- Variable selection and tidying ####

clean_target <- clean_target %>% 
  dplyr::select(c(height, mean_1980, sla_22, percent_n, total_chl_fw_22, xc_fw_22,
                  leaf_d13c, leaf_d18o_corrected, mean_def_obs, sp_id, spot_status, pair_id))
colnames(clean_target) <- c("Height", "BAI80", "SLA", "Leaf N", "Chl.", "Car.",
                            "δ13C", "δ18O", "Defoliation", "sp_id", "spot_status", "pair_id")

# 6.- Correlograms per species and sites ####
# And standardization per site!

aa_cold <- clean_target %>% filter(sp_id == "Abialba" & spot_status == "coldspot") %>% 
  group_by(pair_id) %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>% 
  ungroup() %>% dplyr::select(-c(sp_id, spot_status, pair_id))
aa_hot <- clean_target %>% filter(sp_id == "Abialba" & spot_status == "hotspot") %>% 
  group_by(pair_id) %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>% 
  ungroup() %>% dplyr::select(-c(sp_id, spot_status, pair_id))

ps_cold <- clean_target %>% filter(sp_id == "Pinsylv" & spot_status == "coldspot") %>% 
  group_by(pair_id) %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>% 
  ungroup() %>% dplyr::select(-c(sp_id, spot_status, pair_id))
ps_hot <- clean_target %>% filter(sp_id == "Pinsylv" & spot_status == "hotspot") %>% 
  group_by(pair_id) %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>% 
  ungroup() %>% dplyr::select(-c(sp_id, spot_status, pair_id))

pp_cold <- clean_target %>% filter(sp_id == "Pinpine" & spot_status == "coldspot") %>% 
  group_by(pair_id) %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>% 
  ungroup() %>% dplyr::select(-c(sp_id, spot_status, pair_id))
pp_hot <- clean_target %>% filter(sp_id == "Pinpine" & spot_status == "hotspot") %>% 
  group_by(pair_id) %>% 
  mutate(across(where(is.numeric), ~ as.numeric(scale(.)))) %>% 
  ungroup() %>% dplyr::select(-c(sp_id, spot_status, pair_id))

# Function to return points and geom_smooth
# allow for the method to be changed:
# Source: https://stackoverflow.com/questions/35085261/how-to-use-loess-method-in-ggallyggpairs-using-wrap-function

my_fn <- function(data, mapping, method = "loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(col = "#2274A5") + 
    geom_smooth(method = method, col = "black", ...)
  p
}

# This function is from chatgpt:
cor_sig <- function(data, mapping, method = "pearson",
                    size = 5, face = "bold", col = "black", digits = 3, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # correlation test
  test <- cor.test(x, y, method = method)
  r <- round(test$estimate, digits)
  
  # Only show if p < 0.05
  label <- if (test$p.value < 0.05) paste0("r = ", r) else ""
  
  ggplot(data = data, mapping = mapping) +
    annotate("text", x = 0.5, y = 0.5, label = label,
             size = size, fontface = face, color = col,
             hjust = 0.5, vjust = 0.5) +
    theme_void()
}

## 6.1.- Cold Aa ####

aa_cold_corr <- ggpairs(aa_cold, 
                        title = "A",
                        upper = list(continuous = cor_sig),
                        lower = list(continuous = my_fn)) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0,  # 0 = left, 0.5 = center, 1 = righ
                                  size = 20),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

## 6.2.- Cold Ps ####

ps_cold_corr <- ggpairs(ps_cold, 
                        title = "C",
                        upper = list(continuous = cor_sig),
                        lower = list(continuous = my_fn)) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0,  # 0 = left, 0.5 = center, 1 = righ
                                  size = 20),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

## 6.3.- Cold Pp ####

pp_cold_corr <- ggpairs(pp_cold, 
                        title = "E",
                        upper = list(continuous = cor_sig),
                        lower = list(continuous = my_fn)) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0,  # 0 = left, 0.5 = center, 1 = righ
                                  size = 20),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

## 6.4.- Hot Aa ####

my_fn <- function(data, mapping, method = "loess", ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(col = "#D71515") + 
    geom_smooth(method = method, col = "black", ...)
  p
}

aa_hot_corr <- ggpairs(aa_hot, 
                       title = "B",
                       upper = list(continuous = cor_sig),
                       lower = list(continuous = my_fn)) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0,  # 0 = left, 0.5 = center, 1 = righ
                                  size = 20),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

## 6.5.- Hot ps ####

ps_hot_corr <- ggpairs(ps_hot, 
                       title = "D",
                       upper = list(continuous = cor_sig),
                       lower = list(continuous = my_fn)) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0,  # 0 = left, 0.5 = center, 1 = righ
                                  size = 20),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))

## 6.6.- Hot ps ####

pp_hot_corr <- ggpairs(pp_hot, 
                       title = "E",
                       upper = list(continuous = cor_sig),
                       lower = list(continuous = my_fn)) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0,  # 0 = left, 0.5 = center, 1 = righ
                                  size = 20),
        strip.text.x = element_text(size = 15),
        strip.text.y = element_text(size = 15))


# 7.- Plotting ####

aa_cold_wrap <- wrap_elements(ggmatrix_gtable(aa_cold_corr))
aa_hot_wrap <- wrap_elements(ggmatrix_gtable(aa_hot_corr))

ps_cold_wrap <- wrap_elements(ggmatrix_gtable(ps_cold_corr))
ps_hot_wrap <- wrap_elements(ggmatrix_gtable(ps_hot_corr))

pp_cold_wrap <- wrap_elements(ggmatrix_gtable(pp_cold_corr))
pp_hot_wrap <- wrap_elements(ggmatrix_gtable(pp_hot_corr))

# Combine correlograms using patchwork

tiff("04_figures/99_99_Correlos_SD_site.tiff",
     units = "mm", width = 550, height = 825,
     res = 500, compression = "lzw")
aa_cold_wrap + aa_hot_wrap + ps_cold_wrap + ps_hot_wrap + pp_cold_wrap + 
  pp_hot_wrap + plot_layout(ncol = 2)
dev.off()

