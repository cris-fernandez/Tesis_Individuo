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

# 6.- Filtering by species ####

aa_target <- clean_target %>% filter(sp_id == "Abialba")
ps_target <- clean_target %>% filter(sp_id == "Pinsylv")
pp_target <- clean_target %>% filter(sp_id == "Pinpine")

# 5.- Plotting theme ####

spot_theme <- list(theme_classic(),
                   theme(legend.position = "right",
                         legend.key.size = unit(2, "cm"),  
                         legend.title=element_blank(),
                         axis.text.x = element_text(size = 22),
                         axis.title.x = element_text(size = 30),
                         axis.text.y = element_text(size = 22),
                         axis.title.y = element_text(size = 30),
                         legend.text = element_text(size = 25),
                         plot.tag = element_text(size = 30)))

# 6.- Plotting ####
## 6.1.- Abies alba ####

aa_plot <- ggplot(aa_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  scale_shape_manual(values = c(16,15)) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  xlab("") + 
  ylab(bquote("δ"^{18}*O~"(‰)")) +
  labs(tag = "A") +
  spot_theme + 
  guides(shape = "none")

## 6.2.- Pinus sylvestris ####

ps_plot <- ggplot(ps_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  scale_shape_manual(values = c(16,15,17,18)) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining",
                               "Declining"),
                    name = "") + 
  xlab(bquote("δ"^{13}*C~"(‰)")) +
  ylab("") +
  labs(tag = "B") +
  spot_theme + 
  guides(shape = "none")

## 6.3.- Pinus pinea ####

pp_plot <- ggplot(pp_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  scale_shape_manual(values = c(16)) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2, width = 1.1) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining",
                               "Declining"),
                    name = "") + 
  xlab(bquote("δ"^{13}*C~"(‰)")) +
  ylab(bquote("δ"^{18}*O~"(‰)")) +
  labs(tag = "C") +
  spot_theme + 
  guides(shape = "none")

# 7.- Plotting ####

tiff("04_figures/78_02_Isotopes_Scatter_Supporting_lines.tiff", units = "mm", width = 220, height = 200,
     res = 400, compression = "lzw")
aa_plot + ps_plot + 
  pp_plot  + guide_area() + 
  plot_layout(ncol = 2, guides = "collect")
dev.off()
