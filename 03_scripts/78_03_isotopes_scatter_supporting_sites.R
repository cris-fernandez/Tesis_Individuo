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

hue_aa_target <- clean_target %>% filter(pair_id == "Hue-Abialba")
nav_aa_target <- clean_target %>% filter(pair_id == "Nav-Abialba")

nav_ps_target <- clean_target %>% filter(pair_id == "Nav-Pinsylv")
ter_ps_target <- clean_target %>% filter(pair_id == "Ter-Pinsylv")
gua_ps_target <- clean_target %>% filter(pair_id == "Gua-Pinsylv")
mad_ps_target <- clean_target %>% filter(pair_id == "Mad-Pinsylv")

mad_pp_target <- clean_target %>% filter(pair_id == "Mad-Pinpine")

# 5.- Plotting theme ####

spot_theme <- list(theme_classic(),
                   theme(legend.position = "right",
                         legend.key.size = unit(2, "cm"),  
                         legend.title = element_blank(),
                         axis.text.x = element_text(size = 20),
                         axis.title.x = element_text(size = 25),
                         axis.text.y = element_text(size = 20),
                         axis.title.y = element_text(size = 25),
                         legend.text = element_text(size = 25),
                         plot.tag = element_text(size = 25,
                                                 face = "bold"),
                         plot.title = element_text(size = 22,
                                                   vjust= 1.9)))

# 6.- Plotting Abies alba ####
## 6.1.- Huesca ####

hue_aa_plot <- ggplot(hue_aa_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining\nstands",
                                "Declining\nstands"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining\nstands",
                               "Declining\nstands"),
                    name = "") + 
  xlab("") + 
  ylab(bquote("Δ"^{18}*O~"(‰)")) +
  labs(tag = "(a)",
       title = expression(italic("A. alba") * " - Huesca")) +
  spot_theme + 
  # theme(plot.title = element_text(hjust = -0.65)) + 
  guides(shape = "none")

## 6.2.- Navarra ####

nav_aa_plot <- ggplot(nav_aa_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining\nstands",
                                "Declining\nstands"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining\nstands",
                               "Declining\nstands"),
                    name = "") + 
  xlab("") + 
  ylab("") +
  labs(tag = "(b)",
       title = expression(italic("A. alba") * " - Navarra")) +
  spot_theme + 
  # theme(plot.title = element_text(hjust = -0.70)) + 
  guides(shape = "none")

# 7.- Plotting Pinus sylvestris ####
## 7.1.- Navarra ####

nav_ps_plot <- ggplot(nav_ps_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining\nstands",
                                "Declining\nstands"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining\nstands",
                               "Declining\nstands"),
                    name = "") + 
  xlab("") + 
  ylab(bquote("Δ"^{18}*O~"(‰)")) +
  labs(tag = "(c)",
       title = expression(italic("P. sylvestris") * " - Navarra")) +
  spot_theme + 
  # theme(plot.title = element_text(hjust = 4)) + 
  guides(shape = "none")

## 7.2.- Teruel ####

ter_ps_plot <- ggplot(ter_ps_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining\nstands",
                                "Declining\nstands"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining\nstands",
                               "Declining\nstands"),
                    name = "") + 
  xlab("") + 
  ylab("") +
  labs(tag = "(d)",
       title = expression(italic("P. sylvestris") * " - Teruel")) +
  spot_theme +
  # theme(plot.title = element_text(hjust = -2)) + 
  guides(shape = "none")

## 7.3.- Guadalajara ####

gua_ps_plot <- ggplot(gua_ps_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining\nstands",
                                "Declining\nstands"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining\nstands",
                               "Declining\nstands"),
                    name = "") + 
  xlab("") + 
  ylab(bquote("Δ"^{18}*O~"(‰)")) +
  labs(tag = "(e)",
       title = expression(italic("P. sylvestris") * " - Guadalajara")) +
  spot_theme + 
  # theme(plot.title = element_text(hjust = 0.89)) + 
  guides(shape = "none")

## 7.4.- Madrid ####

mad_ps_plot <- ggplot(mad_ps_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining\nstands",
                                "Declining\nstands"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining\nstands",
                               "Declining\nstands"),
                    name = "") + 
  xlab(bquote("δ"^{13}*C~"(‰)")) +
  ylab("") +
  labs(tag = "(f)",
       title = expression(italic("P. sylvestris") * " - Madrid")) +
  spot_theme + 
  # theme(plot.title = element_text(hjust = -2.8)) + 
  guides(shape = "none")

# 8.- Pinus pinea ####
## 8.1.- Madrid ####

mad_pp_plot <- ggplot(mad_pp_target) + 
  geom_point(aes(x = leaf_d13c, y = leaf_d18o_corrected, colour = spot_status,
                 shape = pair_id), size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected),
              method = "lm",
              colour = "black", fill = "black", alpha = 0.2) + 
  geom_smooth(aes(x = leaf_d13c, y = leaf_d18o_corrected,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2, width = 1.1) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining\nstands",
                                "Declining\nstands"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining\nstands",
                               "Declining\nstands"),
                    name = "") + 
  xlab(bquote("δ"^{13}*C~"(‰)")) +
  ylab(bquote("Δ"^{18}*O~"(‰)")) +
  labs(tag = "(g)",
       title = expression(italic("P. pinea") * " - Madrid")) + 
  spot_theme +
  # theme(plot.title = element_text(hjust = -0.29)) +
  guides(shape = "none")

# 7.- Plotting ####

tiff("04_figures/78_03_Isotopes_Scatter_Supporting_sites_v2.tiff", units = "mm", width = 270, height = 450,
     res = 400, compression = "lzw")
hue_aa_plot + nav_aa_plot + 
  nav_ps_plot + ter_ps_plot + 
  gua_ps_plot + mad_ps_plot +
  mad_pp_plot + guide_area() + 
  plot_layout(ncol = 2, guides = "collect")
dev.off()
