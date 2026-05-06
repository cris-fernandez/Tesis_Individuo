rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "lme4", "lmerTest", "emmeans", "mgcv", "broom.mixed", "xlsx", "ggtext") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading data ####

ci_df_2 <- read.csv("02_clean_data/63_01_AICc_discrete2.csv", 
                    header = T, sep = ",") %>% dplyr::select(-X)

ci_df_3 <- read.csv("02_clean_data/63_02_AICc_discrete3.csv", 
                    header = T, sep = ",") %>% dplyr::select(-X)

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv",
                         header = T, sep = ",") %>% dplyr::select(-X) %>%
  mutate(site = substr(plot_id, 1, 3))
# 
# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3))

# 2.- Variable tidying ####

ci_df_2$spot_status <- factor(ci_df_2$spot_status, levels = c("coldspot", "hotspot"))
ci_df_2$sp_id <- factor(ci_df_2$sp_id, levels = c("Abialba", "Pinsylv", "Pinpine"))

ci_df_3$vigor_id <- factor(ci_df_3$vigor_id, levels = c("hot_healthy", "hot_damaged"))
ci_df_3$sp_id <- factor(ci_df_3$sp_id, levels = c("Abialba", "Pinsylv", "Pinpine"))

# 3.- Clean target data tidying ####

clean_target <- clean_target %>% 
  dplyr::select(-contains("_23")) %>% 
  filter(mean_def_obs < 100)

# Adding T290 defoliation info:
clean_target <- clean_target %>% 
  mutate(mean_def_obs = ifelse(tree_number == "T290", 15, mean_def_obs))

# Additional IDs ####
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

# Data corrections #####
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
  mutate(sp_id = factor(sp_id),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

# Outlayers deletion:

clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 4783, NA, clean_target$mean_1980)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 60 & clean_target$sp_id == "Abialba",
                                    NA, clean_target$mean_def_obs)
clean_target$sla_22 <- ifelse(clean_target$sla_22 > 99 & clean_target$sp_id == "Pinsylv",
                              NA, clean_target$sla_22)
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 < 150 & clean_target$sp_id == "Pinsylv",
                                       NA, clean_target$total_chl_fw_22)
clean_target$xc_fw_22 <- ifelse(clean_target$xc_fw_22 < 5 & clean_target$sp_id == "Pinsylv",
                                       NA, clean_target$xc_fw_22)
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 < 40 & clean_target$sp_id == "Pinpine",
                                       NA, clean_target$total_chl_fw_22)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 58 & clean_target$sp_id == "Pinpine",
                                    NA, clean_target$mean_def_obs)
clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 3000 & clean_target$sp_id == "Abialba" & clean_target$spot_status == "hotspot",
                                 NA, clean_target$mean_1980)


clean_target$pair_id <- factor(clean_target$pair_id , 
                               levels = c("Hue-Abialba", "Nav-Abialba", 
                                          "Nav-Pinsylv", "Ter-Pinsylv",
                                          "Gua-Pinsylv", "Mad-Pinsylv",
                                          "Mad-Pinpine"))

# 4.- Plotting coeffs. variation ####

## 4.1.- Chl ####

chl_plot <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = total_chl_fw_22, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  ylab(expression(paste("Chl. (μg g"^"-1", ")"))) +
  xlab("") + 
  labs(tag = "A") + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 30),
        legend.text = element_text(size = 35,
                                   face = "italic"),
        legend.direction = "horizontal")

## 4.2.- Car. ####

xc_plot <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = xc_fw_22, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  ylab(expression(paste("Car. (μg g"^"-1", ")"))) +
  xlab("") + 
  labs(tag = "B") + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 30),
        legend.text = element_text(size = 35,
                                   face = "italic"),
        legend.direction = "horizontal")

## 4.3.- d13C ####

d13c_plot <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = leaf_d13c, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  ylab(bquote("δ"~C^13~"(‰)")) +
  xlab("") + 
  labs(tag = "C") + 
  scale_x_discrete(labels = c("Huesca", "Navarra", "Navarra", "Teruel", 
                                "Guadalajara", "Madrid", "Madrid")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 20,
                                   angle = 45, 
                                   hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 30),
        legend.text = element_text(size = 35,
                                   face = "italic"),
        legend.direction = "horizontal")

## 4.4.- d18O ####

d18o_plot <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = leaf_d18o_corrected, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  ylab(bquote("δ"~O^18~"(‰)")) +
  xlab("") + 
  labs(tag = "D") +  
  scale_x_discrete(labels = c("Huesca", "Navarra", "Navarra", "Teruel", 
                                                    "Guadalajara", "Madrid", "Madrid")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 20,
                                   angle = 45, 
                                   hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 30),
        legend.text = element_text(size = 35,
                                   face = "italic"),
        legend.direction = "horizontal")

# 5.- Plotting ####
figures <- chl_plot + xc_plot + d13c_plot + d18o_plot

tiff("04_figures/99_99_coeffs_variation_leaf.tiff",
     units = "mm", width = 400, height = 400,
     res = 600, compression = "lzw")

final_plot <- (figures / guide_area()) + 
  plot_layout(guides = "collect",
              heights = c(1, 0.1)) &
  guides(fill = guide_legend(nrow = 1),
         shape = "none") & 
  theme(legend.box = "horizontal")

final_plot

dev.off()