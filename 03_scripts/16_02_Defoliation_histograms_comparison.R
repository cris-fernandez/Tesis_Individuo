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
  mutate(sp_id = factor(sp_id),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

# 5.- Adding systematic classification ####
# This classification is not based on thresholds but actually based on taking the 
# best 2 trees and the worst 2 trees. the other one is classified based on 
# whether its close to the min or the max

clean_target2 <- clean_target %>%
  filter(spot_status == "hotspot") %>% 
  group_by(plot_id) %>%
  arrange(mean_def_obs, .by_group = TRUE) %>%
  mutate(rank_in_plot = row_number()) %>%
  mutate(def_status = case_when(
    rank_in_plot %in% c(1, 2) ~ "healthy",
    rank_in_plot %in% c(4, 5) ~ "damaged",
    TRUE ~ NA_character_  # intermedio, se decidirá luego
  )) %>%
  # Ahora asignamos al intermedio en función de a qué extremo se parece más
  mutate(def_status = if_else(
    is.na(def_status),
    {
      mid_val <- mean_def_obs[rank_in_plot == 3]
      mean_healthy <- mean(mean_def_obs[rank_in_plot %in% c(1,2)], na.rm = TRUE)
      mean_damaged <- mean(mean_def_obs[rank_in_plot %in% c(4,5)], na.rm = TRUE)
      
      if (abs(mid_val - mean_damaged) < abs(mid_val - mean_healthy)) {
        "damaged"
      } else {
        "healthy"
      }
    },
    def_status
  )) %>%
  dplyr::select(-rank_in_plot) %>%
  ungroup()

clean_target2 <- clean_target2 %>% 
  dplyr::select(c(tree_id, def_status))

clean_target <- full_join(clean_target, clean_target2, by = "tree_id")
clean_target <- clean_target %>% 
  mutate(def_status = ifelse(is.na(def_status), "cold_healthy",
                             paste0("hot_", def_status)))

# 6.- Density plots ####
## 6.1.- All ####
density_all_def <- ggplot(data = clean_target) +
  geom_density(aes(x = mean_def_obs, fill = def_status), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  xlab("") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  xlim(0,100) + 
  ylim(0, 0.099) + 
  ggtitle("All") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30, face = "italic"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

density_all_vigor <- ggplot(data = clean_target) +
  geom_density(aes(x = mean_def_obs, fill = vigor_id), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  xlab("") + 
  ylab("Density") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 0.099) + 
  ggtitle("All") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

## 6.2.- Abies alba ####

density_abialba_def <- ggplot(data = clean_target[clean_target$sp_id == "Abialba", ]) +
  geom_density(aes(x = mean_def_obs, fill = def_status), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  xlab("") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  xlim(0,100) + 
  ylim(0, 0.099) + 
  ggtitle("Abies alba") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30, face = "italic"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

density_abialba_vigor <- ggplot(data = clean_target[clean_target$sp_id == "Abialba", ]) +
  geom_density(aes(x = mean_def_obs, fill = vigor_id), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  xlab("") + 
  ylab("Density") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 0.099) + 
  ggtitle("Abies alba") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

## 6.3.- Pinus sylvestris ####

density_pinsylv_def <- ggplot(data = clean_target[clean_target$sp_id == "Pinsylv", ]) +
  geom_density(aes(x = mean_def_obs, fill = def_status), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  xlab("") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  xlim(0,100) + 
  ylim(0, 0.099) + 
  ggtitle("Pinus sylvestris") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30, face = "italic"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

density_pinsylv_vigor <- ggplot(data = clean_target[clean_target$sp_id == "Pinsylv", ]) +
  geom_density(aes(x = mean_def_obs, fill = vigor_id), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  xlab("") + 
  ylab("Density") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 0.099) + 
  ggtitle("Pinus sylvestris") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

## 6.4.- Pinus pinea ####

density_pinpine_def <- ggplot(data = clean_target[clean_target$sp_id == "Pinpine", ]) +
  geom_density(aes(x = mean_def_obs, fill = def_status), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  xlab("") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  xlim(0,100) + 
  ylim(0, 0.099) + 
  ggtitle("Pinus pinea") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30, face = "italic"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

density_pinpine_vigor <- ggplot(data = clean_target[clean_target$sp_id == "Pinpine", ]) +
  geom_density(aes(x = mean_def_obs, fill = vigor_id), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  xlab("") + 
  ylab("Density") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 0.099) + 
  ggtitle("Pinus pinea") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

# 10.- Plotting ####

tiff("04_figures/16_02_Defoliation_comparison.tiff", units = "mm", width = 350, height = 600,
     res = 400, compression = "lzw")
density_all_vigor + density_all_def + density_abialba_vigor + density_abialba_def + 
  density_pinsylv_vigor + density_pinsylv_def + density_pinpine_vigor + density_pinpine_def + 
  guide_area() + plot_layout(guides = 'collect', ncol = 2)
dev.off()
