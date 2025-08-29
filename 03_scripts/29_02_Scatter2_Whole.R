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

clean_target$cn <- clean_target$percent_c / clean_target$percent_n

clean_target <- clean_target %>% filter(mean_def_obs < 100)

# 5.- Plotting function ####
## 5.1.- Colour scales ####

spot_colors <- c("coldspot" = "#2274A5", "hotspot" = "#D71515")
spot_labels <- c("Non-declining site", "Declining site")

## 5.2.- Reusable scales ####

spot_scale <- list(
  scale_colour_manual(values = spot_colors, breaks = names(spot_colors), labels = spot_labels),
  scale_fill_manual(values = spot_colors, breaks = names(spot_colors), labels = spot_labels),
  theme_classic(),
  theme(legend.position = "right",
        legend.key.size = unit(2, "cm"),  
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 30)))

# 6.- Whole plant ####
## 6.1.- BAI80 ####

bai80_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = mean_1980, color = spot_status)) + 
  geom_smooth(aes(x = mean_def_obs, y = mean_1980, col = spot_status, fill = spot_status),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "A") +
  ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + 
  spot_scale

## 6.2.- BAI05 ####

bai05_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = mean_05, color = spot_status)) + 
  geom_smooth(aes(x = mean_def_obs, y = mean_05, col = spot_status, fill = spot_status),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "B") +
  ylab(expression(paste("BAI05 (mm² year"^"-1", ")"))) + 
  spot_scale

## 6.3.- Rt12 ####

rt12_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = Rt12, color = spot_status)) + 
  geom_smooth(aes(x = mean_def_obs, y = Rt12, col = spot_status, fill = spot_status),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "C") +
  ylab("Rt 2012") + 
  spot_scale

## 6.4.- Rt17 ####

rt17_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = Rt17, color = spot_status)) + 
  geom_smooth(aes(x = mean_def_obs, y = Rt17, col = spot_status, fill = spot_status),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "D") +
  ylab("Rt 2017") + 
  spot_scale

## 6.5.- Rt22 ####

rt22_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = Rt22, color = spot_status)) + 
  geom_smooth(aes(x = mean_def_obs, y = Rt22, col = spot_status, fill = spot_status),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "E") +
  ylab("Rt 2022") + 
  spot_scale

## 6.6.- Rs12  ####

rs12_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = Rs12, color = spot_status)) + 
  geom_smooth(aes(x = mean_def_obs, y = Rs12, col = spot_status, fill = spot_status),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "F") +
  ylab("Rs 2012") + 
  spot_scale

## 6.7.- Rs17  ####

rs17_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = Rs17, color = spot_status)) + 
  geom_smooth(aes(x = mean_def_obs, y = Rs17, col = spot_status, fill = spot_status),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "G") +
  ylab("Rs 2017") + 
  spot_scale + 
  xlab("Defoliation (%)") + 
  theme(axis.text.x = element_text(size = 22),
        axis.title.x = element_text(size = 30))

# 8.- Plotting ####

tiff("04_figures/29_02_Scatter_whole2.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
bai80_dot + bai05_dot + rt12_dot + rt17_dot + rt22_dot + rs12_dot + rs17_dot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()