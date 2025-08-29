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

clean_target <- clean_target %>% filter(mean_def_obs < 100) %>% 
  filter(!vigor_id == "cold_healthy")

# 5.- Plotting function ####
## 5.1.- Colour scales ####

spot_colors <- c("hot_healthy" = "#D71515", "hot_damaged" = "#650304")
spot_labels <- c("Healthy tree", "Damaged tree")
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

# 6.- Physio ####
## 6.1.- LWC ####

wc_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = wc_22, color = vigor_id)) + 
  geom_smooth(aes(x = mean_def_obs, y = wc_22, col = vigor_id, fill = vigor_id),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "A") +
  ylab("LWC (%)") + 
  ylim(40, 70) + 
  spot_scale

## 6.2.- Chl. ####

chl_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = wc_22, color = vigor_id)) + 
  geom_smooth(aes(x = mean_def_obs, y = wc_22, col = vigor_id, fill = vigor_id),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "B") +
  ylab(expression(paste("Chl. (μg g"^"-1", ")"))) + 
  spot_scale

## 6.3.- Chl. a/b ####

chlab_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = chla_chlb_22, color = vigor_id)) + 
  geom_smooth(aes(x = mean_def_obs, y = chla_chlb_22, col = vigor_id, fill = vigor_id),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "C") +
  ylab("Chl. a/b") + 
  spot_scale

## 6.4.- Carotenoids ####

xc_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = xc_fw_22, color = vigor_id)) + 
  geom_smooth(aes(x = mean_def_obs, y = xc_fw_22, col = vigor_id, fill = vigor_id),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "D") +
  ylab(expression(paste("Caroten. (μg g"^"-1", ")"))) + 
  spot_scale

## 6.5.- Chl / xc ####

chlxc_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = chl_xc_22, color = vigor_id)) + 
  geom_smooth(aes(x = mean_def_obs, y = chl_xc_22, col = vigor_id, fill = vigor_id),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "E") +
  ylab("Chl. / car.") + 
  spot_scale

## 6.6.- d13C  ####

d13c_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = leaf_d13c, color = vigor_id)) + 
  geom_smooth(aes(x = mean_def_obs, y = leaf_d13c, col = vigor_id, fill = vigor_id),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "F") +
  ylab(bquote("δ"~C^13~"(‰)")) + 
  spot_scale

## 6.7.- d15N  ####

d15n_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = leaf_d15n, color = vigor_id)) + 
  geom_smooth(aes(x = mean_def_obs, y = leaf_d15n, col = vigor_id, fill = vigor_id),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "G") +
  ylab(bquote("δ"~N^15~"(‰)")) + 
  spot_scale + 
  xlab("Defoliation (%)") + 
  theme(axis.text.x = element_text(size = 22),
        axis.title.x = element_text(size = 30))

## 6.8.- d18O  ####

d18o_dot <- ggplot(clean_target) + 
  geom_point(aes(x = mean_def_obs, y = leaf_d18o, color = vigor_id)) + 
  geom_smooth(aes(x = mean_def_obs, y = leaf_d18o, col = vigor_id, fill = vigor_id),
              method = "loess", se = TRUE, size = 1, alpha = 0.2) + 
  labs(tag = "H") +
  ylab(bquote("δ"~O^18~"(‰)")) + 
  spot_scale + 
  xlab("Defoliation (%)") + 
  theme(axis.text.x = element_text(size = 22),
        axis.title.x = element_text(size = 30))

# 8.- Plotting ####

tiff("04_figures/28_03_Scatter_physio3.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
wc_dot + chl_dot + chlab_dot + xc_dot + chlxc_dot + d13c_dot + d15n_dot + d18o_dot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()