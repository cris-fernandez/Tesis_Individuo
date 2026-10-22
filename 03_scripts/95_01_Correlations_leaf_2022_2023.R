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

# 2.- Adding T290 defoliation info:

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

clean_target$sla_23 <- ifelse(clean_target$sla_23 > 200, NA,
                              clean_target$sla_23)
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
         spot_status = fct_relevel(spot_status, "Coldspot", "Hotspot")) %>% 
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
clean_target$total_chl_fw_23 <- ifelse(clean_target$total_chl_fw_23 > 100000,
                                       NA, clean_target$total_chl_fw_23)
clean_target$xc_fw_23 <- ifelse(clean_target$xc_fw_23 < 0, NA, clean_target$xc_fw_23)
clean_target$xc_fw_23 <- ifelse(clean_target$xc_fw_23 > 20000, NA, clean_target$xc_fw_23)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 58 & clean_target$sp_id == "Pinpine",
                                    NA, clean_target$mean_def_obs)
clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 3000 & clean_target$sp_id == "Abialba" & clean_target$spot_status == "hotspot",
                                 NA, clean_target$mean_1980)

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  dplyr::select(c(total_chl_fw_22, total_chl_fw_23, xc_fw_22, xc_fw_23, 
                  sla_22, sla_23, tree_number, sp_id, spot_status,
                  pair_id))

# 6.- Plotting ####

spot_theme <- list(theme_classic(),
                   theme(legend.position = "right",
                         legend.key.size = unit(2, "cm"),  
                         legend.title=element_blank(),
                         axis.text.x = element_text(size = 22),
                         axis.title.x = element_text(size = 30),
                         axis.text.y = element_text(size = 22),
                         axis.title.y = element_text(size = 30),
                         legend.text = element_text(size = 25),
                         plot.tag = element_text(size = 25,
                                                 face = "bold")))

## 6.1.- SLA ####

sla_plot <- ggplot(clean_target) + 
  geom_point(aes(x = sla_22, y = sla_23, colour = spot_status), 
             size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = sla_22, y = sla_23,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  geom_text(label = paste0("r = 0.682\np < 2.2e-16"),
            aes(x = 36, y = 125, label = label),
            # hjust = 1.1, vjust = 1.5,
            size = 9) +
  scale_color_manual(breaks = c("Coldspot", "Hotspot"),
                     values = c("Coldspot" = "#2274A5",
                                "Hotspot" = "#D71515"),
                     labels = c("Non-declining stands",
                                "Declining stands"),
                     name = "") + 
  scale_fill_manual(breaks = c("Coldspot", "Hotspot"),
                    values = c("Coldspot" = "#2274A5",
                               "Hotspot" = "#D71515"),
                    labels = c("Non-declining stands",
                               "Declining stands"),
                    name = "") + 
  xlab(expression(paste("SLA 2022 (cm² g"^"-1", ")"))) + 
  ylab(expression(paste("SLA 2023 (cm² g"^"-1", ")"))) + 
  labs(tag = "(a)") +
  spot_theme + 
  guides(shape = "none")

## 6.2.- Chl. ####

chl_plot <- ggplot(clean_target) + 
  geom_point(aes(x = total_chl_fw_22, y = total_chl_fw_23, colour = spot_status), 
             size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = total_chl_fw_22, y = total_chl_fw_23,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  geom_text(label = paste0("r = 0.646\np < 2.2e-16"),
            aes(x = 500, y = 2500, label = label),
            # hjust = 1.1, vjust = 1.5,
            size = 9) +
  scale_color_manual(breaks = c("Coldspot", "Hotspot"),
                     values = c("Coldspot" = "#2274A5",
                                "Hotspot" = "#D71515"),
                     labels = c("Non-declining stands",
                                "Declining stands"),
                     name = "") + 
  scale_fill_manual(breaks = c("Coldspot", "Hotspot"),
                    values = c("Coldspot" = "#2274A5",
                               "Hotspot" = "#D71515"),
                    labels = c("Non-declining stands",
                               "Declining stands"),
                    name = "") + 
  xlab(expression(paste("Chl. 2022 (μg g"^"-1", ")"))) +
  ylab(expression(paste("Chl. 2023 (μg g"^"-1", ")"))) +
  labs(tag = "(b)") +
  spot_theme + 
  guides(shape = "none")

## 6.3.- Carotenoids ####

xc_plot <- ggplot(clean_target) + 
  geom_point(aes(x = xc_fw_22, y = xc_fw_23, colour = spot_status), 
             size = 3, alpha = 0.75) + 
  geom_smooth(aes(x = xc_fw_22, y = xc_fw_23,
                  colour = spot_status, fill = spot_status),
              method = "lm", alpha = 0.2) + 
  geom_text(label = paste0("r = 0.620\np < 2.2e-16"),
    aes(x = 17, y = 75, label = label),
    # hjust = 1.1, vjust = 1.5,
    size = 9) +
  scale_color_manual(breaks = c("Coldspot", "Hotspot"),
                     values = c("Coldspot" = "#2274A5",
                                "Hotspot" = "#D71515"),
                     labels = c("Non-declining stands",
                                "Declining stands"),
                     name = "") + 
  scale_fill_manual(breaks = c("Coldspot", "Hotspot"),
                    values = c("Coldspot" = "#2274A5",
                               "Hotspot" = "#D71515"),
                    labels = c("Non-declining stands",
                               "Declining stands"),
                    name = "") + 
  xlab(expression(paste("Car. 2022 (μg g"^"-1", ")"))) +
  ylab(expression(paste("Car. 2023 (μg g"^"-1", ")"))) +
  labs(tag = "(c)") +
  spot_theme + 
  guides(shape = "none")


tiff("04_figures/95_01_Correlos_leaf_22_23.tiff",
     units = "mm", width = 400, height = 400,
     res = 500, compression = "lzw")
sla_plot + chl_plot + xc_plot + guide_area() + 
  plot_layout(ncol = 2, guides = "collect")
dev.off()

# Correlations!
cor.test(clean_target$sla_22, clean_target$sla_23, use = "complete.obs")
cor.test(clean_target$total_chl_fw_22, clean_target$total_chl_fw_23, use = "complete.obs")
cor.test(clean_target$xc_fw_22, clean_target$xc_fw_23, use="complete.obs")
