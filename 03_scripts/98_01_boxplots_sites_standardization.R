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
clean_target$pair_id <- factor(clean_target$pair_id , 
                               levels = c("Nav-Abialba", "Hue-Abialba", 
                                          "Nav-Pinsylv", "Ter-Pinsylv",
                                          "Gua-Pinsylv", "Mad-Pinsylv",
                                          "Mad-Pinpine"))

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

# 5.- Boxplots ####
## 5.1.- Height ####

box_h <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = height, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "(a)") +
  xlab("") + 
  ylab("Tree height (m)") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 30, vjust = 1.15),
        legend.text = element_text(size = 23),
        plot.tag = element_text(size = 22,
                                face = "bold")) 

## 5.2.- BAI80 ####

box_bai <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = mean_1980, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "(b)") +
  xlab("") + 
  ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 30, vjust = 1.15),
        legend.text = element_text(size = 23),
        plot.tag = element_text(size = 22,
                                face = "bold")) 

## 5.3.- SLA ####

box_sla <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = sla_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "(c)") +
  ylab(expression(paste("SLA (cm² g"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 30, vjust = 1.15),
        legend.text = element_text(size = 23),
        plot.tag = element_text(size = 22,
                                face = "bold"))

## 5.4.- N content ####

box_n <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = percent_n, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "(d)") +
  ylab(expression(paste("Leaf N content (%)"))) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 20, angle = 60, vjust = -0.09),
        axis.title.x = element_text(size = 30, vjust = 1.15),
        legend.text = element_text(size = 23),
        plot.tag = element_text(size = 22,
                                face = "bold"))

## 5.5.- Chl ####

box_chl <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = total_chl_fw_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "(e)") +
  xlab("") + 
  ylab(expression(paste("Chl. content (μg g"^"-1", ")"))) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 30, vjust = 1.15),
        legend.text = element_text(size = 23),
        plot.tag = element_text(size = 22,
                                face = "bold")) 


## 5.6.- Carotenoids ####

box_xc <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = xc_fw_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "(f)") +
  xlab("") + 
  ylab(expression(paste("Car. content (μg g"^"-1", ")"))) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 30, vjust = 1.15),
        legend.text = element_text(size = 23),
        plot.tag = element_text(size = 22,
                                face = "bold")) 

## 5.7.- d13C ####

box_d13c <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = leaf_d13c, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "(g)") +
  ylab(bquote("δ"^{13}*C~"(‰)")) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 30, vjust = 1.15),
        legend.text = element_text(size = 23),
        plot.tag = element_text(size = 22,
                                face = "bold")) 

## 5.8.- d18O ####

box_d18o <- ggplot(clean_target) + 
  geom_boxplot(aes(x = pair_id, y = leaf_d18o_corrected, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "(h)") +
  ylab(bquote("δ"^{18}*O~"(‰)")) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 20, angle = 60, vjust = -0.09),
        axis.title.x = element_text(size = 30, vjust = 1.15),
        legend.text = element_text(size = 23),
        plot.tag = element_text(size = 22,
                                face = "bold"))

# 6.- Leaf traits plotting ####

tiff("04_figures/98_01_Sites_boxplots.tiff", units = "mm", width = 500, height = 800,
     res = 900, compression = "lzw")
box_h + box_chl + box_bai + box_xc + box_sla + box_d13c + box_n + box_d18o + 
guide_area() + 
  plot_layout(guides = 'collect', ncol = 2)
dev.off()