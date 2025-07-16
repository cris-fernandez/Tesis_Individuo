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
  mutate(sp_id = factor(sp_id))

clean_target$sp_id <- fct_relevel(clean_target$sp_id, "Abialba", "Pinsylv", "Pinpine")

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target2 <- clean_target %>% 
  mutate(sp_id = "all")

clean_target <- rbind(clean_target, clean_target2)
clean_target$sp_id <- as.factor(clean_target$sp_id)
clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine", "all"),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(mean_def_obs, height, dbh, total_chl_fw_22, xc_fw_22, 
                  chlor_a_22, chlor_b_22, chla_chlb_22,
                  chl_xc_22, percent_c, percent_n, cn_ratio, leaf_d13c, 
                  leaf_d18o, leaf_d15n, wood_d13c_17, wood_d13c_22, sla_22,
                  age, hegyi_index, mean_bai, mean_1980, mean_20, mean_15,
                  mean_10, mean_05, Rt12, Rt17, Rt22, Rs12, Rs17, 
                  tree_number, sp_id, wc_22, spot_status, vigor_id)) %>% 
  dplyr::select(sort(names(.))) %>% 
  filter(sp_id == "all")

# 6.- Plotting ####
## 6.1.- Age ####

box_age <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = age, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "A") +
  ylab("Tree age (years)") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.2.- Hegyi Index ####

box_hegyi <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = hegyi_index, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "B") +
  ylab("Hegyi index") +
  xlab("") + 
  ylim(0, 75) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.3.- BAI 1980 ####

box_bai80 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = mean_1980, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "C") +
  ylab(expression(paste("Tree average growth 80 (mm² year"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 
## 6.4.- Rs12 ####

box_rs12 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = Rs12, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "D") +
  ylab(expression(paste("2012 Resilience"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 




## 6.5.- Defoliation ####

box_defo <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = mean_def_obs, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "A") +
  ylab("Tree mean defoliation (%)") +
  xlab("") + 
  ylim(0, 90) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 
## 6.6.- Water content ####

box_wc <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = wc_22, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "B") +
  ylab("Leaves water content (%)") +
  xlab("") + 
  # ylim(0, 90) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.7.- Total chlorophyll ####

box_chl_fw <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = total_chl_fw_22, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "C") +
  ylab(expression(paste("Leaf chlorophyll content (μg g"^"-1", ")"))) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 


box_n <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = percent_n, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "C") +
  ylab(expression(paste("Leaf N content (%)"))) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.8.- N content ####

box_n <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = percent_n, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "D") +
  ylab(expression(paste("Leaf N content (%)"))) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.9.- d13c ####

box_d13c <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = leaf_d13c, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "E") +
  ylab(bquote("Leaves δ"~C^13~"(‰)")) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.10.- BAI 05 ####

box_bai05 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = mean_05, fill = vigor_id)) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#785EF0",
                               "hot_healthy" = "#FFB000",
                               "hot_damaged" = "#DC267F"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  labs(tag = "F") +
  ylab(expression(paste("Tree average growth 05 (mm² year"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

# 7.- Leaf traits plotting ####

tiff("04_figures/13_01_Predisposing_all_boxplots.tiff", units = "mm", width = 300, height = 300,
     res = 400, compression = "lzw")
box_age + box_hegyi + box_bai80 + box_rs12 + 
  plot_layout(guides = 'collect', ncol = 2) + 
  plot_annotation("All",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          size = 40)))
dev.off()

tiff("04_figures/13_01_Response_all_boxplots.tiff", units = "mm", width = 400, height = 300,
     res = 400, compression = "lzw")
box_defo + box_wc + box_chl_fw + 
  box_n + box_d13c + box_bai05 + 
  plot_layout(guides = 'collect', ncol = 3) + 
  plot_annotation("All",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          size = 40)))
dev.off()