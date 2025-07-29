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

# 5.- Creating "all" sp_id dataframe ####

clean_target2 <- clean_target %>% 
  mutate(sp_id = "all")

clean_target3 <- rbind(clean_target, clean_target2) %>% 
  mutate(sp_id = fct_relevel(sp_id, "all", "Abialba", "Pinsylv", "Pinpine"))

# Adding a "ghost" factor between "all" and all species to force a larger 
# spacing between the first and the others in the figures

original_levels <- levels(clean_target3$sp_id)
new_levels <- c("all", "", setdiff(original_levels, "all")) # Adding blank space right after "all"
clean_target3$sp_id <- factor(clean_target3$sp_id, levels = new_levels)

# 6.- Whole Plant ####
## 6.1.- BAI80 ####

bai80_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = mean_1980, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  scale_x_discrete(drop = FALSE, 
                   expand = expansion(mult = c(0.2, 0.2))) + 
  labs(tag = "A") +
  ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 6.2.- BAI05 ####

bai05_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = mean_05, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  scale_x_discrete(drop = FALSE, 
                   expand = expansion(mult = c(0.2, 0.2))) + 
  labs(tag = "B") +
  ylab(expression(paste("BAI05 (mm² year"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 6.3.- Rt12 ####

rt12_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = Rt12, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  scale_x_discrete(drop = FALSE, 
                   expand = expansion(mult = c(0.2, 0.2))) + 
  labs(tag = "C") +
  ylab("Rt 2012") + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 6.4.- Rt17 ####

rt17_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = Rt17, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  scale_x_discrete(drop = FALSE, 
                   expand = expansion(mult = c(0.2, 0.2))) + 
  labs(tag = "D") +
  ylab("Rt 2017") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 6.5.- Rt22 ####

rt22_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = Rt22, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  scale_x_discrete(drop = FALSE, 
                   expand = expansion(mult = c(0.2, 0.2))) + 
  labs(tag = "E") +
  ylab("Rt 2022") + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 6.6.- Rs12  ####

rs12_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = Rs12, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  scale_x_discrete(drop = FALSE, 
                   expand = expansion(mult = c(0.2, 0.2))) + 
  labs(tag = "F") +
  ylab("Rs 2012") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 6.7.- Rs17  ####

rs17_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = Rs17, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                    values = c("cold_healthy" = "#2274A5",
                               "hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Non-declining",
                               "D-Healthy",
                               "D-Damaged"),
                    name = "") +
  scale_x_discrete(drop = FALSE, 
                   expand = expansion(mult = c(0.2, 0.2)),
                   labels=c("all" = "All", 
                            "Abialba" = "A. alba",
                            "Pinsylv" = "P. sylv.",
                            "Pinpine" = "P. pinea")) + 
  labs(tag = "G") +
  ylab("Rs 2017") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 7.- Select WP ####
## 7.1.- BAI80 ####
# No need to change it :)

## 7.2.- BAI05 ####

bai05_box_select <- bai05_box + 
  labs(tag = "B")

## 7.3.- Rs12 ####

rs12_box_select <- rs12_box + 
  scale_x_discrete(drop = FALSE, 
                   expand = expansion(mult = c(0.2, 0.2)),
                   labels=c("all" = "All", 
                            "Abialba" = "A. alba",
                            "Pinsylv" = "P. sylv.",
                            "Pinpine" = "P. pinea")) + 
  labs(tag = "C") +
  ylab("Rs 2012") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 8.- Plotting ####

tiff("04_figures/17_03_All_wholeplant3_boxplots.tiff", units = "mm", width = 300, height = 700,
     res = 400, compression = "lzw")
bai80_box / bai05_box / rt12_box / rt17_box / rt22_box / rs12_box / rs17_box
dev.off()

tiff("04_figures/17_03_Select_wholeplant3_boxplots.tiff", units = "mm", width = 300, height = 500,
     res = 400, compression = "lzw")
bai80_box / bai05_box_select / rs12_box_select
dev.off()