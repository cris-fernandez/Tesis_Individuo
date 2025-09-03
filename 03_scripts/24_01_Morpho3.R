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

# Filtering only hotspot observations:
clean_target3 <- clean_target3 %>% filter(!vigor_id == "cold_healthy") 

# 6.- Morpho ####
## 6.1.- Height ####

h_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = height, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"),
                    values = c("hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Healthy tree",
                               "Damaged tree"),
                    name = "") + 
  labs(tag = "A") +
  ylab("Height (m)") + 
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

## 6.2.- DBH ####

dbh_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = dbh, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"),
                    values = c("hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Healthy tree",
                               "Damaged tree"),
                    name = "") +
  labs(tag = "B") +
  ylab("d.b.h. (cm)") + 
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

## 6.3.- C ####

c_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = percent_c, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"),
                    values = c("hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Healthy tree",
                               "Damaged tree"),
                    name = "") +
  labs(tag = "C") +
  ylab("C content (%)") + 
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

## 6.4.- N ####

n_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = percent_n, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"),
                    values = c("hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Healthy tree",
                               "Damaged tree"),
                    name = "") +
  labs(tag = "D") +
  ylab("N content (%)") + 
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

## 6.5.- C:N ####

cn_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = percent_c / percent_n, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"),
                    values = c("hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Healthy tree",
                               "Damaged tree"),
                    name = "") +
  labs(tag = "E") +
  ylab("C:N ratio") + 
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

## 6.6.- SLA  ####

sla_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = sla_22, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"),
                    values = c("hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Healthy tree",
                               "Damaged tree"),
                    name = "") +
  labs(tag = "F") +
  ylab(expression(paste("SLA (cm² g"^"-1", ")"))) + 
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

## 6.7.- Age ####

age_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = age, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"),
                    values = c("hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Healthy tree",
                               "Damaged tree"),
                    name = "") +
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "G") +
  ylab("Age (years)") + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

## 6.8.- Hegyi Index  ####

hegyi_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = hegyi_index, fill = vigor_id),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"),
                    values = c("hot_healthy" = "#D71515",
                               "hot_damaged" = "#650304"),
                    labels = c("Healthy tree",
                               "Damaged tree"),
                    name = "") +
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "H") +
  ylab("Hegyi Index") + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 7.- Select morpho ####
## 7.1.- Height ####
# No need to change it :)

## 7.2.- C ####

c_box_select <- c_box + 
  labs(tag = "B")

## 7.4.- Age ####

age_box_select <- age_box + 
  labs(tag = "C")

## 7.5.- Hegyi Index ####

hegyi_box_select <- hegyi_box + 
  labs(tag = "D") + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  theme_classic() +
  theme(legend.position = "right",
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

tiff("04_figures/24_01_All_morpho3.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
h_box + dbh_box + c_box + n_box + cn_box + sla_box + age_box + hegyi_box + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

tiff("04_figures/24_02_Select_morpho3.tiff", units = "mm", width = 400, height = 300,
     res = 400, compression = "lzw")
h_box + c_box_select + age_box_select + hegyi_box_select + 
  guide_area() + plot_layout(ncol = 2, guides = "collect")
dev.off()