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

# 5.- Adding SPEI data ####

clean_plot <- read.csv("02_clean_data/02_00_climate_means.csv", 
header = T, sep = ",") %>% dplyr::select(-X)

clean_target <- full_join(clean_target, clean_plot, by = "plot_id")

# 6.- Creating "all" sp_id dataframe ####

clean_target2 <- clean_target %>% 
  mutate(sp_id = "all")

clean_target3 <- rbind(clean_target, clean_target2) %>% 
  mutate(sp_id = fct_relevel(sp_id, "all", "Abialba", "Pinsylv", "Pinpine"))

# Adding a "ghost" factor between "all" and all species to force a larger 
# spacing between the first and the others in the figures

original_levels <- levels(clean_target3$sp_id)
new_levels <- c("all", "", setdiff(original_levels, "all")) # Adding blank space right after "all"
clean_target3$sp_id <- factor(clean_target3$sp_id, levels = new_levels)

# 7.- Plotting ####
## 7.1.- Tmax  ####

tmax_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = Tmax, fill = vigor_id),
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
  ylab("Tmax (ºC)") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 7.2.- Prcp  ####

prcp_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = Prcp, fill = vigor_id),
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
  ylab("Precipitation (mm)") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 7.3.- Hegyi Index  ####

hegyi_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = hegyi_index, fill = vigor_id),
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
  ylab("Hegyi Index") +
  xlab("") + 
  ylim(0, 60) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))
legend.text = element_text(size = 12)

## 7.4.- Age  ####

age_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = age, fill = vigor_id),
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
  ylab("Age (years)") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 7.5.- BAI80  ####

bai_box <- ggplot(clean_target3) + 
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
  labs(tag = "E") +
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
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 7.6.- Rs12  ####

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
  ylab("Rs12") + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "botom",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

tiff("04_figures/13_05_Predisposing_boxplots.tiff", units = "mm", width = 300, height = 650,
     res = 400, compression = "lzw")
tmax_box / prcp_box / hegyi_box / age_box / bai_box / rs12_box 
dev.off()
