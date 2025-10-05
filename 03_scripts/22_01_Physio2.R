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

# Adding correctness factor for d18O 
o18_correct <- read.csv("02_clean_data/02_01_iso_corrections.csv", sep = ";")

# Taking only oct - apr data

o18_correct <- o18_correct %>% dplyr::select(-c(MAY, JUN, JUL, AUG, SEP)) %>% 
  pivot_longer(cols = JAN:DEC,
               names_to = "month",
               values_to = "o18_correct") %>% 
  rename(plot_id = PLOT) %>% 
  group_by(plot_id) %>% 
  summarise(mean_o18_correct = mean(o18_correct, na.rm = TRUE))

# Join

clean_target3 <- left_join(clean_target3, o18_correct, by = "plot_id")
clean_target3 <- clean_target3 %>% 
  # mutate(corrected_d18o = 1000 * ((leaf_d18o / mean_o18_correct) - 1))
  mutate(corrected_d18o = leaf_d18o / mean_o18_correct)

# 6.- Physio ####
## 6.1.- LWC ####

wc_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = wc_22, fill = spot_status),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") +
  labs(tag = "A") +
  ylab("LWC (%)") + 
  xlab("") + 
  ylim(40, 70) + 
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

## 6.2.- Chl. ####

chl_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = total_chl_fw_22, fill = spot_status),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") +
  labs(tag = "B") +
  ylab(expression(paste("Chl. (μg g"^"-1", ")"))) +
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

## 6.3.- Chl. a/b ####

chlab_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = chla_chlb_22, fill = spot_status),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") +
  labs(tag = "C") +
  ylab("Chl. a/b") + 
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

## 6.4.- Carotenoids ####

xc_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = xc_fw_22, fill = spot_status),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") +
  labs(tag = "D") +
  ylab(expression(paste("Caroten. (μg g"^"-1", ")"))) +
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

## 6.5.- Chl / xc ####

chlxc_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = chl_xc_22, fill = spot_status),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") +
  labs(tag = "E") +
  ylab("Chl. / car.") + 
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

## 6.6.- d13C  ####

d13c_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = leaf_d13c, fill = spot_status),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") +
  labs(tag = "F") +
  ylab(bquote("δ"~C^13~"(‰)")) +
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

## 6.7.- d15N  ####

d15n_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = leaf_d15n, fill = spot_status),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") +
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "G") +
  ylab(bquote("δ"~N^15~"(‰)")) +
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

## 6.8.- d18O  ####

d18o_box <- ggplot(clean_target3) + 
  geom_boxplot(aes(x = sp_id, y = corrected_d18o, fill = spot_status),
               outlier.size = 0.9, outlier.alpha = 0.2) + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") +
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "H") +
  ylab(bquote("δ"~O^18~"(‰)")) +
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

# 7.- Select physio ####
## 7.1.- LWC ####
# No need to change it :)

## 7.2.- Chl. ####

chl_box_select <- chl_box + 
  labs(tag = "B") + 
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

## 7.3.- d13C ####

d13c_box_select <- d13c_box + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "C") +
  ylab(bquote("Leaves δ"~C^13~"(‰)")) +
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

# 8.- Plotting ####

tiff("04_figures/22_01_All_physio2.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
wc_box + chl_box + chlab_box + xc_box + chlxc_box + d13c_box + d15n_box + d18o_box + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

tiff("04_figures/22_02_Select_physio2.tiff", units = "mm", width = 300, height = 300,
     res = 400, compression = "lzw")
wc_box + chl_box_select + d13c_box_select  + 
  guide_area() + plot_layout(ncol = 2, guides = "collect")
dev.off()