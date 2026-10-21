rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "vegan", "stats", "devtools", "lavaan", "tidySEM") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading data ####

# climate_data <- read.csv("02_clean_data/02_00_climate_series.csv") %>% dplyr::select(-X)
dendro_data <- read.csv("02_clean_data/02_02_dendro_series.csv") %>% dplyr::select(-X)
dendro_data$site <- substr(dendro_data$plot_id, start = 1, stop = 3)

# clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3))

# 2.- Plotting theme ####

spot_theme <- list(scale_y_continuous(labels = scales::label_number(accuracy = 0.1)),
                   theme_classic(),
                   theme(legend.position = "right",
                         legend.key.size = unit(2, "cm"),  
                         legend.title = element_blank(),
                         axis.text.x = element_text(size = 20,
                                                    angle = 45,
                                                    hjust = 1),
                         axis.title.x = element_text(size = 22),
                         axis.text.y = element_text(size = 20),
                         axis.title.y = element_text(size = 22),
                         legend.text = element_text(size = 25),
                         plot.tag = element_text(size = 25,
                                                 face = "bold"),
                         plot.title = element_text(size = 22,
                                                   vjust= 1.9)))

# 3.- Plotting ####

## 3.1.- Abies alba - Huesca ####

abies_hue <- dendro_data %>% 
  filter(site == "FAG" | site ==  "OZA")

mean_abies_hue <- abies_hue %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T),
            se_bai = sd(bai, na.rm = T) / sqrt(sum(!is.na(bai))))

abhue_plot <- ggplot(data = abies_hue) + 
  geom_line(aes(x = year, y = bai / 1000, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  geom_line(data = mean_abies_hue, aes(x = year, y = mean_bai / 1000, col = spot_status), size = 1.6) +
  geom_ribbon(data = mean_abies_hue, aes(x = year, y = mean_bai / 1000, 
                                         ymin = (mean_bai - se_bai) / 1000, 
                                         ymax = (mean_bai + se_bai) / 1000,
                                         fill = spot_status),
              alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
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
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", "10"^"-3",")"))) + 
  labs(tag = "(a)",
       title = expression(italic("A. alba") * " - Huesca")) +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     # guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        # axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22)) + 
  spot_theme

## 3.2.- Abies alba - Navarra ####

abies_nav <- dendro_data %>% 
  filter(site == "BAS" | site ==  "SAR")

mean_abies_nav <- abies_nav %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T),
            se_bai = sd(bai, na.rm = T) / sqrt(sum(!is.na(bai))))

abnav_plot <- ggplot(data = abies_nav) + 
  geom_line(aes(x = year, y = bai / 1000, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  geom_line(data = mean_abies_nav, aes(x = year, y = mean_bai / 1000, col = spot_status), size = 1.6) +
  geom_ribbon(data = mean_abies_nav, aes(x = year, y = mean_bai / 1000, 
                                         ymin = (mean_bai - se_bai) / 1000, 
                                         ymax = (mean_bai + se_bai) / 1000,
                                         fill = spot_status),
              alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
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
  xlab("") + 
  ylab("") +
  labs(tag = "(b)",
       title = expression(italic("A. alba") * " - Navarra")) +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     # guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        # axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22)) + 
  spot_theme

## 3.3.- Pinus sylvestris - Navarra ####

psylv_nav <- dendro_data %>% 
  filter(site == "URZ" | site == "RON")

mean_psylv_nav <- psylv_nav %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T),
            se_bai = sd(bai, na.rm = T) / sqrt(sum(!is.na(bai))))

psnav_plot <- ggplot(data = psylv_nav) + 
  geom_line(aes(x = year, y = bai / 1000, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  geom_line(data = mean_psylv_nav, aes(x = year, y = mean_bai / 1000, col = spot_status), size = 1.6) +
  geom_ribbon(data = mean_psylv_nav, aes(x = year, y = mean_bai / 1000, 
                                         ymin = (mean_bai - se_bai) / 1000, 
                                         ymax = (mean_bai + se_bai) / 1000,
                                         fill = spot_status),
              alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
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
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", "10"^"-3",")"))) + 
  labs(tag = "(c)",
       title = expression(italic("P. sylvestris") * " - Navarra")) +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     # guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        # axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22)) + 
  spot_theme

## 3.4.- Pinus sylvestris - Teruel ####

psylv_ter <- dendro_data %>% 
  filter(site == "COR" | site == "CED")

mean_psylv_ter <- psylv_ter %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T),
            se_bai = sd(bai, na.rm = T) / sqrt(sum(!is.na(bai))))

pster_plot <- ggplot(data = psylv_ter) + 
  geom_line(aes(x = year, y = bai / 1000, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  geom_line(data = mean_psylv_ter, aes(x = year, y = mean_bai / 1000, col = spot_status), size = 1.6) +
  geom_ribbon(data = mean_psylv_ter, aes(x = year, y = mean_bai / 1000, 
                                         ymin = (mean_bai - se_bai) / 1000, 
                                         ymax = (mean_bai + se_bai) / 1000,
                                         fill = spot_status),
              alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
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
  xlab("") + 
  ylab("") +
  labs(tag = "(d)",
       title = expression(italic("P. sylvestris") * " - Teruel")) +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     # guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        # axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22)) + 
  spot_theme

## 3.5.- Pinus sylvestris - Guadalajara ####

psylv_gua <- dendro_data %>% 
  filter(site == "ADO" | site == "ALU" | site == "TRA")

mean_psylv_gua <- psylv_gua %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T),
            se_bai = sd(bai, na.rm = T) / sqrt(sum(!is.na(bai))))

psgua_plot <- ggplot(data = psylv_gua) + 
  geom_line(aes(x = year, y = bai / 1000, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  geom_line(data = mean_psylv_gua, aes(x = year, y = mean_bai / 1000, col = spot_status), size = 1.6) +
  geom_ribbon(data = mean_psylv_gua, aes(x = year, y = mean_bai / 1000, 
                                         ymin = (mean_bai - se_bai) / 1000, 
                                         ymax = (mean_bai + se_bai) / 1000,
                                         fill = spot_status),
              alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
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
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", "10"^"-3",")"))) + 
  labs(tag = "(e)",
       title = expression(italic("P. sylvestris") * " - Guadalajara")) +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     # guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        # axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22)) + 
  spot_theme

## 3.6.- Pinus sylvestris - Madrid ####

psylv_mad <- dendro_data %>% 
  filter(site == "GUA")

mean_psylv_mad <- psylv_mad %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T),
            se_bai = sd(bai, na.rm = T) / sqrt(sum(!is.na(bai))))

psmad_plot <- ggplot(data = psylv_mad) + 
  geom_line(aes(x = year, y = bai / 1000, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  geom_line(data = mean_psylv_mad, aes(x = year, y = mean_bai / 1000, col = spot_status), size = 1.6) +
  geom_ribbon(data = mean_psylv_mad, aes(x = year, y = mean_bai / 1000, 
                                         ymin = (mean_bai - se_bai) / 1000, 
                                         ymax = (mean_bai + se_bai) / 1000,
                                         fill = spot_status),
              alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
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
  xlab("Year") + 
  ylab(expression(paste("BAI (mm² year"^"-1", "10"^"-3",")"))) + 
  labs(tag = "(f)",
       title = expression(italic("P. sylvestris") * " - Madrid")) +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     # guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.ticks.length.x = rel(2),
        # axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22)) + 
  spot_theme

## 3.7.- Pinus pinea - Madrid ####

ppine_mad <- dendro_data %>% 
  filter(site == "NAV" | site == "PEL")

mean_ppine_mad <- ppine_mad %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T),
            se_bai = sd(bai, na.rm = T) / sqrt(sum(!is.na(bai))))

ppmad_plot <- ggplot(data = ppine_mad) + 
  geom_line(aes(x = year, y = bai / 1000, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  geom_line(data = mean_ppine_mad, aes(x = year, y = mean_bai / 1000, col = spot_status), size = 1.6) +
  geom_ribbon(data = mean_ppine_mad, aes(x = year, y = mean_bai / 1000, 
                                         ymin = (mean_bai - se_bai) / 1000, 
                                         ymax = (mean_bai + se_bai) / 1000,
                                         fill = spot_status),
              alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
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
  xlab("Year") + 
  ylab(expression(paste("BAI (mm² year"^"-1", "10"^"-3",")"))) + 
  labs(tag = "(g)",
       title = expression(italic("P. pinea") * " - Madrid")) +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     # guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.ticks.length.x = rel(2),
        # axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22)) + 
  spot_theme

# 7.- Plotting ####

tiff("04_figures/96_01_Dendro_R1.tiff", units = "mm", width = 370, height = 500,
     res = 400, compression = "lzw")
abhue_plot + abnav_plot + 
  psnav_plot + pster_plot + 
  psgua_plot + psmad_plot +
  ppmad_plot + guide_area() + 
  plot_layout(ncol = 2, guides = "collect")
dev.off()