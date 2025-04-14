rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd("C:/Users/recup/Desktop/TESIS/PAPERS/01_INDIVIDUO/DATA")
getwd()

# 1.- Reading data ####

climate_data <- read.csv("00_climate_series.csv") %>% select(-X)
dendro_data <- read.csv("00_dendro_series.csv") %>% select(-X)

# 2.- Tidying and joining ####

dendro_data$site <- substr(dendro_data$plot_id, start = 1, stop = 3)

dendro_climate <- full_join(climate_data, dendro_data, by = c("site", "year"))

# 3.- Plotting ####

## 3.1.- Abies NAVARRA ####

abies_nav <- dendro_climate %>% 
  filter(site == "BAS" | site ==  "SAR")

mean_abies_nav <- abies_nav %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T)) %>% 
  mutate(spot_status = tolower(spot_status))

abnav_plot <- ggplot(data = abies_nav) + 
  geom_line(aes(x = year, y = bai, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#9d9ac9",
                                "coldspot" = "#746fb2"),
                     name = "",
                     guide = "none") + 
  geom_line(data = mean_abies_nav, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "A") +
  ggtitle("Navarra") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7))

## 3.2.- Abies HUESCA ####

abies_hue <- dendro_climate %>% 
  filter(site == "FAG" | site ==  "OZA")

mean_abies_hue <- abies_hue %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T)) %>% 
  mutate(spot_status = tolower(spot_status))

abhue_plot <- ggplot(data = abies_hue) + 
  geom_line(aes(x = year, y = bai, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#9d9ac9",
                                "coldspot" = "#746fb2"),
                     name = "",
                     guide = "none") + 
  geom_line(data = mean_abies_hue, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab("") +
  labs(tag = "B") +
  ggtitle("Huesca") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.minor.ticks.length.x.bottom = rel(0.7))

## 3.3.- Pinea MADRID ####

ppine_mad <- dendro_climate %>% 
  filter(site == "NAV" | site ==  "PEL")

mean_ppine_mad <- ppine_mad %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T)) %>% 
  mutate(spot_status = tolower(spot_status))

ppmad_plot <- ggplot(data = ppine_mad) + 
  geom_line(aes(x = year, y = bai, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#e58f4d",
                                "coldspot" = "#db5f02"),
                     name = "",
                     guide = "none") + 
  geom_line(data = mean_ppine_mad, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab("") +
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "C") +
  ggtitle("Madrid") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7))

## 3.4.- Sylv MADRID ####

psylv_mad <- dendro_climate %>% 
  filter(site == "GUA")

mean_psylv_mad <- psylv_mad %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T)) %>% 
  mutate(spot_status = tolower(spot_status))

psmad_plot <- ggplot(data = psylv_mad) + 
  geom_line(aes(x = year, y = bai, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#5fbb9f",
                                "coldspot" = "#1b9e77"),
                     name = "",
                     guide = "none") + 
  geom_line(data = mean_psylv_mad, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab("") +
  labs(tag = "D") +
  ggtitle("Madrid") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.minor.ticks.length.x.bottom = rel(0.7))

## 3.5.- Sylv GUADALAJARA ####

psylv_gua <- dendro_climate %>% 
  filter(site == "ALU" | site == "ADO" | site == "TRA")

mean_psylv_gua <- psylv_gua %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T)) %>% 
  mutate(spot_status = tolower(spot_status))

psgua_plot <- ggplot(data = psylv_gua) + 
  geom_line(aes(x = year, y = bai, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#5fbb9f",
                                "coldspot" = "#1b9e77"),
                     name = "",
                     guide = "none") + 
  geom_line(data = mean_psylv_gua, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "E") +
  ggtitle("Guadalajara") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7))

## 3.6.- Sylv TERUEL ####

psylv_ter <- dendro_climate %>% 
  filter(site == "COR" | site == "CED")

mean_psylv_ter <- psylv_ter %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T)) %>% 
  mutate(spot_status = tolower(spot_status))

pster_plot <- ggplot(data = psylv_ter) + 
  geom_line(aes(x = year, y = bai, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#5fbb9f",
                                "coldspot" = "#1b9e77"),
                     name = "",
                     guide = "none") + 
  geom_line(data = mean_psylv_ter, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("Year") + 
  ylab("") +
  labs(tag = "F") +
  ggtitle("Teruel") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.minor.ticks.length.x.bottom = rel(0.7))

## 3.7.- Sylv NAVARRA ####

psylv_nav <- dendro_climate %>% 
  filter(site == "URZ" | site == "RON")

mean_psylv_nav <- psylv_nav %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai, na.rm = T)) %>% 
  mutate(spot_status = tolower(spot_status))

psnav_plot <- ggplot(data = psylv_nav) + 
  geom_line(aes(x = year, y = bai, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#5fbb9f",
                                "coldspot" = "#1b9e77"),
                     name = "",
                     guide = "none") + 
  geom_line(data = mean_psylv_nav, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("Year") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "G") +
  ggtitle("Navarra") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7))

# 4.- Exporting ####

setwd("C:/Users/recup/Desktop/TESIS/PAPERS/01_INDIVIDUO/FIGURES")

tiff("grouped_dendro.tiff", units = "mm", width = 700, height = 780,
     res = 700, compression = "lzw")
  abnav_plot + abhue_plot + ppmad_plot + psmad_plot +
  psgua_plot + pster_plot + psnav_plot + plot_layout(ncol = 2)
dev.off()