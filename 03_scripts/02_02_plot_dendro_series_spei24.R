rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading data ####

climate_data <- read.csv("02_clean_data/02_00_climate_series.csv") %>% dplyr::select(-X)
spei_data <- read.csv("02_clean_data/02_00_spei_series.csv") %>% dplyr::select(-X) %>% 
  filter(year < 2023) %>% filter(month == 7)
dendro_data <- read.csv("02_clean_data/02_02_dendro_series.csv") %>% dplyr::select(-X)

# 2.- Tidying and joining ####

spei_data$site <- substr(spei_data$plot_id, start = 1, stop = 3)
spei_data <- spei_data %>% group_by(site, year) %>% 
  summarise(mean_spei12 = mean(spei12, na.rm = T),
            mean_spei18 = mean(spei18, na.rm = T),
            mean_spei24 = mean(spei24, na.rm = T))

dendro_data$site <- substr(dendro_data$plot_id, start = 1, stop = 3)

dendro_spei <- full_join(dendro_data, spei_data, by = c("site", "year"))

# Now, we standardize BAI by dividing it by d.b.h.:

dendro_spei$bai_tf <- dendro_spei$bai / dendro_spei$dbh

# 3.- Plotting ####

## 3.1.- Abies NAVARRA ####

abies_nav <- dendro_spei %>% 
  filter(site == "BAS" | site ==  "SAR") %>% 
  filter(year > 1949)

mean_abies_nav <- abies_nav %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

abnav_plot <- ggplot(data = abies_nav) + 
  geom_col(aes(x = year, y = 0.5 * mean_spei24), fill = "black", alpha = 0.2)  +
  geom_line(aes(x = year, y = bai_tf, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#9d9ac9",
                                "coldspot" = "#746fb2"),
                     name = "",
                     guide = "none") + 
  scale_fill_manual(values = c("Hotspot" = "red",
                               "Coldspot" = "blue",
                               "hotspot" = "#9d9ac9",
                               "coldspot" = "#746fb2"),
                    name = "",
                    guide = "none") + 
  geom_line(data = mean_abies_nav, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_abies_nav, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "A") +
  ggtitle("Navarra - BAS/SAR") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-100, 175),
                     sec.axis = sec_axis(~.*0.025, 
                                         name = "",
                                         labels = NULL)) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.2.- Abies HUESCA ####

abies_hue <- dendro_spei %>% 
  filter(site == "FAG" | site ==  "OZA") %>% 
  filter(year > 1949)

mean_abies_hue <- abies_hue %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

abhue_plot <- ggplot(data = abies_hue) + 
  geom_col(aes(x = year, y = 0.5 * mean_spei24), fill = "black", alpha = 0.2)  +
  geom_line(aes(x = year, y = bai_tf, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#9d9ac9",
                                "coldspot" = "#746fb2"),
                     name = "",
                     guide = "none") + 
  scale_fill_manual(values = c("Hotspot" = "red",
                               "Coldspot" = "blue",
                               "hotspot" = "#9d9ac9",
                               "coldspot" = "#746fb2"),
                    name = "",
                    guide = "none") + 
  geom_line(data = mean_abies_hue, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_abies_hue, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab("") +
  labs(tag = "B") +
  ggtitle("Huesca - FAG/OZA") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-100, 175),
                     sec.axis = sec_axis(~.*0.025, 
                                         name = "July 24 month-SPEI ")) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        axis.title.y.right = element_text(size = 20),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.3.- Sylv NAVARRA ####

psylv_nav <- dendro_spei %>% 
  filter(site == "URZ" | site == "RON") %>% 
  filter(year > 1949)

mean_psylv_nav <- psylv_nav %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

psnav_plot <- ggplot(data = psylv_nav) + 
  geom_col(aes(x = year, y = 0.5 * mean_spei24), fill = "black", alpha = 0.2)  +
  geom_line(aes(x = year, y = bai_tf, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#5fbb9f",
                                "coldspot" = "#1b9e77"),
                     name = "",
                     guide = "none") + 
  scale_fill_manual(values = c("Hotspot" = "red",
                               "Coldspot" = "blue",
                               "hotspot" = "#5fbb9f",
                               "coldspot" = "#1b9e77"),
                    name = "",
                    guide = "none") + 
  geom_line(data = mean_psylv_nav, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_psylv_nav, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "C") +
  ggtitle("Navarra - RON/URZ") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-100, 175),
                     sec.axis = sec_axis(~.*0.025, 
                                         name = "",
                                         labels = NULL)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.4.- Sylv MADRID ####

psylv_mad <- dendro_spei %>% 
  filter(site == "GUA") %>% 
  filter(year > 1949)

mean_psylv_mad <- psylv_mad %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

psmad_plot <- ggplot(data = psylv_mad) + 
  geom_col(aes(x = year, y = 0.5 * mean_spei24), fill = "black", alpha = 0.2)  +
  geom_line(aes(x = year, y = bai_tf, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#5fbb9f",
                                "coldspot" = "#1b9e77"),
                     name = "",
                     guide = "none") + 
  scale_fill_manual(values = c("Hotspot" = "red",
                               "Coldspot" = "blue",
                               "hotspot" = "#5fbb9f",
                               "coldspot" = "#1b9e77"),
                    name = "",
                    guide = "none") + 
  geom_line(data = mean_psylv_mad, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_psylv_mad, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab("") +
  labs(tag = "D") +
  ggtitle("Madrid - GUA") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-100, 175),
                     sec.axis = sec_axis(~.*0.025, 
                                         name = "July 24 month-SPEI ")) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        axis.title.y.right = element_text(size = 20),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.5.- Sylv GUADALAJARA ####

psylv_gua <- dendro_spei %>% 
  filter(site == "ALU" | site == "ADO" | site == "TRA") %>% 
  filter(year > 1949)

mean_psylv_gua <- psylv_gua %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

psgua_plot <- ggplot(data = psylv_gua) + 
  geom_col(aes(x = year, y = 0.5 * mean_spei24), fill = "black", alpha = 0.2)  +
  geom_line(aes(x = year, y = bai_tf, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#5fbb9f",
                                "coldspot" = "#1b9e77"),
                     name = "",
                     guide = "none") + 
  scale_fill_manual(values = c("Hotspot" = "red",
                               "Coldspot" = "blue",
                               "hotspot" = "#5fbb9f",
                               "coldspot" = "#1b9e77"),
                    name = "",
                    guide = "none") + 
  geom_line(data = mean_psylv_gua, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_psylv_gua, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "E") +
  ggtitle("Guadalajara - ADO/ALU/TRA") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-100, 175),
                     sec.axis = sec_axis(~.*0.025, 
                                         name = "",
                                         labels = NULL)) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.6.- Sylv TERUEL ####

psylv_ter <- dendro_spei %>% 
  filter(site == "COR" | site == "CED") %>% 
  filter(year > 1949)

mean_psylv_ter <- psylv_ter %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

pster_plot <- ggplot(data = psylv_ter) + 
  geom_col(aes(x = year, y = 0.5 * mean_spei24), fill = "black", alpha = 0.2)  +
  geom_line(aes(x = year, y = bai_tf, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#5fbb9f",
                                "coldspot" = "#1b9e77"),
                     name = "",
                     guide = "none") + 
  scale_fill_manual(values = c("Hotspot" = "red",
                               "Coldspot" = "blue",
                               "hotspot" = "#5fbb9f",
                               "coldspot" = "#1b9e77"),
                    name = "",
                    guide = "none") + 
  geom_line(data = mean_psylv_ter, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_psylv_ter, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("Year") + 
  ylab("") +
  labs(tag = "F") +
  ggtitle("Teruel - COR/CED") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-100, 175),
                     sec.axis = sec_axis(~.*0.025, 
                                         name = "July 24 month-SPEI ")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        axis.title.y.right = element_text(size = 20),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.7.- Pinea MADRID ####

ppine_mad <- dendro_spei %>% 
  filter(site == "NAV" | site ==  "PEL") %>% 
  filter(year > 1949)

mean_ppine_mad <- ppine_mad %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

ppmad_plot <- ggplot(data = ppine_mad) + 
  geom_col(aes(x = year, y = 0.5 * mean_spei24), fill = "black", alpha = 0.2)  +
  geom_line(aes(x = year, y = bai_tf, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "#e58f4d",
                                "coldspot" = "#db5f02"),
                     name = "",
                     guide = "none") + 
  scale_fill_manual(values = c("Hotspot" = "red",
                               "Coldspot" = "blue",
                               "hotspot" = "#e58f4d",
                               "coldspot" = "#db5f02"),
                    name = "",
                    guide = "none") + 
  geom_line(data = mean_ppine_mad, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_ppine_mad, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("Year") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "G") +
  ggtitle("Madrid - PEL/NAV") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-100, 175),
                     sec.axis = sec_axis(~.*0.025, 
                                         name = "July 24 month-SPEI")) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

# 4.- Exporting ####

tiff("04_figures/04_02_grouped_dendro_spei24.tiff", units = "mm", width = 700, height = 780,
     res = 700, compression = "lzw")
abnav_plot + abhue_plot + psnav_plot + psmad_plot +
  psgua_plot + pster_plot + ppmad_plot + plot_layout(ncol = 2)
dev.off()