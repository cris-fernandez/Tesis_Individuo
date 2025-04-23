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

climate_data <- read.csv("02_clean_data/02_00_climate_series.csv") %>% select(-X)
dendro_data <- read.csv("02_clean_data/02_02_dendro_series.csv") %>% select(-X)

# 2.- Tidying and joining ####

dendro_data$site <- substr(dendro_data$plot_id, start = 1, stop = 3)

dendro_climate <- full_join(climate_data, dendro_data, by = c("site", "year"))

# Now, we standardize BAI by dividing it by d.b.h.:

dendro_climate <- dendro_climate %>% 
  mutate(bai_tf = bai / mean(bai, na.rm = T)) %>% 
  filter(year > 1949)

# 3.- Plotting ####

## 3.1.- RON  ####

filter1 <- dendro_climate %>% 
  filter(site == "RON" | site ==  "URZ")

mean_filter1 <- filter1 %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

filter1_plot <- ggplot(data = filter1) + 
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
  geom_line(data = mean_filter1, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_filter1, aes(x = year, y = mean_bai,
                                       ymin = mean_bai - se_bai,
                                       ymax = mean_bai + se_bai,
                                       fill = spot_status), alpha = 0.45) + 
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
  ylim(0, 10) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.2.- RON + GUA ####

filter2 <- dendro_climate %>% 
  filter(site == "RON" | site ==  "URZ" | site == "GUA")

mean_filter2 <- filter2 %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

filter2_plot <- ggplot(data = filter2) + 
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
  geom_line(data = mean_filter2, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_filter2, aes(x = year, y = mean_bai,
                                       ymin = mean_bai - se_bai,
                                       ymax = mean_bai + se_bai,
                                       fill = spot_status), alpha = 0.45) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "B") +
  ggtitle("+ Madrid - GUA") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  ylim(0, 10) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.3.- RON + GUA + ADO ####

filter3 <- dendro_climate %>% 
  filter(site == "RON" | site ==  "URZ" | site == "GUA" | site == "ADO" | 
           site == "ALU" | site == "TRA")

mean_filter3 <- filter3 %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))


filter3_plot <- ggplot(data = filter3) + 
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
  geom_line(data = mean_filter3, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_filter3, aes(x = year, y = mean_bai,
                                       ymin = mean_bai - se_bai,
                                       ymax = mean_bai + se_bai,
                                       fill = spot_status), alpha = 0.45) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "C") +
  ggtitle("+ Guadalajara - ADO/ALU/TRA") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  ylim(0, 10) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.4.- RON + GUA + ADO  + COR ####

filter4 <- dendro_climate %>% 
  filter(site == "RON" | site ==  "URZ" | site == "GUA" | site == "ADO" | 
           site == "ALU" | site == "TRA" | site == "COR" | site == "CED")

mean_filter4 <- filter4 %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))


filter4_plot <- ggplot(data = filter4) + 
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
  geom_line(data = mean_filter4, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_filter4, aes(x = year, y = mean_bai,
                                       ymin = mean_bai - se_bai,
                                       ymax = mean_bai + se_bai,
                                       fill = spot_status), alpha = 0.45) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("Year") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "D") +
  ggtitle("+ Teruel - COR/CED") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  ylim(0, 10) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.5.- BAS ####

filter5 <- dendro_climate %>% 
  filter(site == "BAS" | site ==  "SAR")

mean_filter5 <- filter5 %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

filter5_plot <- ggplot(data = filter1) + 
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
  geom_line(data = mean_filter5, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_filter5, aes(x = year, y = mean_bai,
                                       ymin = mean_bai - se_bai,
                                       ymax = mean_bai + se_bai,
                                       fill = spot_status), alpha = 0.45) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "E") +
  ggtitle("Navarra - BAS/SAR") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  ylim(0, 10) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.6.- BAS + FAG ####

filter6 <- dendro_climate %>% 
  filter(site == "BAS" | site ==  "SAR" | site == "FAG" | site ==  "OZA")

mean_filter6 <- filter6 %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

filter6_plot <- ggplot(data = filter6) + 
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
  geom_line(data = mean_filter6, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_filter6, aes(x = year, y = mean_bai,
                                       ymin = mean_bai - se_bai,
                                       ymax = mean_bai + se_bai,
                                       fill = spot_status), alpha = 0.45) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "F") +
  ggtitle("+ Huesca - FAG/OZA") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  ylim(0, 10) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.7.- NAV ####

filter7 <- dendro_climate %>% 
  filter(site == "NAV" | site ==  "PEL")

mean_filter7 <- filter7 %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

filter7_plot <- ggplot(data = filter7) + 
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
  geom_line(data = mean_filter7, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_filter7, aes(x = year, y = mean_bai,
                                       ymin = mean_bai - se_bai,
                                       ymax = mean_bai + se_bai,
                                       fill = spot_status), alpha = 0.45) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "G") +
  ggtitle("Madrid - NAV/PEL") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  ylim(0, 10) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.8.- All ####

filter8 <- dendro_climate

mean_filter8 <- filter8 %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

filter8_plot <- ggplot(data = filter8) + 
  geom_line(aes(x = year, y = bai_tf, col = spot_status, alpha = tree_number),
            size = 0.2) + 
  scale_color_manual(values = c("Hotspot" = "red",
                                "Coldspot" = "blue",
                                "hotspot" = "gray35",
                                "coldspot" = "black"),
                     name = "",
                     guide = "none") + 
  scale_fill_manual(values = c("Hotspot" = "red",
                               "Coldspot" = "blue",
                               "hotspot" = "gray35",
                               "coldspot" = "black"),
                    name = "",
                    guide = "none") + 
  geom_line(data = mean_filter8, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_filter8, aes(x = year, y = mean_bai,
                                       ymin = mean_bai - se_bai,
                                       ymax = mean_bai + se_bai,
                                       fill = spot_status), alpha = 0.45) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("Year") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "H") +
  ggtitle("All") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  ylim(0, 10) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

# 4.- Exporting ####

tiff("04_figures/04_02_grouped_dendro_additive.tiff", units = "mm", width = 700, height = 780,
     res = 700, compression = "lzw")
filter1_plot + filter5_plot + filter2_plot + filter6_plot +
  filter3_plot + filter7_plot + filter4_plot + filter8_plot + plot_layout(ncol = 2)
dev.off()