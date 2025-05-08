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

spei_data <- read.csv("02_clean_data/02_00_spei_series.csv") %>% dplyr::select(-X) %>% 
  filter(year < 2023) %>% filter(month == 7)
dendro_data <- read.csv("02_clean_data/02_02_dendro_series.csv") %>% dplyr::select(-X)

# 2.- Tidying and joining ####

dendro_data$site <- substr(dendro_data$plot_id, start = 1, stop = 3)
sp_data <- dendro_data %>% select(c(sp_id, plot_id)) %>% unique() %>% 
  filter(!is.na(sp_id))

spei_data$site <- substr(spei_data$plot_id, start = 1, stop = 3)
spei_data <- full_join(spei_data, sp_data, by = "plot_id")

spei_data <- spei_data %>% group_by(sp_id, year) %>% 
  summarise(mean_spei12 = mean(spei12, na.rm = T),
            mean_spei18 = mean(spei18, na.rm = T),
            mean_spei24 = mean(spei24, na.rm = T))

dendro_spei <- full_join(dendro_data, spei_data, by = c("sp_id", "year"))

# Now, we standardize BAI by dividing it by d.b.h.:

dendro_spei$bai_tf <- dendro_spei$bai / dendro_spei$dbh

# 3.- Plotting ####

## 3.1.- Abies  ####

abies <- dendro_spei %>% 
  filter(sp_id == "Abialba") %>% 
  filter(year > 1949)

mean_abies <- abies %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

spei12_abies <- spei_data %>% 
  filter(sp_id == "Abialba") %>% 
  dplyr::select(year, mean_spei12) %>% 
  group_by(year) %>% 
  summarise(mean_spei12 = mean(mean_spei12, na.rm = T))
  

abies_plot <- ggplot(data = abies) + 
  geom_col(data = spei12_abies, aes(x = year, y = 20 * mean_spei12), fill = "black", alpha = 0.35)  +
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
  geom_line(data = mean_abies, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_abies, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "A") +
  ggtitle("Abies alba") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-75, 175),
                     sec.axis = sec_axis(~.*0.05, 
                                         name = "",
                                         breaks = seq(-4, 4, 1),
                                         labels = NULL)) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.2.- Sylv MADRID ####

psylv <- dendro_spei %>% 
  filter(sp_id == "Pinsylv") %>% 
  filter(year > 1949)

mean_psylv <- psylv %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

spei12_psylv <- spei_data %>% 
  filter(sp_id == "Pinsylv") %>% 
  dplyr::select(year, mean_spei12) %>% 
  group_by(year) %>% 
  summarise(mean_spei12 = mean(mean_spei12, na.rm = T))

psylv_plot <- ggplot(data = psylv) + 
  geom_col(data = spei12_psylv, aes(x = year, y = 20 * mean_spei12), fill = "black", alpha = 0.35)  +
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
  geom_line(data = mean_psylv, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_psylv, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("") + 
  ylab("") +
  labs(tag = "B") +
  ggtitle("Pinus sylvestris") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-75, 175),
                     sec.axis = sec_axis(~.*0.05, 
                                         breaks = seq(-4, 4, 1),
                                         name = "July 12 month-SPEI ",
                                         labels = c("-4", "", "-2", "", "0",
                                                    "", "2", "", "4"))) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = rel(2),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        axis.title.y.right = element_text(size = 20),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

## 3.3.- Pinea MADRID ####

ppine <- dendro_spei %>% 
  filter(sp_id == "Pinpine") %>% 
  filter(year > 1949)

mean_ppine <- ppine %>% 
  group_by(year, spot_status) %>% 
  summarise(mean_bai = mean(bai_tf, na.rm = T),
            se_bai = sd(bai_tf, na.rm = T) / sqrt(n())) %>% 
  mutate(spot_status = tolower(spot_status))

spei12_ppine <- spei_data %>% 
  filter(sp_id == "Pinpine") %>% 
  dplyr::select(year, mean_spei12) %>% 
  group_by(year) %>% 
  summarise(mean_spei12 = mean(mean_spei12, na.rm = T))

ppine_plot <- ggplot(data = ppine) + 
  geom_col(data = spei12_ppine, aes(x = year, y = 20 * mean_spei12), fill = "black", alpha = 0.35)  +
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
  geom_line(data = mean_ppine, aes(x = year, y = mean_bai, col = spot_status), size = 2) +
  geom_ribbon(data = mean_ppine, aes(x = year, y = mean_bai,
                                         ymin = mean_bai - se_bai, 
                                         ymax = mean_bai + se_bai,
                                         fill = spot_status), alpha = 0.3) + 
  scale_alpha_discrete(range = c(0.2, 0.21),
                       guide = "none") + 
  xlab("Year") + 
  ylab(expression(paste("BAI (mm² year"^"-1", ")"))) +
  labs(tag = "C") +
  ggtitle("Pinus pinea") +
  scale_x_continuous(breaks = seq(1950, 2022, 10), 
                     limits = c(1950, 2022),
                     guide = guide_axis(minor.ticks = TRUE),
                     minor_breaks = seq(1950, 2022, 1)) +
  scale_y_continuous(limits = c(-75, 175),
                     sec.axis = sec_axis(~.*0.05, 
                                         breaks = seq(-4, 4, 1),
                                         name = "July 12 month-SPEI ",
                                         labels = c("-4", "", "-2", "", "0",
                                                    "", "2", "", "4"))) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

# 4.- Defoliation insert ####

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv", 
                         header = T, sep = ",") %>% 
  dplyr::select(c(tree_number, spot_status, sp_id, mean_def_obs)) %>% 
  filter(!is.na(sp_id))
clean_target$sp_id <- factor(clean_target$sp_id, 
                             levels=c("Abialba", "Pinsylv", "Pinpine"), ordered = TRUE)

defoliation_plot <- ggplot(data = clean_target) + 
  geom_boxplot(aes(x = sp_id, y = mean_def_obs, 
                   fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     labels = c("Declining",
                                "Non-declining"),
                     guide = guide_legend(override.aes = list(fill = "gray")),
                     name = "") + 
  xlab("Species") + 
  ylab("Tree defoliation (%)") +
  labs(tag = "D") +
  ylim(0, 90) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.ticks.length.x = rel(2),
        axis.title.y = element_text(size = 20),
        axis.minor.ticks.length.x.bottom = rel(0.7),
        plot.tag = element_text(size = 22),
        plot.title = element_text(size = 22))

# 5.- Exporting ####

tiff("04_figures/04_02_sp_dendro_spei12.tiff", units = "mm", width = 450, height = 250,
     res = 700, compression = "lzw")
abies_plot + psylv_plot + ppine_plot + defoliation_plot + 
  plot_layout(ncol = 2)
dev.off()