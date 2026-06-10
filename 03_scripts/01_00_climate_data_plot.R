rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading plots ####

plots <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/02_clean_data/02_01_clean_plot.csv", 
                  header = T, sep = ",")

# Since I am just interested on the coordinates, I will select the needed columns

plots <- plots %>% dplyr::select(c(plot_id, spot_status, sp_name,
                            region)) %>% 
  mutate(sp_id = ifelse(sp_name == "Abies alba", "Abialba",
                        ifelse(sp_name == "Pinus sylvestris", "Pinsylv", "Pinpine")),
         region_id = substr(region, 1, 3),
         pair_id = paste0(region_id, "-", sp_id))

# 2.- Reading climate ####

climate_data_series <- read.csv("02_clean_data/02_00_climate_series.csv") %>% 
  dplyr::select(-X)

# 2.1.- Doing a small chart for the manuscript 

climate_table <- full_join(climate_data_series, plots, by = "plot_id") %>%
  mutate(Tmean = (Tmin + Tmax)/2,
         pair_status = paste0(pair_id, "-", spot_status))

averages <- climate_table %>% group_by(pair_status) %>%
  summarise(Avg_Tmean = mean(Tmean, na.rm = T),
            Avg_Prcp = mean(Prcp, na.rm = T),
            sd_Tmean = sd(Tmean, na.rm = T),
            sd_Prcp = sd(Prcp, na.rm = T),
            min_Tmean = quantile(Tmean, .025, na.rm = T),
            min_Prcp = quantile(Prcp, .025, na.rm = T),
            max_Tmean = quantile(Tmean, .975, na.rm = T),
            max_Prcp = quantile(Prcp, .975, na.rm = T))

# 2.2.- Wilcoxon climate ####

wilcox_temp <- climate_table %>% 
  dplyr::select(c(Tmean, spot_status, pair_id)) %>% 
  group_by(pair_id) %>% 
  summarise(p = wilcox.test(Tmean ~ spot_status)$p.value) %>% 
  mutate(p_bonf = p.adjust(p, method = "bonferroni"))

wilcox_prcp <- climate_table %>% 
  dplyr::select(c(Prcp, spot_status, pair_id)) %>% 
  group_by(pair_id) %>% 
  summarise(p = wilcox.test(Prcp ~ spot_status)$p.value) %>% 
  mutate(p_bonf = p.adjust(p, method = "bonferroni"))
  

# 3.- Joining ####

climate_plots <- full_join(climate_data_series, plots, by = "plot_id") %>% 
  mutate(Tmean = (Tmin + Tmax)/2) %>% 
  group_by(pair_id, year) %>% 
  summarise(avg_Tmax = mean(Tmax, na.rm = T),
            avg_Tmean = mean(Tmean, na.rm = T),
            avg_Tmin = mean(Tmin, na.rm = T),
            avg_Prcp = mean(Prcp, na.rm = T))

# No problem at all, same number of observations than in climate_data_series

climate_plots_long <- climate_plots %>% 
  pivot_longer(cols = avg_Tmax:avg_Prcp,
               names_to = "var_type",
               values_to = "climate")

temperatures <- climate_plots_long %>% 
  filter(!var_type == "avg_Prcp")

precipitation <- climate_plots_long %>% 
  filter(var_type == "avg_Prcp")

# 4.- Temperatures ####
## 4.1.- Huesca Abialba ####

hue_aa <- temperatures %>% filter(pair_id == "Hue-Abialba")
hue_aa_plot <- ggplot(hue_aa) + 
  geom_line(aes(x = year, y = climate, col = var_type), size  = 1.5) +
  scale_color_manual(breaks = c("avg_Tmax", "avg_Tmean", "avg_Tmin"),
                     values = c("avg_Tmax" = "#940909",
                                "avg_Tmean" = "#C540ED",
                                "avg_Tmin" = "#093E5E"),
                     labels = c("Max. temperatures", "Mean temperatures", "Min. temperatures"),
                     name = "") + 
  ylab("Temperature (ºC)") + 
  xlab("") + 
  labs(tag = "(a)",
       title = expression(italic("A. alba") * " - Huesca")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

## 4.2.- Navarra Abialba ####

nav_aa <- temperatures %>% filter(pair_id == "Nav-Abialba")
nav_aa_plot <- ggplot(nav_aa) + 
  geom_line(aes(x = year, y = climate, col = var_type), size  = 1.5) +
  scale_color_manual(breaks = c("avg_Tmax", "avg_Tmean", "avg_Tmin"),
                     values = c("avg_Tmax" = "#940909",
                                "avg_Tmean" = "#C540ED",
                                "avg_Tmin" = "#093E5E"),
                     labels = c("Max. temperatures", "Mean temperatures", "Min. temperatures"),
                     name = "") + 
  ylab("Temperature (ºC)") + 
  xlab("") + 
  labs(tag = "(b)",
       title = expression(italic("A. alba") * " - Navarra")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

## 4.3.- Navarra Pinsylv ####

nav_ps <- temperatures %>% filter(pair_id == "Nav-Pinsylv")
nav_ps_plot <- ggplot(nav_ps) + 
  geom_line(aes(x = year, y = climate, col = var_type), size  = 1.5) +
  scale_color_manual(breaks = c("avg_Tmax", "avg_Tmean", "avg_Tmin"),
                     values = c("avg_Tmax" = "#940909",
                                "avg_Tmean" = "#C540ED",
                                "avg_Tmin" = "#093E5E"),
                     labels = c("Max. temperatures", "Mean temperatures", "Min. temperatures"),
                     name = "") + 
  ylab("Temperature (ºC)") + 
  xlab("") + 
  labs(tag = "(c)",
       title = expression(italic("P. sylvestris") * " - Navarra")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

## 4.4.- Teruel Pinsylv ####

ter_ps <- temperatures %>% filter(pair_id == "Ter-Pinsylv")
ter_ps_plot <- ggplot(ter_ps) + 
  geom_line(aes(x = year, y = climate, col = var_type), size  = 1.5) +
  scale_color_manual(breaks = c("avg_Tmax", "avg_Tmean", "avg_Tmin"),
                     values = c("avg_Tmax" = "#940909",
                                "avg_Tmean" = "#C540ED",
                                "avg_Tmin" = "#093E5E"),
                     labels = c("Max. temperatures", "Mean temperatures", "Min. temperatures"),
                     name = "") + 
  ylab("Temperature (ºC)") + 
  xlab("") + 
  labs(tag = "(d)",
       title = expression(italic("P. sylvestris") * " - Teruel")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

## 4.5.- Guadalajara Pinsylv ####

gua_ps <- temperatures %>% filter(pair_id == "Gua-Pinsylv")
gua_ps_plot <- ggplot(gua_ps) + 
  geom_line(aes(x = year, y = climate, col = var_type), size  = 1.5) +
  scale_color_manual(breaks = c("avg_Tmax", "avg_Tmean", "avg_Tmin"),
                     values = c("avg_Tmax" = "#940909",
                                "avg_Tmean" = "#C540ED",
                                "avg_Tmin" = "#093E5E"),
                     labels = c("Max. temperatures", "Mean temperatures", "Min. temperatures"),
                     name = "") + 
  ylab("") + 
  xlab("") + 
  labs(tag = "(e)",
       title = expression(italic("P. sylvestris") * " - Guadalajara")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

## 4.6.- Madrid Pinsylv ####

mad_ps <- temperatures %>% filter(pair_id == "Mad-Pinsylv")
mad_ps_plot <- ggplot(mad_ps) + 
  geom_line(aes(x = year, y = climate, col = var_type), size  = 1.5) +
  scale_color_manual(breaks = c("avg_Tmax", "avg_Tmean", "avg_Tmin"),
                     values = c("avg_Tmax" = "#940909",
                                "avg_Tmean" = "#C540ED",
                                "avg_Tmin" = "#093E5E"),
                     labels = c("Max. temperatures", "Mean temperatures", "Min. temperatures"),
                     name = "") + 
  ylab("") + 
  xlab("") + 
  labs(tag = "(f)",
       title = expression(italic("P. sylvestris") * " - Madrid")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

## 4.7.- Madrid Pinpine ####

mad_pp <- temperatures %>% filter(pair_id == "Mad-Pinpine")
mad_pp_plot <- ggplot(mad_pp) + 
  geom_line(aes(x = year, y = climate, col = var_type), size  = 1.5) +
  scale_color_manual(breaks = c("avg_Tmax", "avg_Tmean", "avg_Tmin"),
                     values = c("avg_Tmax" = "#940909",
                                "avg_Tmean" = "#C540ED",
                                "avg_Tmin" = "#093E5E"),
                     labels = c("Max. temperatures", "Mean temperatures", "Min. temperatures"),
                     name = "") + 
  ylab("") + 
  xlab("") + 
  labs(tag = "(h)",
       title = expression(italic("P. pinea") * " - Madrid")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 5.- Plotting ####

tiff("04_figures/01_00_Temperature_plots.tiff",
     units = "mm", width = 400, height = 550,
     res = 700, compression = "lzw")

hue_aa_plot + gua_ps_plot + 
  nav_aa_plot + mad_ps_plot + 
  nav_ps_plot + mad_pp_plot + 
  ter_ps_plot + guide_area() + 
  plot_layout(guides = "collect", 
              ncol = 2,
              widths = c(1,1.1))  &
  guides(color = guide_legend(nrow = 3,
                              override.aes = list(size = 3)))
dev.off()

# 6.- Precipitation ####
## 6.1.- Huesca Abialba ####

hue_aa <- precipitation %>% filter(pair_id == "Hue-Abialba")
hue_aa_plot <- ggplot(hue_aa) + 
  geom_line(aes(x = year, y = climate), col = "#093E5E", size  = 1.5) +
  ylab("Annual precipitation (mm)") + 
  xlab("") + 
  labs(tag = "(a)",
       title = expression(italic("A. alba") * " - Huesca")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.position = "none")

## 6.2.- Navarra Abialba ####

nav_aa <- precipitation %>% filter(pair_id == "Nav-Abialba")
nav_aa_plot <- ggplot(nav_aa) + 
  geom_line(aes(x = year, y = climate), col = "#093E5E", size  = 1.5) +
  ylab("Annual precipitation (mm)") + 
  xlab("") + 
  labs(tag = "(b)",
       title = expression(italic("A. alba") * " - Navarra")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.position = "none")

## 6.3.- Navarra Pinsylv ####

nav_ps <- precipitation %>% filter(pair_id == "Nav-Pinsylv")
nav_ps_plot <- ggplot(nav_ps) + 
  geom_line(aes(x = year, y = climate), col = "#093E5E", size  = 1.5) +
  ylab("Annual precipitation (mm)") + 
  xlab("") + 
  labs(tag = "(c)",
       title = expression(italic("P. sylvestris") * " - Navarra")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.position = "none")

## 6.4.- Teruel Pinsylv ####

ter_ps <- precipitation %>% filter(pair_id == "Ter-Pinsylv")
ter_ps_plot <- ggplot(ter_ps) + 
  geom_line(aes(x = year, y = climate), col = "#093E5E", size  = 1.5) +
  ylab("Annual precipitation (mm)") + 
  xlab("") + 
  labs(tag = "(d)",
       title = expression(italic("P. sylvestris") * " - Teruel")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.position = "none")

## 6.5.- Guadalajara Pinsylv ####

gua_ps <- precipitation %>% filter(pair_id == "Gua-Pinsylv")
gua_ps_plot <- ggplot(gua_ps) + 
  geom_line(aes(x = year, y = climate), col = "#093E5E", size  = 1.5) +
  ylab("") + 
  xlab("") + 
  labs(tag = "(e)",
       title = expression(italic("P. sylvestris") * " - Guadalajara")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.position = "none")

## 6.6.- Madrid Pinsylv ####

mad_ps <- precipitation %>% filter(pair_id == "Mad-Pinsylv")
mad_ps_plot <- ggplot(mad_ps) + 
  geom_line(aes(x = year, y = climate), col = "#093E5E", size  = 1.5) +
  ylab("") + 
  xlab("") + 
  labs(tag = "(f)",
       title = expression(italic("P. sylvestris") * " - Madrid")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.position = "none")

## 6.7.- Madrid Pinpine ####

mad_pp <- precipitation %>% filter(pair_id == "Mad-Pinpine")
mad_pp_plot <- ggplot(mad_pp) + 
  geom_line(aes(x = year, y = climate, col = var_type), size  = 1.5) +
  geom_line(aes(x = year, y = climate), col = "#093E5E", size  = 1.5) +
  ylab("") + 
  xlab("") + 
  labs(tag = "(g)",
       title = expression(italic("P. pinea") * " - Madrid")) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        plot.tag = element_text(size = 25,
                                face = "bold"),
        plot.title = element_text(size = 22,
                                  vjust= 1.9),
        legend.text = element_text(size = 35),
        legend.position = "none")

# 7.- Plotting ####

tiff("04_figures/01_00_Precipitation_plots.tiff",
     units = "mm", width = 400, height = 550,
     res = 700, compression = "lzw")
hue_aa_plot + gua_ps_plot + 
  nav_aa_plot + mad_ps_plot + 
  nav_ps_plot + mad_pp_plot + 
  ter_ps_plot + guide_area() + 
  plot_layout(ncol = 2,
              widths = c(1,1.1))
dev.off()
