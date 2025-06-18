rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading plot data ####

clean_plot <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_01_result_plot.csv", 
                       header = T, sep = ",") %>% 
  mutate(site = substr(plot_id, 1, 3))

# 2.- Selecting variables ####

clean_plot <- clean_plot %>% 
  dplyr::select(c(Ca_ppm_mean, P_ppm_sd, K_ppm_mean, pH_H2O_sd, clay_perc_sd,
                  Ca_ppm_sd, N_perc_sd, loam_perc_sd, sand_perc_sd, P_ppm_mean,
                  CaCO3_perc_mean, N_perc_mean, pH_H2O_mean, clay_perc_mean,
                  sand_perc_mean, loam_perc_mean, spot_status, sp_id)) %>% 
  select(sort(names(.)))

# 3.- Filtering by Pinus sylvestris ####

clean_plot <- clean_plot %>% filter(sp_id == "Pinsylv")

# colnames(clean_plot) <- c("", "Ca content (sd)", 
#                           "CacCO3 content (mean)", 
#                           "Clay proportion (mean)", "Clay proportion (sd)",
#                           "K content (mean)", 
#                           "Loam proportion (mean)", "Loam proportion (sd)",
#                           "N content (mean)", "N content (sd)",
#                           "P content (mean)", "P content (sd)",
#                           "Soil pH (mean)", "Soil pH (sd)",
#                           "Sand proportion (mean)", "Sand proportion (sd)")

# 3.- Boxplots ####

## 3.1.- Ca mean ####

box_ca_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = Ca_ppm_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "A") +
  xlab("") + 
  ylab(expression(paste("Mean Ca content (ppm)"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.2.- Ca sd ####

box_ca_sd <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = Ca_ppm_sd, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "B") +
  xlab("") + 
  ylab(expression(paste("Ca content SD"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.3.- CaCO3 mean ####

box_caco3_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = CaCO3_perc_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "C") +
  xlab("") + 
  ylab(expression(paste("Mean CaCO3 proportion (%)"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 


## 3.4.- Clay mean ####

box_clay_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = clay_perc_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "D") +
  xlab("") + 
  ylab(expression(paste("Mean clay proportion (%)"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.5.- Clay sd ####

box_clay_sd <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = clay_perc_sd, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "E") +
  xlab("") + 
  ylab(expression(paste("Clay proportion SD"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.6.- K mean ####

box_k_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = K_ppm_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "F") +
  xlab("") + 
  ylab(expression(paste("Mean K content (ppm)"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.7.- Loam mean ####

box_loam_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = loam_perc_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "G") +
  xlab("") + 
  ylab(expression(paste("Mean loam proportion (%)"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.8.- Loam sd ####

box_loam_sd <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = loam_perc_sd, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "H") +
  xlab("") + 
  ylab(expression(paste("Loam proportion SD"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.9.- N mean ####

box_n_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = N_perc_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "I") +
  xlab("") + 
  ylab(expression(paste("Mean N content (%)"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.10.- N sd ####

box_n_sd <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = N_perc_sd, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "J") +
  xlab("") + 
  ylab(expression(paste("N content SD"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.11.- P mean ####

box_p_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = P_ppm_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "K") +
  xlab("") + 
  ylab(expression(paste("Mean P content (ppm)"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.12.- P sd ####

box_p_sd <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = P_ppm_sd, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "L") +
  xlab("") + 
  ylab(expression(paste("P content SD"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.13.- pH mean ####

box_ph_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = pH_H2O_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "M") +
  xlab("") + 
  ylab(expression(paste("Mean soil pH"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.14.- pH sd ####

box_ph_sd <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = pH_H2O_sd, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "N") +
  xlab("") + 
  ylab(expression(paste("Soil pH SD"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.15.- Sand mean ####

box_sand_mean <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = sand_perc_mean, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "O") +
  xlab("") + 
  ylab(expression(paste("Mean sand proportion (%)"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 3.16.- Sand sd ####

box_sand_sd <- ggplot(clean_plot) + 
  geom_boxplot(aes(x = sp_id, y = sand_perc_sd, alpha = spot_status), 
               fill = "#1b9e77") + 
  scale_alpha_manual(breaks = c("Hotspot", "Coldspot"),
                     values = c("Hotspot" = 0.5,
                                "Coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "P") +
  xlab("") + 
  ylab(expression(paste("Sand proportion SD"))) +
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

# 4.- Plotting ####

tiff("04_figures/04_04_Soil_boxplots.tiff", units = "mm", width = 360, height = 360,
     res = 800, compression = "lzw")
box_ca_mean + box_ca_sd + box_caco3_mean + box_clay_mean + 
  box_clay_sd + box_k_mean + box_loam_mean + box_loam_sd +
  box_n_mean + box_n_sd + box_p_mean + box_p_sd + 
  box_ph_mean + box_ph_sd + box_sand_mean + box_sand_sd
dev.off()
