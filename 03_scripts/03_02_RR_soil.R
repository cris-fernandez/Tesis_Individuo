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

# 2.- Calculating the mean values ####

# Mean values are calculated by spot status, as they are needed 
# for the calculation of the response ratio 

rr_soil <- clean_plot %>% 
  group_by(spot_status) %>% 
  summarise(avg_clay_mean = mean(clay_perc_mean, na.rm = T),
            avg_clay_sd = mean(clay_perc_sd, na.rm = T),
            avg_loam_mean = mean(loam_perc_mean, na.rm = T),
            avg_loam_sd = mean(loam_perc_sd, na.rm = T),
            avg_sand_mean = mean(sand_perc_mean, na.rm = T),
            avg_sand_sd = mean(sand_perc_sd, na.rm = T),
            avg_ph_mean = mean(pH_H2O_mean, na.rm = T),
            avg_ph_sd = mean(pH_H2O_sd, na.rm = T),
            avg_caco3_mean = mean(CaCO3_perc_mean, na.rm = T),
            avg_caco3_sd = mean(CaCO3_perc_sd, na.rm = T),
            avg_c_mean = mean(C_org_perc_mean, na.rm = T),
            avg_c_sd = mean(C_org_perc_sd, na.rm = T),
            avg_om_mean = mean(OM_perc_mean, na.rm = T),
            avg_om_sd = mean(OM_perc_sd, na.rm = T),
            avg_n_mean = mean(N_perc_mean, na.rm = T),
            avg_n_sd = mean(N_perc_sd, na.rm = T),
            avg_cn_mean = mean(C_N_ratio_mean, na.rm = T),
            avg_cn_sd = mean(C_N_ratio_sd, na.rm = T),
            avg_p_mean = mean(P_ppm_mean, na.rm = T),
            avg_p_sd = mean(P_ppm_sd, na.rm = T),
            avg_ca_mean = mean(Ca_ppm_mean, na.rm = T),
            avg_ca_sd = mean(Ca_ppm_sd, na.rm = T),
            avg_k_mean = mean(K_ppm_mean, na.rm = T),
            avg_k_sd = mean(K_ppm_sd, na.rm = T),
            avg_mg_mean = mean(Mg_ppm_mean, na.rm = T),
            avg_mg_sd = mean(Mg_ppm_sd, na.rm = T))

# The standard deviations per group for the calculation of SE later:

sd_soil <- clean_plot %>% 
  group_by(spot_status) %>% 
  summarise(se_clay_mean = sd(clay_perc_mean, na.rm = T) / sqrt(n()),
            se_clay_sd = sd(clay_perc_sd, na.rm = T) / sqrt(n()),
            se_clay_mean = sd(loam_perc_mean, na.rm = T) / sqrt(n()),
            se_clay_sd = sd(loam_perc_sd, na.rm = T) / sqrt(n()),
            se_sand_mean = sd(sand_perc_mean, na.rm = T) / sqrt(n()),
            se_sand_sd = sd(sand_perc_sd, na.rm = T) / sqrt(n()),
            se_ph_mean = sd(pH_H2O_mean, na.rm = T) / sqrt(n()),
            se_ph_sd = sd(pH_H2O_sd, na.rm = T) / sqrt(n()),
            se_caco3_mean = sd(CaCO3_perc_mean, na.rm = T) / sqrt(n()),
            se_caco3_sd = sd(CaCO3_perc_sd, na.rm = T) / sqrt(n()),
            se_c_mean = sd(C_org_perc_mean, na.rm = T) / sqrt(n()),
            se_c_sd = sd(C_org_perc_sd, na.rm = T) / sqrt(n()),
            se_om_mean = sd(OM_perc_mean, na.rm = T) / sqrt(n()),
            se_om_sd = sd(OM_perc_sd, na.rm = T) / sqrt(n()),
            se_n_mean = sd(N_perc_mean, na.rm = T) / sqrt(n()),
            se_n_sd = sd(N_perc_sd, na.rm = T) / sqrt(n()),
            se_cn_mean = sd(C_N_ratio_mean, na.rm = T) / sqrt(n()),
            se_cn_sd = sd(C_N_ratio_sd, na.rm = T) / sqrt(n()),
            se_p_mean = sd(P_ppm_mean, na.rm = T) / sqrt(n()),
            se_p_sd = sd(P_ppm_sd, na.rm = T) / sqrt(n()),
            se_ca_mean = sd(Ca_ppm_mean, na.rm = T) / sqrt(n()),
            se_ca_sd = sd(Ca_ppm_sd, na.rm = T) / sqrt(n()),
            se_k_mean = sd(K_ppm_mean, na.rm = T) / sqrt(n()),
            se_k_sd = sd(K_ppm_sd, na.rm = T) / sqrt(n()),
            se_mg_mean = sd(Mg_ppm_mean, na.rm = T) / sqrt(n()),
            se_mg_sd = sd(Mg_ppm_sd, na.rm = T) / sqrt(n()))


# Data wrangling to obtain the desired structure: two columns 
# with every variable in different rows

rownames(rr_soil) <- rr_soil$spot_status
rr_soil <- rr_soil %>% 
  t() %>% 
  as.data.frame()

rownames(sd_soil) <- sd_soil$spot_status
sd_soil <- sd_soil %>% 
  t() %>% 
  as.data.frame

# Removal of the former sp_status column 

rr_soil <- rr_soil[-which(rownames(rr_soil) == "spot_status"), ]
sd_soil <- sd_soil[-which(rownames(sd_soil) == "spot_status"), ]

# 3.- Joining data frames ####

# First, we need to make sure the "var" column matches both dataframes

rr_soil <- rr_soil %>% 
  mutate(var = rownames(rr_soil)) %>% 
  mutate(var = gsub("avg_", "", var))

sd_soil <- sd_soil %>% 
  mutate(var = rownames(sd_soil)) %>% 
  mutate(var = gsub("se_", "", var)) # So var in both dfs are equal

# Now, we can join by var:

rr_df <- full_join(rr_soil, sd_soil, by = "var")
colnames(rr_df) <- c("mean_hotspot", "mean_coldspot", "var", "se_hotspot", "se_coldspot")

# 4.- Calculating the log response ratio ####

# We first need to transform all variables but "var" to numeric

rr_df <- rr_df %>% 
  mutate_at(vars(mean_hotspot, mean_coldspot, se_hotspot, se_coldspot), as.numeric)

rr_df$response_ratio <- abs(log(rr_df$mean_hotspot / rr_df$mean_coldspot))

# 5.- Calculating SE ####

# The SE of a response ratio equales the square root of the sum of squares of 
# the quotient of SE and the mean of each set (hot and coldspot) 

rr_df <- rr_df %>% 
  mutate(se_rr = sqrt((se_hotspot / mean_hotspot)^2 + (se_coldspot / mean_coldspot)^2))

# 6.- Plotting`####

varnames <- c("Ca content (mean)", "P content (sd)", "K content (mean)", 
              "Soil pH (sd)", "Clay proportion (sd)", "Ca content (sd)",
              "N content (sd)","Loam proportion (sd)", "Sand proportion (sd)", 
              "P content (mean)", "CacCO3 content (mean)", "N content (mean)",
              "Soil pH (mean)", "Clay proportion (mean)", "Sand proportion (mean)",
              "Loam proportion (mean)", "C:N ratio (mean)", "C content (mean)",
              "Org. matter content (mean)", "Mg content (mean)", "Mg content (sd)",
              "C:N ratio (sd)", "K content (sd)", "Org. matter content (sd)",
              "C content (sd)", "CacCO3 content (sd)") %>% rev()

rr_plot <- ggplot(rr_df) + 
  geom_point(aes(y = fct_reorder(var, response_ratio), x = response_ratio), 
             size = 2.5) +
  geom_errorbarh(aes(xmax = response_ratio + se_rr, xmin = response_ratio - se_rr, 
                     y = fct_reorder(var, response_ratio)), height = 0, size = 1.1) + 
  geom_vline(xintercept = 0, linetype = "dashed", 
             color = "gray35", size = .15) + 
  scale_y_discrete(labels = varnames) +
  xlab("log(Response ratio)") + 
  ylab("") + 
  theme_classic() + 
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))

tiff("04_figures/04_03_soil_response_ratios.tiff", units = "mm", 
     width = 200, height = 300,
     res = 700, compression = "lzw")
rr_plot
dev.off()
