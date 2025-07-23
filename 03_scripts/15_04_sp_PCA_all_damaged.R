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
  mutate(sp_id = factor(sp_id))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine"),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

# 5.- Adding climate & SPEI data ####

climate_plot <- read.csv("02_clean_data/02_00_climate_means.csv", 
                         header = T, sep = ",") %>% dplyr::select(-X)
spei_plot <- read.csv("02_clean_data/02_00_spei_series.csv", 
                      header = T, sep = ",") %>% dplyr::select(-X) %>% 
  group_by(plot_id) %>% 
  summarise(mean_spei12 = mean(spei12, na.rm = T),
            mean_spei18 = mean(spei18, na.rm = T),
            mean_spei24 = mean(spei24, na.rm = T))


clean_target <- full_join(clean_target, climate_plot, by = "plot_id")
clean_target <- full_join(clean_target, spei_plot, by = "plot_id")

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(mean_def_obs, height, dbh, total_chl_fw_22, chla_chlb_22,
                  chl_xc_22, percent_c, percent_n, cn_ratio, leaf_d13c, 
                  leaf_d18o, leaf_d15n, sla_22, xc_fw_22,
                  age, hegyi_index, mean_bai, mean_1980, mean_20, mean_15,
                  mean_10, mean_05, Rt12, Rt17, Rt22, Rs12, Rs17, wc_22,
                  tree_number, sp_id, spot_status, Tmax, Prcp, mean_spei12,
                  vigor_id)) %>% 
  dplyr::select(sort(names(.))) %>% 
  filter(vigor_id == "hot_damaged")

# 6.- Data normalization ####

# However, since I do not know whether scale() does exactly the same as 
# the manual standardization, I will test both procedures

clean_target <- na.omit(clean_target)

norm_target <- clean_target %>%
  mutate(defoliation_ST = (mean_def_obs - mean(mean_def_obs, na.rm = T)) / sd(mean_def_obs, na.rm = T),
         height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         dbh_ST = (dbh - mean(dbh, na.rm = T)) / sd(dbh, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         chl_ab_ST = (chla_chlb_22 - mean(chla_chlb_22, na.rm = T)) / sd(chla_chlb_22, na.rm = T),
         chl_xc_ST = (chl_xc_22 - mean(chl_xc_22, na.rm = T)) / sd(chl_xc_22, na.rm = T),
         percent_c_ST = (percent_c - mean(percent_c, na.rm = T)) / sd(percent_c, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         cn_ratio_ST = (cn_ratio - mean(cn_ratio, na.rm = T)) / sd(cn_ratio, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         leaf_d15n_ST = (leaf_d15n - mean(leaf_d15n, na.rm = T)) / sd(leaf_d15n, na.rm = T),
         leaf_d18o_ST = (leaf_d18o - mean(leaf_d18o, na.rm = T)) / sd(leaf_d18o, na.rm = T),
         # wood_d13c_17_ST = (wood_d13c_17 - mean(wood_d13c_17, na.rm = T)) / sd(wood_d13c_17, na.rm = T),
         # wood_d13c_22_ST = (wood_d13c_22 - mean(wood_d13c_22, na.rm = T)) / sd(wood_d13c_22, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         age_ST = (age - mean(age, na.rm = T)) / sd(age, na.rm = T),
         hegyi_index_ST = (hegyi_index - mean(hegyi_index, na.rm = T)) / sd(hegyi_index, na.rm = T),
         bai_ST = (mean_bai - mean(mean_bai, na.rm = T)) / sd(mean_bai, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T),
         bai_20_ST = (mean_20 - mean(mean_20, na.rm = T)) / sd(mean_20, na.rm = T),
         bai_15_ST = (mean_15 - mean(mean_15, na.rm = T)) / sd(mean_15, na.rm = T),
         bai_10_ST = (mean_10 - mean(mean_10, na.rm = T)) / sd(mean_10, na.rm = T),
         bai_05_ST = (mean_05 - mean(mean_05, na.rm = T)) / sd(mean_05, na.rm = T),
         Rt12_ST = (Rt12 - mean(Rt12, na.rm = T)) / sd(Rt12, na.rm = T),
         Rs12_ST = (Rs12 - mean(Rs12, na.rm = T)) / sd(Rs12, na.rm = T),
         Rt17_ST = (Rt17 - mean(Rt17, na.rm = T)) / sd(Rt17, na.rm = T),
         Rs17_ST = (Rs17 - mean(Rs17, na.rm = T)) / sd(Rs17, na.rm = T),
         Rt22_ST = (Rt22 - mean(Rt22, na.rm = T)) / sd(Rt22, na.rm = T),
         wc_ST = (wc_22 - mean(wc_22, na.rm = T)) / sd(wc_22, na.rm = T),
         Tmax_ST = (Tmax - mean(Tmax, na.rm = T)) / sd(Tmax, na.rm = T),
         Prcp_ST = (Prcp - mean(Prcp, na.rm = T)) / sd(Prcp, na.rm = T),
         spei12_ST = (mean_spei12 - mean(mean_spei12, na.rm = T)) / sd(mean_spei12, na.rm = T))

norm_target <- norm_target %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

# 7.- Correlations matrix ####

# The chart is needed for the PCA:

correlogram <- cor(norm_target)
ggcorrplot(correlogram)

# 8.- PCA analysis ####

pca_results <- princomp(norm_target)
summary(pca_results)

# The first two components explain only 39.73% of the data variance!

pca_results$loadings[, 1:2]

pca_df <- cbind(norm_target, pca_results$scores) # Scores for the points
pca_df$sp_id <- clean_target$sp_id
pca_df$spot_status <- clean_target$spot_status
pca_df$vigor_id <- clean_target$vigor_id

# 9.- Biplot ####

# Just to compare whether we are making the biplot correctly or not

fviz_pca_var(pca_results, col.var = "black")


# 10.- Biplot - manually ####

## 10.1.- Loadings dataframe

loadings_df <- as.data.frame(pca_results$loadings[, 1:2]) # They are not merged
# into pca_df as they have different row numbers

loadings_df$variable <- rownames(loadings_df) # So we know what variable is which

# Adding a column with the proper names of the variables to appear on the PCA:

loadings_df$varnames <- c("Defoliation", "Height", "dbh", 
                          "Chl.", "Carotenoids", "Chl. a/b", "Chl. / xc", 
                          "C", "N", "C:N", "δ13C", "δ15N", "δ18O", "SLA", 
                          "age", "Hegyi", "BAI", "BAI80", "BAI20", "BAI15",
                          "BAI10", "BAI05", "Rt12", "Rs12", "Rt17", "Rs17",
                          "Rt22", "LWC", "Tmax", "Prcp", "SPEI12")

## 10.2.- Scale factor ####

# Scale factor is just a constant number used to multiply the length of the vectors 
# thus allowing us to see them more clearly

scale_factor <- 20 

## 10.3.- Multiplying ####

loadings_df <- loadings_df %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)

## 10.4.- Density raster ####

# Instead of using the argument stat_density_2d, we will create a raster, as 
# it allows us to better customize the looks and looks better <3

# This is performed with kde2d, which does a Two-Dimensional Kernel Density Estimation

dens <- kde2d(x = pca_df$Comp.1,
              y = pca_df$Comp.2,
              n = 200)  # resolution

dens_df <- as.data.frame(expand.grid(x = dens$x, y = dens$y))
dens_df$z <- as.vector(dens$z)

## 10.5.- Density 50% ####

# To add a dashed contour line marking the space where the 50% of the trees 
# might appear, we just need to calculate the density at the 50th percentile

z_sorted <- sort(as.vector(dens$z))
cdf <- cumsum(z_sorted) / sum(z_sorted)

level_50 <- z_sorted[which.min(abs(cdf - 0.5))]

# Assigning each density value to each vigor_id factor:

sp_levels <- levels(pca_df$sp_id)

dens_list <- lapply(sp_levels, function(v) {
  data_sub <- pca_df %>% filter(sp_id == v)
  dens <- kde2d(x = data_sub$Comp.1, y = data_sub$Comp.2, n = 200)
  
  dens_df <- as.data.frame(expand.grid(x = dens$x, y = dens$y))
  dens_df$z <- as.vector(dens$z)
  dens_df$sp_id <- v
  
  # Calcular nivel 50% para este grupo
  z_sorted <- sort(as.vector(dens$z))
  cdf <- cumsum(z_sorted) / sum(z_sorted)
  level_50 <- z_sorted[which.min(abs(cdf - 0.5))]
  
  list(data = dens_df, level_50 = level_50)
})


# Crear un dataframe con los niveles de contorno 50% por grupo
level_50_df <- data.frame(
  sp_id = sp_levels,
  level_50 = sapply(dens_list, function(x) x$level_50)
)

#MASCHATGPT
sp_colors <- c("Abialba" = "#785EF0",
               "Pinsylv" = "#FFB000",
               "Pinpine" = "#990000")

# Aplicar colores con alpha según z
dens_df_all <- do.call(rbind, lapply(dens_list, function(d) d$data))

# Normalizar z por grupo y aplicar alpha
dens_df_all <- dens_df_all %>%
  group_by(sp_id) %>%
  mutate(z_scaled = (z - min(z)) / (max(z) - min(z)),
         fill = scales::alpha(sp_colors[sp_id], z_scaled * 1)) %>%
  ungroup()

## 10.6.- Biplot, all ####

biplot_all <- ggplot() +
  geom_tile(data = dens_df_all, aes(x = x, y = y, fill = z), alpha = 0.5) +
  scale_fill_gradientn(colours = viridis::viridis(10)) +
  geom_contour(data = dens_df_all,
               aes(x = x, y = y, z = z, color = sp_id),
               breaks = level_50,
               size = 0.7,
               linetype = "dashed") + 
  geom_point(data = pca_df, aes(x = Comp.1, y = Comp.2, color = sp_id), 
             alpha = 0.85) +
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (26.66 %)") + 
  ylab("PC2 (16.60 %)") + 
  labs(tag = "A") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

## 10.7.- Plotting ####

tiff("04_figures/15_04_pca_all_damaged.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
biplot_all 
dev.off()