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
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"),
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot"))


# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(height, dbh, total_chl_fw_22, chla_chlb_22,
                  chl_xc_22, percent_c, percent_n, cn_ratio, leaf_d13c, 
                  leaf_d18o, leaf_d15n, sla_22, xc_fw_22,
                  age, hegyi_index, mean_1980, mean_05, Rt12, Rt17, 
                  Rt22, Rs12, Rs17, wc_22,
                  tree_number, sp_id, spot_status, vigor_id)) %>% 
  dplyr::select(sort(names(.)))

clean_target <- clean_target[complete.cases(clean_target), ]

summary(clean_target)

# 6.- Filtering per species ####

aa_target <- clean_target %>% filter(sp_id == "Abialba")
ps_target <- clean_target %>% filter(sp_id == "Pinsylv")
pp_target <- clean_target %>% filter(sp_id == "Pinpine")

# 7.- Normalization ####

norm_aa_target <- aa_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T),
         bai_05_ST = (mean_05 - mean(mean_05, na.rm = T)) / sd(mean_05, na.rm = T),
         Rs12_ST = (Rs12 - mean(Rs12, na.rm = T)) / sd(Rs12, na.rm = T),
         wc_ST = (wc_22 - mean(wc_22, na.rm = T)) / sd(wc_22, na.rm = T))

vigor_aa <- norm_aa_target$vigor_id
norm_aa_target <- norm_aa_target %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

norm_ps_target <- ps_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T),
         bai_05_ST = (mean_05 - mean(mean_05, na.rm = T)) / sd(mean_05, na.rm = T),
         Rs12_ST = (Rs12 - mean(Rs12, na.rm = T)) / sd(Rs12, na.rm = T),
         wc_ST = (wc_22 - mean(wc_22, na.rm = T)) / sd(wc_22, na.rm = T))

vigor_ps <- norm_ps_target$vigor_id
norm_ps_target <- norm_ps_target %>% dplyr::select(c(contains("_ST"))) %>% 
  dplyr::select(-spot_status)

norm_pp_target <- pp_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T),
         bai_05_ST = (mean_05 - mean(mean_05, na.rm = T)) / sd(mean_05, na.rm = T),
         Rs12_ST = (Rs12 - mean(Rs12, na.rm = T)) / sd(Rs12, na.rm = T),
         wc_ST = (wc_22 - mean(wc_22, na.rm = T)) / sd(wc_22, na.rm = T))

vigor_pp <- norm_pp_target$vigor_id
norm_pp_target <- norm_pp_target %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

# 8.- Correlations matrix ####

# The chart is needed for the PCA:

correlogram_aa <- cor(norm_aa_target)
correlogram_ps <- cor(norm_ps_target)
correlogram_pp <- cor(norm_pp_target)

# 9.- PCA analysis ####

pca_results_aa <- princomp(norm_aa_target)
summary(pca_results_aa)

pca_results_ps <- princomp(norm_ps_target)
summary(pca_results_ps)

pca_results_pp <- princomp(norm_pp_target)
summary(pca_results_pp)

# Selecting the first two components

pca_results_aa$loadings[, 1:2]
pca_results_ps$loadings[, 1:2]
pca_results_pp$loadings[, 1:2]

pca_df_aa <- cbind(norm_aa_target, pca_results_aa$scores) # Scores for the points
pca_df_aa$sp_id <- norm_aa_target$sp_id
pca_df_aa$vigor_id <- norm_aa_target$vigor_id

pca_df_ps <- cbind(norm_ps_target, pca_results_ps$scores) # Scores for the points
pca_df_ps$sp_id <- norm_ps_target$sp_id
pca_df_ps$vigor_id <- norm_ps_target$vigor_id

pca_df_pp <- cbind(norm_pp_target, pca_results_pp$scores) # Scores for the points
pca_df_pp$sp_id <- norm_pp_target$sp_id
pca_df_pp$vigor_id <- norm_pp_target$vigor_id

# 10.- Biplots ####

## 10.1.- Loadings dataframe ####

loadings_df_aa <- as.data.frame(pca_results_aa$loadings[, 1:2])
loadings_df_ps <- as.data.frame(pca_results_ps$loadings[, 1:2])
loadings_df_pp <- as.data.frame(pca_results_pp$loadings[, 1:2])
# They are not merged into pca_df as they have different row numbers

loadings_df_aa$variable <- rownames(loadings_df_aa) # So we know what variable is which
loadings_df_ps$variable <- rownames(loadings_df_ps)
loadings_df_pp$variable <- rownames(loadings_df_pp)

# Adding a column with the proper names of the variables to appear on the PCA:

varnames <- c("Height", "Chl.", "N", "δ13C", "SLA", 
              "BAI80", "BAI05", "Rs12", "LWC")

loadings_df_aa$varnames <- varnames
loadings_df_ps$varnames <- varnames
loadings_df_pp$varnames <- varnames

## 10.2.- Scale factor ####

# Scale factor is just a constant number used to multiply the length of the vectors 
# thus allowing us to see them more clearly

scale_factor <- 20 

## 10.3.- Multiplying ####

loadings_df_aa <- loadings_df_aa %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)
loadings_df_ps <- loadings_df_ps %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)
loadings_df_pp <- loadings_df_pp %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)


# 11.- Biplots by vigor ####
## 11.1.- Density raster ####

# Instead of using the argument stat_density_2d, we will create a raster, as 
# it allows us to better customize the looks and looks better <3

# This is performed with kde2d, which does a Two-Dimensional Kernel Density Estimation

dens_aa <- kde2d(x = pca_df_aa$Comp.1,
                 y = pca_df_aa$Comp.2,
                 n = 200)  # resolution
dens_ps <- kde2d(x = pca_df_ps$Comp.1,
                 y = pca_df_ps$Comp.2,
                 n = 200) 
dens_pp <- kde2d(x = pca_df_pp$Comp.1,
                 y = pca_df_pp$Comp.2,
                 n = 200) 

dens_df_aa <- as.data.frame(expand.grid(x = dens_aa$x, y = dens_aa$y))
dens_df_aa$z <- as.vector(dens_aa$z)

dens_df_ps <- as.data.frame(expand.grid(x = dens_ps$x, y = dens_ps$y))
dens_df_ps$z <- as.vector(dens_ps$z)

dens_df_pp <- as.data.frame(expand.grid(x = dens_pp$x, y = dens_pp$y))
dens_df_pp$z <- as.vector(dens_pp$z)

## 11.2.- Density 50% ####

# To add a dashed contour line marking the space where the 50% of the trees 
# might appear, we just need to calculate the density at the 50th percentile

z_sorted_aa <- sort(as.vector(dens_aa$z))
cdf_aa <- cumsum(z_sorted_aa) / sum(z_sorted_aa)
level_50_aa <- z_sorted_aa[which.min(abs(cdf_aa - 0.1))] # Is 0.1 for the 90%? Check later

z_sorted_ps <- sort(as.vector(dens_ps$z))
cdf_ps <- cumsum(z_sorted_ps) / sum(z_sorted_ps)
level_50_ps <- z_sorted_ps[which.min(abs(cdf_ps - 0.1))]

z_sorted_pp <- sort(as.vector(dens_pp$z))
cdf_pp <- cumsum(z_sorted_pp) / sum(z_sorted_pp)
level_50_pp <- z_sorted_pp[which.min(abs(cdf_pp - 0.1))]

# Assigning each density value to each vigor_id factor:

vigor_levels <- levels(clean_target$vigor_id) #It is null when used in the 3 dataframes 
# per species because vigor_id was not selected in those!

pca_df_aa$vigor_id <- vigor_aa # Reincorporating vigor_id into the all-numeric df
pca_df_ps$vigor_id <- vigor_ps
pca_df_pp$vigor_id <- vigor_pp

dens_list_aa <- lapply(vigor_levels, function(v) {
  data_sub_aa <- pca_df_aa %>% filter(vigor_id == v)
  dens_aa <- kde2d(x = data_sub_aa$Comp.1, y = data_sub_aa$Comp.2, n = 200)
  
  dens_df_aa <- as.data.frame(expand.grid(x = dens_aa$x, y = dens_aa$y))
  dens_df_aa$z <- as.vector(dens_aa$z)
  dens_df_aa$vigor_id <- v
  
  # Calcular nivel 50% para este grupo
  z_sorted_aa <- sort(as.vector(dens_aa$z))
  cdf_aa <- cumsum(z_sorted_aa) / sum(z_sorted_aa)
  level_50_aa <- z_sorted_aa[which.min(abs(cdf_aa - 0.1))]
  
  list(data = dens_df_aa, level_50 = level_50_aa)
})

dens_list_ps <- lapply(vigor_levels, function(v) {
  data_sub_ps <- pca_df_ps %>% filter(vigor_id == v)
  dens_ps <- kde2d(x = data_sub_ps$Comp.1, y = data_sub_ps$Comp.2, n = 200)
  
  dens_df_ps <- as.data.frame(expand.grid(x = dens_ps$x, y = dens_ps$y))
  dens_df_ps$z <- as.vector(dens_ps$z)
  dens_df_ps$vigor_id <- v
  
  # Calcular nivel 50% para este grupo
  z_sorted_ps <- sort(as.vector(dens_ps$z))
  cdf_ps <- cumsum(z_sorted_ps) / sum(z_sorted_ps)
  level_50_ps <- z_sorted_ps[which.min(abs(cdf_ps - 0.1))]
  
  list(data = dens_df_ps, level_50 = level_50_ps)
})

dens_list_pp <- lapply(vigor_levels, function(v) {
  data_sub_pp <- pca_df_pp %>% filter(vigor_id == v)
  dens_pp <- kde2d(x = data_sub_pp$Comp.1, y = data_sub_pp$Comp.2, n = 200)
  
  dens_df_pp <- as.data.frame(expand.grid(x = dens_pp$x, y = dens_pp$y))
  dens_df_pp$z <- as.vector(dens_pp$z)
  dens_df_pp$vigor_id <- v
  
  # Calcular nivel 50% para este grupo
  z_sorted_pp <- sort(as.vector(dens_pp$z))
  cdf_pp <- cumsum(z_sorted_pp) / sum(z_sorted_pp)
  level_50_pp <- z_sorted_pp[which.min(abs(cdf_pp - 0.1))]
  
  list(data = dens_df_pp, level_50 = level_50_pp)
})
# Dataframes with the 50% contour per group: 

level_50_df_aa <- data.frame(
  vigor_id = vigor_levels,
  level_50 = sapply(dens_list_aa, function(x) x$level_50)) # This retrieves the 
# 50% value per vigor category of the list

level_50_df_ps <- data.frame(
  vigor_id = vigor_levels,
  level_50 = sapply(dens_list_ps, function(x) x$level_50))

level_50_df_pp <- data.frame(
  vigor_id = vigor_levels,
  level_50 = sapply(dens_list_pp, function(x) x$level_50))

# Creating colour scale: 

sp_colors <- c("cold_healthy" = "#2274A5",
               "hot_healthy" = "#D71515",
               "hot_damaged" = "#650304")

# Applying colours with alpha by z value:
dens_df_all_aa <- do.call(rbind, lapply(dens_list_aa, function(d) d$data))
dens_df_all_ps <- do.call(rbind, lapply(dens_list_ps, function(d) d$data))
dens_df_all_pp <- do.call(rbind, lapply(dens_list_pp, function(d) d$data))

# Normalising z per group and applying the colour:
dens_df_all_aa <- dens_df_all_aa %>%
  group_by(vigor_id) %>%
  mutate(z_scaled = (z - min(z)) / (max(z) - min(z)),
         fill = scales::alpha(sp_colors[vigor_id], z_scaled * 1)) %>%
  ungroup()

dens_df_all_ps <- dens_df_all_ps %>%
  group_by(vigor_id) %>%
  mutate(z_scaled = (z - min(z)) / (max(z) - min(z)),
         fill = scales::alpha(sp_colors[vigor_id], z_scaled * 1)) %>%
  ungroup()

dens_df_all_pp <- dens_df_all_pp %>%
  group_by(vigor_id) %>%
  mutate(z_scaled = (z - min(z)) / (max(z) - min(z)),
         fill = scales::alpha(sp_colors[vigor_id], z_scaled * 1)) %>%
  ungroup()

# 12.- Plotting ####

## 12.1.- Abies alba ####

biplot_aa <- ggplot() +
  geom_tile(data = dens_df_all_aa, aes(x = x, y = y, fill = z), alpha = 0.5) +
  scale_fill_gradientn(colours = viridis::viridis(10)) +
  geom_contour(data = dens_df_all_aa,
               aes(x = x, y = y, z = z, color = vigor_id),
               breaks = level_50_aa,
               size = 0.7,
               linetype = "dashed") + 
  geom_point(data = pca_df_aa, aes(x = Comp.1, y = Comp.2, color = vigor_id), 
             alpha = 0.85) +
  scale_color_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                     values = c("cold_healthy" = "#2274A5",
                                "hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Non-declining", 
                                "D-Healthy",
                                "D-Damaged"),
                     name = "") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_aa,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (20.82 %)") + 
  ylab("PC2 (18.20 %)") + 
  labs(tag = "A") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_aa,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

## 12.2.- Pinus sylvestris ####

biplot_ps <- ggplot() +
  geom_tile(data = dens_df_all_ps, aes(x = x, y = y, fill = z), alpha = 0.5) +
  scale_fill_gradientn(colours = viridis::viridis(10)) +
  geom_contour(data = dens_df_all_ps,
               aes(x = x, y = y, z = z, color = vigor_id),
               breaks = level_50_ps,
               size = 0.7,
               linetype = "dashed") + 
  geom_point(data = pca_df_ps, aes(x = Comp.1, y = Comp.2, color = vigor_id), 
             alpha = 0.85) +
  scale_color_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                     values = c("cold_healthy" = "#2274A5",
                                "hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Non-declining", 
                                "D-Healthy",
                                "D-Damaged"),
                     name = "") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_ps,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (20.82 %)") + 
  ylab("PC2 (18.20 %)") + 
  labs(tag = "B") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_ps,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

## 12.3.- Pinus pinea ####

biplot_pp <- ggplot() +
  geom_tile(data = dens_df_all_pp, aes(x = x, y = y, fill = z), alpha = 0.5) +
  scale_fill_gradientn(colours = viridis::viridis(10)) +
  geom_contour(data = dens_df_all_pp,
               aes(x = x, y = y, z = z, color = vigor_id),
               breaks = level_50_pp,
               size = 0.7,
               linetype = "dashed") + 
  geom_point(data = pca_df_pp, aes(x = Comp.1, y = Comp.2, color = vigor_id), 
             alpha = 0.85) +
  scale_color_manual(breaks = c("cold_healthy", "hot_healthy", "hot_damaged"),
                     values = c("cold_healthy" = "#2274A5",
                                "hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Non-declining", 
                                "D-Healthy",
                                "D-Damaged"),
                     name = "") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_pp,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (20.82 %)") + 
  ylab("PC2 (18.20 %)") + 
  labs(tag = "C") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_pp,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

# 13.- Saving ####

tiff("04_figures/35_04_PCA_Select_3sp.tiff", units = "mm",
     width = 900, height = 300,
     res = 600, compression = "lzw")
biplot_aa + biplot_ps + biplot_pp
dev.off()