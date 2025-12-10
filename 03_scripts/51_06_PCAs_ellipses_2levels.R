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
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot")) %>% 
  filter(mean_def_obs < 100)


# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(height, total_chl_fw_22, percent_n, leaf_d13c, leaf_d18o_corrected,
                  sla_22, xc_fw_22,mean_1980, mean_def_obs, tree_number, sp_id, spot_status, vigor_id)) %>% 
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
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         leaf_d18o_ST = (leaf_d18o_corrected - mean(leaf_d18o_corrected, na.rm = T)) / sd(leaf_d18o_corrected, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_aa <- norm_aa_target$vigor_id
spot_aa <- norm_aa_target$spot_status
norm_aa_target <- norm_aa_target %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

norm_ps_target <- ps_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         leaf_d18o_ST = (leaf_d18o_corrected - mean(leaf_d18o_corrected, na.rm = T)) / sd(leaf_d18o_corrected, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_ps <- norm_ps_target$vigor_id
spot_ps <- norm_ps_target$spot_status
norm_ps_target <- norm_ps_target %>% dplyr::select(c(contains("_ST"))) %>% 
  dplyr::select(-spot_status)

norm_pp_target <- pp_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         xc_ST = (xc_fw_22 - mean(xc_fw_22, na.rm = T)) / sd(xc_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         leaf_d18o_ST = (leaf_d18o_corrected - mean(leaf_d18o_corrected, na.rm = T)) / sd(leaf_d18o_corrected, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T))

vigor_pp <- norm_pp_target$vigor_id
spot_pp <- norm_pp_target$spot_status
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
pca_df_ps <- cbind(norm_ps_target, pca_results_ps$scores) # Scores for the points
pca_df_pp <- cbind(norm_pp_target, pca_results_pp$scores) # Scores for the points

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

varnames_loadings <- c("Height", "Chl.", "Car.", "N", "δ13C", "δ18O", "SLA", "BAI80")

loadings_df_aa$varnames <- varnames_loadings
loadings_df_ps$varnames <- varnames_loadings
loadings_df_pp$varnames <- varnames_loadings

## 10.2.- Scale factor ####

# Scale factor is just a constant number used to multiply the length of the vectors 
# thus allowing us to see them more clearly

scale_factor <- 13 

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
## 11.1.- Assigning defoliation ####

vigor_levels <- levels(clean_target$vigor_id) #It is null when used in the 3 dataframes 
# per species because vigor_id was not selected in those!

pca_df_aa$vigor_id <- vigor_aa # Reincorporating vigor_id into the all-numeric df
pca_df_ps$vigor_id <- vigor_ps
pca_df_pp$vigor_id <- vigor_pp

pca_df_aa$spot_status <- spot_aa # Reincorporating spot_status into the all-numeric df
pca_df_ps$spot_status <- spot_ps
pca_df_pp$spot_status <- spot_pp

pca_df_aa$mean_def_obs <- aa_target$mean_def_obs
pca_df_ps$mean_def_obs <- ps_target$mean_def_obs
pca_df_pp$mean_def_obs <- pp_target$mean_def_obs

# 12.- Plotting - plot level ####
## 12.1.- Abies alba ####

biplot_aa2 <- ggplot() +
  geom_point(data = pca_df_aa, aes(x = Comp.1, y = Comp.2, color = spot_status), 
             size = 4, alpha = 0.95) +
  stat_ellipse(data = pca_df_aa, aes(x = Comp.1, y = Comp.2, 
                                     color = spot_status, fill = spot_status), 
               type = "norm", level = 0.95, geom = "polygon",
               alpha = 0.3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_aa,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") + 
  guides(fill = "none") +
  xlab("PC1 (35.13 %)") + 
  ylab("PC2 (19.55 %)") + 
  labs(tag = "Abies alba") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28, face = "italic"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_aa,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

## 12.2.- Pinus sylvestris ####

biplot_ps2 <- ggplot() +
  geom_point(data = pca_df_ps, aes(x = Comp.1, y = Comp.2, color = spot_status), 
             size = 4, alpha = 0.95) +
  stat_ellipse(data = pca_df_ps, aes(x = Comp.1, y = Comp.2, 
                                     color = spot_status, fill = spot_status), 
               type = "norm", level = 0.95, geom = "polygon",
               alpha = 0.3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_ps,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") + 
  guides(fill = "none") +
  xlab("PC1 (48.58 %)") + 
  ylab("PC2 (19.97 %)") + 
  labs(tag = "Pinus sylvestris") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28, face = "italic"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_ps,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

## 12.3.- Pinus pinea ####

biplot_pp2 <- ggplot() +
  geom_point(data = pca_df_pp, aes(x = Comp.1, y = Comp.2, color = spot_status), 
             size = 4, alpha = 0.95) +
  stat_ellipse(data = pca_df_pp, aes(x = Comp.1, y = Comp.2, 
                                     color = spot_status, fill = spot_status), 
               type = "norm", level = 0.95, geom = "polygon",
               alpha = 0.3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_pp,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_fill_manual(breaks = c("coldspot", "hotspot"),
                    values = c("coldspot" = "#2274A5",
                               "hotspot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") + 
  guides(fill = "none") +
  xlab("PC1 (32.54 %)") + 
  ylab("PC2 (21.40 %)") + 
  labs(tag = "Pinus pinea") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        plot.tag = element_text(size = 28, face = "italic"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_pp,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

# 13.- Plotting - plot level ####
## 13.1.- Abies alba ####

pca_df_aa3 <- subset(pca_df_aa, vigor_id != "cold_healthy")
biplot_aa3 <- ggplot() + 
  geom_point(data = pca_df_aa3, aes(x = Comp.1, y = Comp.2, color = vigor_id), 
             size = 4, alpha = 0.95) + 
  stat_ellipse(data = pca_df_aa3, aes(x = Comp.1, y = Comp.2, 
                                     color = vigor_id, fill = vigor_id), 
               type = "norm", level = 0.95, geom = "polygon", alpha = 0.3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") + 
  geom_segment(data = loadings_df_aa, 
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "black", size = 0.8) + 
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"), 
                     values = c("hot_healthy" = "#D71515", 
                                "hot_damaged" = "#650304"), 
                     labels = c("Declining, healthy", 
                                "Declining, damaged"), 
                     name = "") + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"), 
                    values = c("hot_healthy" = "#D71515", 
                               "hot_damaged" = "#650304"), 
                    labels = c("Declining, healthy", 
                               "Declining, damaged"), 
                    name = "") + guides(fill = "none") + 
  xlab("PC1 (35.13 %)") + 
  ylab("PC2 (19.55 %)") + 
  labs(tag = " ") + 
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
        legend.title = element_text(size = 25), 
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_aa, 
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames), 
            size = 6, fontface = "bold")

## 13.2.- Pinus sylvestris ####

pca_df_ps3 <- subset(pca_df_ps, vigor_id != "cold_healthy")
biplot_ps3 <- ggplot() +
  geom_point(data = pca_df_ps3, aes(x = Comp.1, y = Comp.2, color = vigor_id), 
             size = 4, alpha = 0.95) +
  stat_ellipse(data = pca_df_ps3, aes(x = Comp.1, y = Comp.2, 
                                     color = vigor_id, fill = vigor_id), 
               type = "norm", level = 0.95, geom = "polygon",
               alpha = 0.3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_ps,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"), 
                     values = c("hot_healthy" = "#D71515", 
                                "hot_damaged" = "#650304"), 
                     labels = c("Declining, healthy", 
                                "Declining, damaged"), 
                     name = "") + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"), 
                    values = c("hot_healthy" = "#D71515", 
                               "hot_damaged" = "#650304"), 
                    labels = c("Declining, healthy", 
                               "Declining, damaged"), 
                    name = "") + guides(fill = "none") + 
  guides(fill = "none") +
  xlab("PC1 (48.58 %)") + 
  ylab("PC2 (19.97 %)") + 
  labs(tag = " ") +
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
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_ps,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

## 13.3.- Pinus pinea ####

pca_df_pp3 <- subset(pca_df_pp, vigor_id != "cold_healthy")
biplot_pp3 <- ggplot() +
  geom_point(data = pca_df_pp3, aes(x = Comp.1, y = Comp.2, color = vigor_id), 
             size = 4, alpha = 0.95) +
  stat_ellipse(data = pca_df_pp3, aes(x = Comp.1, y = Comp.2, 
                                     color = vigor_id, fill = vigor_id), 
               type = "norm", level = 0.95, geom = "polygon",
               alpha = 0.3) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df_pp,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"), 
                     values = c("hot_healthy" = "#D71515", 
                                "hot_damaged" = "#650304"), 
                     labels = c("Declining, healthy", 
                                "Declining, damaged"), 
                     name = "") + 
  scale_fill_manual(breaks = c("hot_healthy", "hot_damaged"), 
                    values = c("hot_healthy" = "#D71515", 
                               "hot_damaged" = "#650304"), 
                    labels = c("Declining, healthy", 
                               "Declining, damaged"), 
                    name = "") + guides(fill = "none") + 
  guides(fill = "none") +
  xlab("PC1 (32.54 %)") + 
  ylab("PC2 (21.40 %)") + 
  labs(tag = " ") +
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
        legend.title = element_text(size = 25),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df_pp,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

# 14.- Saving ####

tiff("04_figures/51_06_PCA_ellipses_comparisons_V2.tiff", units = "mm",
     width = 600, height = 800,
     res = 600, compression = "lzw")
(biplot_aa2 + biplot_ps2 + biplot_pp2 + 
  guide_area() + 
  plot_layout(ncol = 1, guides = "collect")) | 
(biplot_aa3 + biplot_ps3 + biplot_pp3 + 
  guide_area() + 
  plot_layout(ncol = 1, guides = "collect"))
dev.off()