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
                                ifelse(clean_target$prec < 30,
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

clean_target$sp_id <- fct_relevel(clean_target$sp_id, "Abialba", "Pinsylv", "Pinpine")

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(mean_def_obs, height, dbh, chl_fw_22, xc_fw_22, chla_chlb_22,
                  chl_xc_22, percent_c, percent_n, cn_ratio, leaf_d13c, 
                  leaf_d18o, leaf_d15n, wood_d13c_17, wood_d13c_22, sla_22,
                  age, hegyi_index, mean_bai, mean_1980, mean_20, mean_15,
                  mean_10, mean_05, Rt12, Rt17, Rt22, Rs12, Rs17, wc_22,
                  tree_number, sp_id, spot_status)) %>% 
  dplyr::select(sort(names(.)))

# 6.- Data normalization ####

# However, since I do not know whether scale() does exactly the same as 
# the manual standardization, I will test both procedures

norm_target <- clean_target %>%
  mutate(defoliation_ST = (mean_def_obs - mean(mean_def_obs, na.rm = T)) / sd(mean_def_obs, na.rm = T),
         height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         dbh_ST = (dbh - mean(dbh, na.rm = T)) / sd(dbh, na.rm = T),
         chl_ST = (chl_fw_22 - mean(chl_fw_22, na.rm = T)) / sd(chl_fw_22, na.rm = T),
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
         wc_ST = (wc_22 - mean(wc_22, na.rm = T)) / sd(wc_22, na.rm = T))

norm_target <- norm_target %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

# 7.- Correlations matrix ####

# Omission of NAs
norm_target <- na.omit(norm_target)
clean_target <- clean_target %>% dplyr::select(-c(wood_d13c_17, wood_d13c_22))
clean_target <- na.omit(clean_target)

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

# 9.- Biplot ####

# Just to compare whether we are making the biplot correctly or not

fviz_pca_var(pca_results, col.var = "black")


# 10.- Biplot - manually ####

## 10.1.- Loadings dataframe

loadings_df <- as.data.frame(pca_results$loadings[, 1:2]) # They are not merged
# into pca_df as they have different row numbers

loadings_df$variable <- rownames(loadings_df) # So we know what variable is which

# Adding a column with the proper names of the variables to appear on the PCA:

loadings_df$varnames <- c("Defoliation", "Height", "d.b.h.", "Chl.", "Carotenoids",
                          "Chl. a / b", "Chl. / carotenoids", "Leaves C content",
                          "Leaves N content", "Leaves C:N ratio", "Leaves δ13C",
                          "Leaves δ15N", "Leaves δ18O", "SLA", "Age", "Hegyi Index",
                          "BAI", "BAI 1980", "BAI 20", "BAI 15", "BAI 10", "BAI 05",
                          "Rt12", "Rs12", "Rt17", "Rs17", "Rt22", "LWC")

## 10.2.- Scale factor ####

# Scale factor is just a constant number used to multiply the length of the vectors 
# thus allowing us to see them more clearly

scale_factor <- 15 

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


## 10.6.- Biplot, all ####

biplot_all <- ggplot() +
  geom_raster(data = dens_df, aes(x = x, y = y, fill = z), interpolate = TRUE) +
  scale_fill_gradientn(colours = c(scales::alpha("black", 0),
                                   scales::alpha("black", 0.4))) +
  geom_contour(data = dens_df,
               aes(x = x, y = y, z = z),
               breaks = level_50,
               color = "grey40",
               size = 0.7,
               linetype = "dashed") + 
  geom_point(data = pca_df, aes(x = Comp.1, y = Comp.2), color = "black", 
             alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (26.66 %)") + 
  ylab("PC2 (13.07 %)") + 
  labs(tag = "A") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.tag = element_text(size = 22),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5)) + 
  geom_text(data = loadings_df,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 3.5, fontface = "bold")

## 10.7.- Biplot, sp ####

biplot_sp <- ggplot() +
  geom_point(data = pca_df, aes(x = Comp.1, y = Comp.2, color = sp_id), 
             alpha = 1, size = 2.75) +
  scale_colour_manual(name = "",
                      values = c("Abialba" = "#746fb2",
                                 "Pinsylv" = "#1b9e77",
                                 "Pinpine" = "#db5f02"),
                      labels = c("Abialba" = "Abies alba",
                                 "Pinsylv" = "Pinus sylvestris",
                                 "Pinpine" = "Pinus pinea")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "grey50", size = 0.8, alpha = 0.5) +
  guides(fill = "none") +
  xlab("PC1 (26.66 %)") + 
  ylab("PC2 (13.07 %)") + 
  labs(tag = "B") +
  theme_classic() + 
  theme(legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.tag = element_text(size = 22),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5),
        legend.position = c(0.98, 0.02))

## 10.8.- Biplot, status ####

biplot_status <- ggplot() +
  geom_point(data = pca_df, aes(x = Comp.1, y = Comp.2, color = spot_status), 
             alpha = 1, size = 2.75) +
  scale_colour_manual(name = "",
                      values = c("hotspot" = "red4",
                                 "coldspot" = "navy"),
                      labels = c("hotspot" = "Declining",
                                 "coldspot" = "Non-declining")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "grey50", size = 0.8, alpha = 0.5) +
  guides(fill = "none") +
  xlab("PC1 (26.66 %)") + 
  ylab("PC2 (13.07 %)") + 
  labs(tag = "C") +
  theme_classic() + 
  theme(legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.tag = element_text(size = 22),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    linewidth = 0.5),
        legend.position = c(0.98, 0.02))


tiff("04_figures/07_01_pca_pro.tiff", units = "mm", 
     width = 400, height = 400,
     res = 700, compression = "lzw")
biplot_all / (biplot_sp + biplot_status) + 
  plot_layout(guides = 'collect', heights = c(2.5, 1))
dev.off()

