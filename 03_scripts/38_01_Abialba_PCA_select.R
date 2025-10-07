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

# Filtering species:

clean_target <- clean_target %>% filter(sp_id == "Abialba")

# 6.- Data normalization ####

# However, since I do not know whether scale() does exactly the same as 
# the manual standardization, I will test both procedures

clean_target <- na.omit(clean_target)

norm_target <- clean_target %>%
  mutate(chl_ST = (total_chl_fw_22 - mean(total_chl_fw_22, na.rm = T)) / sd(total_chl_fw_22, na.rm = T),
         percent_n_ST = (percent_n - mean(percent_n, na.rm = T)) / sd(percent_n, na.rm = T),
         leaf_d13c_ST = (leaf_d13c - mean(leaf_d13c, na.rm = T)) / sd(leaf_d13c, na.rm = T),
         age_ST = (age - mean(age, na.rm = T)) / sd(age, na.rm = T),
         height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         sla_ST = (sla_22 - mean(sla_22, na.rm = T)) / sd(sla_22, na.rm = T),
         hegyi_index_ST = (hegyi_index - mean(hegyi_index, na.rm = T)) / sd(hegyi_index, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T),
         bai_05_ST = (mean_05 - mean(mean_05, na.rm = T)) / sd(mean_05, na.rm = T),
         Rs12_ST = (Rs12 - mean(Rs12, na.rm = T)) / sd(Rs12, na.rm = T),
         wc_ST = (wc_22 - mean(wc_22, na.rm = T)) / sd(wc_22, na.rm = T))

norm_target <- norm_target %>% dplyr::select(contains("_ST")) %>% 
  dplyr::select(-spot_status)

# 7.- Correlations matrix ####

# The chart is needed for the PCA:

correlogram <- cor(norm_target)
# ggcorrplot(correlogram)

# 8.- PCA analysis ####

pca_results <- princomp(norm_target)
summary(pca_results)

# The first two components explain only 36.91% of the data variance!

pca_results$loadings[, 1:2]

pca_df <- cbind(norm_target, pca_results$scores) # Scores for the points
pca_df$sp_id <- clean_target$sp_id
pca_df$spot_status <- clean_target$spot_status
pca_df$vigor_id <- clean_target$vigor_id

# 9.- Biplot ####

# Just to compare whether we are making the biplot correctly or not

fviz_pca_var(pca_results, col.var = "black")


# 10.- Biplot - sp ####

## 10.1.- Loadings dataframe ####

loadings_df <- as.data.frame(pca_results$loadings[, 1:2]) # They are not merged
# into pca_df as they have different row numbers

loadings_df$variable <- rownames(loadings_df) # So we know what variable is which

# Adding a column with the proper names of the variables to appear on the PCA:

loadings_df$varnames <- c("Chl.", "N", "δ13C",
                          "Age", "Height", "SLA", "Hegyi", "BAI80", "BAI05", 
                          "Rs12", "LWC")

## 10.2.- Scale factor ####

# Scale factor is just a constant number used to multiply the length of the vectors 
# thus allowing us to see them more clearly

scale_factor <- 20 

## 10.3.- Multiplying ####

loadings_df <- loadings_df %>% 
  mutate(Comp.1 = scale_factor * Comp.1,
         Comp.2 = scale_factor * Comp.2)

# 11.- Biplot - vigor ####
## 11.1.- Density raster ####

# Instead of using the argument stat_density_2d, we will create a raster, as 
# it allows us to better customize the looks and looks better <3

# This is performed with kde2d, which does a Two-Dimensional Kernel Density Estimation

dens <- kde2d(x = pca_df$Comp.1,
              y = pca_df$Comp.2,
              n = 200)  # resolution

dens_df <- as.data.frame(expand.grid(x = dens$x, y = dens$y))
dens_df$z <- as.vector(dens$z)

## 11.2.- Density 50% ####

# To add a dashed contour line marking the space where the 50% of the trees 
# might appear, we just need to calculate the density at the 50th percentile

z_sorted <- sort(as.vector(dens$z))
cdf <- cumsum(z_sorted) / sum(z_sorted)

level_50 <- z_sorted[which.min(abs(cdf - 0.1))]

# Assigning each density value to each vigor_id factor:

vigor_levels <- levels(pca_df$vigor_id)

dens_list <- lapply(vigor_levels, function(v) {
  data_sub <- pca_df %>% filter(vigor_id == v)
  dens <- kde2d(x = data_sub$Comp.1, y = data_sub$Comp.2, n = 200)
  
  dens_df <- as.data.frame(expand.grid(x = dens$x, y = dens$y))
  dens_df$z <- as.vector(dens$z)
  dens_df$vigor_id <- v
  
  # Calcular nivel 50% para este grupo
  z_sorted <- sort(as.vector(dens$z))
  cdf <- cumsum(z_sorted) / sum(z_sorted)
  level_50 <- z_sorted[which.min(abs(cdf - 0.1))]
  
  list(data = dens_df, level_50 = level_50)
})


# Crear un dataframe con los niveles de contorno 50% por grupo
level_50_df <- data.frame(
  vigor_id = vigor_levels,
  level_50 = sapply(dens_list, function(x) x$level_50)
)

#MASCHATGPT
sp_colors <- c("cold_healthy" = "#2274A5",
               "hot_healthy" = "#D71515",
               "hot_damaged" = "#650304")

# Aplicar colores con alpha según z
dens_df_all <- do.call(rbind, lapply(dens_list, function(d) d$data))

# Normalizar z por grupo y aplicar alpha
dens_df_all <- dens_df_all %>%
  group_by(vigor_id) %>%
  mutate(z_scaled = (z - min(z)) / (max(z) - min(z)),
         fill = scales::alpha(sp_colors[vigor_id], z_scaled * 1)) %>%
  ungroup()

## 11.3.- Biplot, vigor ####

biplot_vigor <- ggplot() +
  geom_tile(data = dens_df_all, aes(x = x, y = y, fill = z), alpha = 0.5) +
  scale_fill_gradientn(colours = viridis::viridis(10)) +
  geom_contour(data = dens_df_all,
               aes(x = x, y = y, z = z, color = vigor_id),
               breaks = level_50,
               size = 0.7,
               linetype = "dashed") + 
  geom_point(data = pca_df, aes(x = Comp.1, y = Comp.2, color = vigor_id), 
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
  geom_segment(data = loadings_df,
               aes(x = 0, y = 0, xend = Comp.1, yend = Comp.2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", size = 0.8) +
  guides(fill = "none") +
  xlab("PC1 (22.35 %)") + 
  ylab("PC2 (14.57 %)") + 
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
  geom_text(data = loadings_df,
            aes(x = Comp.1 * 1.1, y = Comp.2 * 1.1, label = varnames),
            size = 6, fontface = "bold")

# 12.- Plotting ####

tiff("04_figures/38_01_Abialba_PCA_select.tiff", units = "mm",
     width = 300, height = 300,
     res = 600, compression = "lzw")
biplot_vigor
dev.off()