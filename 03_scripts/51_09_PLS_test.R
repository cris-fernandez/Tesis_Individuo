rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "BiocManager", "mixOmics") #list of packages
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

# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3))

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
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot")) %>% 
  filter(mean_def_obs < 100)

# Outlayers deletion:

clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 4783, NA, clean_target$mean_1980)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 60 & clean_target$sp_id == "Abialba",
                                    NA, clean_target$mean_def_obs)
clean_target$sla_22 <- ifelse(clean_target$sla_22 > 99 & clean_target$sp_id == "Pinsylv",
                              NA, clean_target$sla_22)
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 < 150 & clean_target$sp_id == "Pinsylv",
                                       NA, clean_target$total_chl_fw_22)
clean_target$xc_fw_22 <- ifelse(clean_target$xc_fw_22 < 5 & clean_target$sp_id == "Pinsylv",
                                NA, clean_target$xc_fw_22)
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 < 40 & clean_target$sp_id == "Pinpine",
                                       NA, clean_target$total_chl_fw_22)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 58 & clean_target$sp_id == "Pinpine",
                                    NA, clean_target$mean_def_obs)
clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 3000 & clean_target$sp_id == "Abialba" & clean_target$spot_status == "hotspot",
                                 NA, clean_target$mean_1980)

# 5.- Selecting variables ####

clean_target <- clean_target %>%
  dplyr::select(height, mean_1980, sla_22, percent_n,
                total_chl_fw_22, xc_fw_22, leaf_d13c, leaf_d18o_corrected,
                mean_def_obs, tree_number, plot_id, site, pair_id,
                sp_id, spot_status, vigor_id) %>%
  filter(!is.na(sp_id))

# Preparing the variables for the PLS ####

predictors <- c("height", "mean_1980", "sla_22", "percent_n", 
                "total_chl_fw_22", "xc_fw_22", "leaf_d13c", "leaf_d18o_corrected")

pls_data <- clean_target %>%
  dplyr::select(all_of(predictors),
    sp_id, spot_status, site, plot_id, pair_id) %>%
  filter(complete.cases(across(all_of(predictors))),
    !is.na(sp_id),
    !is.na(spot_status)) %>%
  mutate(sp_status = paste0(sp_id, "-", spot_status))

# Sample size per sp_status
table(pls_data$sp_status)

# 6. Standardising ####

predictors_norm <- scale(pls_data[, predictors])

group_levels <- droplevels(as.factor(pls_data$sp_status))


# 3. Fit PLS-DA 

plsda_fit <- mixOmics::plsda(predictors_norm, group_levels, ncomp = 2)
# ncomp = number of axes, like PC1 and PC2 of a PCA.

plsda_fit # "No variable selection means that my variables have not been subset,
# therefore it is selecting everything (this is apparently a good sign (?))



# 4.- PLS-DA scores for each tree ####

scores_df <- as.data.frame(plsda_fit$variates$X[, 1:2])
colnames(scores_df) <- c("PLS1", "PLS2")

# Metadata must be in the same row order as the observations used to fit the model
scores_df$sp_id <- pls_data$sp_id
scores_df$spot_status <- pls_data$spot_status

scores_df$sp_id <- factor(scores_df$sp_id)
scores_df$spot_status <- factor(scores_df$spot_status)


# 5.- Predictor loadings ####

loadings_matrix <- plsda_fit$loadings$X[, 1:2, drop = FALSE]

loadings_df <- data.frame(
  PLS1 = loadings_matrix[, 1],
  PLS2 = loadings_matrix[, 2],
  varnames = rownames(loadings_matrix))

# 6.- Rescaling arrows ####

arrow_scale <- 6.5
loadings_df$PLS1_scaled <- loadings_df$PLS1 * arrow_scale
loadings_df$PLS2_scaled <- loadings_df$PLS2 * arrow_scale


# 7.- Biplot ####
# I guess it would technically not be a biplot but whatever (?)

biplot_plsda <- ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(data = scores_df,
    aes(x = PLS1, y = PLS2,
      col = sp_id,
      shape = spot_status),
    size = 4, alpha = 0.75, stroke = 1.1) +
  scale_shape_manual(breaks = c("Coldspot", "Hotspot"),
                    values = c("Coldspot" = 19,
                               "Hotspot" = 1),
                    labels = c("Non-declining stands",
                               "Declining stands"),
                    name = "") + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c(expression(italic("Abies alba")),
                                expression(italic("Pinus sylvestris")),
                                expression(italic("Pinus pinea"))),
                     name = "") +
  geom_segment(data = loadings_df,
    aes(x = 0, y = 0, xend = PLS1_scaled, yend = PLS2_scaled),
    arrow = arrow(length = unit(0.2, "cm")),
    colour = "black",linewidth = 0.8) + 
  geom_text(data = loadings_df,
    aes(x = PLS1_scaled * 1.10,
        y = PLS2_scaled * 1.10,
        label = varnames), size = 6, fontface = "bold") + 
  guides(colour = guide_legend(order = 1, nrow = 3, byrow = TRUE),
    shape = guide_legend(order = 2, nrow = 2, byrow = TRUE)) +
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
                                    linewidth = 0.5))

# Saving:
tiff("04_figures/51_09_PLS-DA.tiff", units = "mm",
     width = 250, height = 250,
     res = 600, compression = "lzw")
biplot_plsda
dev.off()
#   
#   theme(
#     axis.text.x = element_text(size = 15),
#     axis.text.y = element_text(size = 15),
#     axis.title.x = element_text(size = 20),
#     axis.title.y = element_text(size = 20),
#     plot.title = element_text(size = 24, face = "bold"),
#     legend.position = "right",
#     legend.text = element_text(size = 13),
#     legend.title = element_text(size = 14),
#     panel.border = element_rect(
#       colour = "black",
#       fill = NA,
#       linewidth = 0.5
#     )
#   )
