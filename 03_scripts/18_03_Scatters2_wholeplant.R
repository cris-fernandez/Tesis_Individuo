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
  mutate(sp_id = factor(sp_id),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target$cn <- clean_target$percent_c / clean_target$percent_n

# 5.- Plotting function ####
## 5.1.- Colour scales ####

spot_colors <- c("coldspot" = "#2274A5", "hotspot" = "#D71515")
spot_labels <- c("Non-declining site", "Declining site")

## 5.2.- Reusable scales ####

spot_scale <- list(
  scale_colour_manual(values = spot_colors, breaks = names(spot_colors), labels = spot_labels),
  scale_fill_manual(values = spot_colors, breaks = names(spot_colors), labels = spot_labels),
  theme_classic(),
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.tag = element_text(size = 25)))

## 5.3.- Function ####
make_scatter_plot <- function(data, yvar, ylab_txt = " ", tag = " ", show_y = FALSE, show_x = FALSE, ylim_vals = NULL) {
  ggplot(data) +
    geom_point(aes(x = mean_def_obs, y = .data[[yvar]], col = spot_status), alpha = 0.25, size = 1.3) +
    geom_smooth(aes(x = mean_def_obs, y = .data[[yvar]], col = spot_status, fill = spot_status),
                method = "lm", se = TRUE, size = 1, alpha = 0.2) +
    labs(tag = tag, y = ylab_txt, x = if (show_x) "Defoliation (%)" else "") +
    (if (!is.null(ylim_vals)) ylim(ylim_vals[1], ylim_vals[2]) else NULL) +
    spot_scale +
    theme(axis.text.y = element_text(size = if (show_y) 22 else 0),
          axis.title.y = element_text(size = if (show_y) 30 else 0),
          axis.title.x = element_text(size = if (show_x) 26 else 0),
          axis.text.x  = element_text(size = if (show_x) 20 else 0),
          axis.ticks.x = element_line(colour = "black"),
          axis.line.x  = element_line(colour = "black"))
}


# 6.- Scatterplots ####
## 6.1.- BAI80 ####

bai80_all <- make_scatter_plot(clean_target, yvar = "mean_1980", ylab_txt = expression(paste("BAI80 (mm² year"^"-1", ")")), 
                           tag = "A", show_y = TRUE)
bai80_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "mean_1980")
bai80_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "mean_1980")
bai80_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "mean_1980")

bai80_all <- bai80_all + ggtitle("All") + theme(plot.title = element_text(size = 30))
bai80_aa <- bai80_aa + ggtitle("A. alba") + theme(plot.title = element_text(size = 30, face = "italic"))
bai80_ps <- bai80_ps + ggtitle("P. sylv.") + theme(plot.title = element_text(size = 30, face = "italic"))
bai80_pp <- bai80_pp + ggtitle("P. pinea") + theme(plot.title = element_text(size = 30, face = "italic"))

## 6.2.- BAI05 ####

bai05_all <- make_scatter_plot(clean_target, yvar = "mean_05", ylab_txt = expression(paste("BAI05 (mm² year"^"-1", ")")), 
                             tag = "B", show_y = TRUE)
bai05_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "mean_05")
bai05_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "mean_05")
bai05_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "mean_05")

## 6.3.- Rt12 ####

rt12_all <- make_scatter_plot(clean_target, yvar = "Rt12", ylab_txt = "Rt 2012", 
                           tag = "D", show_y = TRUE)
rt12_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "Rt12")
rt12_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "Rt12")
rt12_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "Rt12")

## 6.5.- Rt17 ####

rt17_all <- make_scatter_plot(clean_target, yvar = "Rt17", ylab_txt = "Rt 2017", 
                              tag = "E", show_y = TRUE)
rt17_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "Rt17")
rt17_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "Rt17")
rt17_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "Rt17")

## 6.6.- Rt22 ####

rt22_all <- make_scatter_plot(clean_target, yvar = "Rt22", ylab_txt = "Rt 2022", 
                              tag = "F", show_y = TRUE)
rt22_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "Rt22")
rt22_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "Rt22")
rt22_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "Rt22")

## 6.7.- Rs12 ####

rs12_all <- make_scatter_plot(clean_target, yvar = "Rs12", ylab_txt = "Rs 2012", 
                              tag = "G", show_y = TRUE)
rs12_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "Rs12")
rs12_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "Rs12")
rs12_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "Rs12")

## 6.5.- Rs17 ####

rs17_all <- make_scatter_plot(clean_target, yvar = "Rs17", ylab_txt = "Rs 2017", 
                              tag = "H", show_y = TRUE, show_x = TRUE)
rs17_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "Rs17",
                              show_x = TRUE)
rs17_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "Rs17",
                              show_x = TRUE)
rs17_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "Rs17",
                              show_x = TRUE)

rs17_all <- rs17_all + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.spacing.x = unit(1, 'cm'),
        legend.key.size = unit(2, "cm"),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 7.- Select whole-plant ####
## 7.1.- BAI80 ####
# No need to change it :)

## 7.2.- BAI05 ####
# No need to change it :)

## 7.3.- Rs12 ####

rs12_all_select <- rs12_all + labs(tag = "C") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.spacing.x = unit(1, 'cm'),
        legend.key.size = unit(2, "cm"),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 8.- Plotting ####

tiff("04_figures/18_03_All_wholeplant2_scatterplots.tiff", units = "mm", width = 400, height = 800,
     res = 400, compression = "lzw")
bai80_all + plot_spacer() + bai80_aa + bai80_ps + bai80_pp + 
  bai05_all + plot_spacer() + bai05_aa + bai05_ps + bai05_pp + 
  rt12_all + plot_spacer() + rt12_aa + rt12_ps + rt12_pp + 
  rt17_all + plot_spacer() + rt17_aa + rt17_ps + rt17_pp + 
  rt22_all + plot_spacer() + rt22_aa + rt22_ps + rt22_pp + 
  rs12_all + plot_spacer() + rs12_aa + rs12_ps + rs12_pp + 
  rs17_all + plot_spacer() + rs17_aa + rs17_ps + rs17_pp + 
  guide_area() + 
  plot_layout(ncol = 5, widths = c(3, 1, 3, 3, 3), guides = "collect")
dev.off()

tiff("04_figures/18_03_Select_wholeplant2_scatterplots.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
bai80_all + plot_spacer() + bai80_aa + bai80_ps + bai80_pp + 
  bai05_all + plot_spacer() + bai05_aa + bai05_ps + bai05_pp + 
  rs12_all_select + plot_spacer() + rt12_aa + rt12_ps + rt12_pp + 
  guide_area() + 
  plot_layout(ncol = 5, widths = c(3, 1, 3, 3, 3), guides = "collect")
dev.off()