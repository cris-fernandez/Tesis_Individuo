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

clean_target <- clean_target %>% filter(mean_def_obs < 100)

# 5.- Plotting function ####
## 5.1.- Colour scales ####

spot_colors <- c("cold_healthy" = "#2274A5", "hot_healthy" = "#D71515", hot_damaged = "#650304")
spot_labels <- c("Non-declining", "D-Healthy", "D-Damaged")

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
    geom_point(aes(x = mean_def_obs, y = .data[[yvar]], col = vigor_id), alpha = 0.25, size = 1.3) +
    geom_smooth(aes(x = mean_def_obs, y = .data[[yvar]], col = vigor_id, fill = vigor_id),
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
## 6.1.- LWC ####

wc_all <- make_scatter_plot(clean_target, yvar = "wc_22", ylab_txt = "LWC (%)", 
                            tag = "A", show_y = TRUE)
wc_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "wc_22")
wc_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "wc_22")
wc_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "wc_22")

wc_all <- wc_all + ggtitle("All") + theme(plot.title = element_text(size = 30))
wc_aa <- wc_aa + ggtitle("A. alba") + theme(plot.title = element_text(size = 30, face = "italic"))
wc_ps <- wc_ps + ggtitle("P. sylv.") + theme(plot.title = element_text(size = 30, face = "italic"))
wc_pp <- wc_pp + ggtitle("P. pinea") + theme(plot.title = element_text(size = 30, face = "italic"))

## 6.2.- Chl. ####

chl_all <- make_scatter_plot(clean_target, yvar = "total_chl_fw_22", ylab_txt = expression(paste("Chl. (μg g"^"-1", ")")), 
                             tag = "B", show_y = TRUE)
chl_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "total_chl_fw_22")
chl_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "total_chl_fw_22")
chl_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "total_chl_fw_22")

## 6.3.- Chl. a/b ####

chlab_all <- make_scatter_plot(clean_target, yvar = "chla_chlb_22", ylab_txt = "Chl. a/b", 
                               tag = "C", show_y = TRUE)
chlab_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "chla_chlb_22")
chlab_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "chla_chlb_22")
chlab_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "chla_chlb_22")

## 6.4.- Carotenoids ####

xc_all <- make_scatter_plot(clean_target, yvar = "xc_fw_22", ylab_txt = expression(paste("Caroten. (μg g"^"-1", ")")), 
                            tag = "D", show_y = TRUE)
xc_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "xc_fw_22")
xc_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "xc_fw_22")
xc_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "xc_fw_22")

## 6.5.- Chl. / carotenoids ####

chlxc_all <- make_scatter_plot(clean_target, yvar = "chl_xc_22", ylab_txt = "Chl. / car.", 
                               tag = "E", show_y = TRUE)
chlxc_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "chl_xc_22")
chlxc_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "chl_xc_22")
chlxc_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "chl_xc_22")

## 6.6.- d13C  ####

d13c_all <- make_scatter_plot(clean_target, yvar = "leaf_d13c", ylab_txt = bquote("δ"~C^13~"(‰)"), 
                              tag = "F", show_y = TRUE)
d13c_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "leaf_d13c")
d13c_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "leaf_d13c")
d13c_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "leaf_d13c")

## 6.7.- d15N ####

d15n_all <- make_scatter_plot(clean_target, yvar = "leaf_d15n", ylab_txt = bquote("δ"~N^15~"(‰)"), 
                              tag = "F", show_y = TRUE)
d15n_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "leaf_d15n")
d15n_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "leaf_d15n")
d15n_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "leaf_d15n")

## 6.8.- d18O ####

d18o_all <- make_scatter_plot(clean_target, yvar = "leaf_d18o", ylab_txt = bquote("δ"~O^18~"(‰)"), 
                              tag = "F", show_y = TRUE)
d18o_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "leaf_d18o")
d18o_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "leaf_d18o")
d18o_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "leaf_d18o")

d18o_all <- d18o_all + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.spacing.x = unit(1, 'cm'),
        legend.key.size = unit(2, "cm"),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 7.- Select physio ####
## 7.1.- LWC ####
# No need to change it :)

## 7.2.- Chl. ####

chl_all_select <- chl_all + labs(tag = "B")

## 7.3.- d13C ####

d13c_all_select <- d13c_all + labs(tag = "C") +
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

tiff("04_figures/18_02_All_physio3_scatterplots.tiff", units = "mm", width = 400, height = 700,
     res = 400, compression = "lzw")
wc_all + plot_spacer() + wc_aa + wc_ps + wc_pp + 
  chl_all + plot_spacer() + chl_aa + chl_ps + chl_pp + 
  chlab_all + plot_spacer() + chlab_aa + chlab_ps + chlab_pp + 
  xc_all + plot_spacer() + xc_aa + xc_ps + xc_pp + 
  chlxc_all + plot_spacer() + chlxc_aa + chlxc_ps + chlxc_pp + 
  d13c_all + plot_spacer() + d13c_aa + d13c_ps + d13c_pp + 
  d15n_all + plot_spacer() + d15n_aa + d15n_ps + d15n_pp + 
  d18o_all + plot_spacer() + d18o_aa + d18o_ps + d18o_pp + 
  guide_area() + 
  plot_layout(ncol = 5, widths = c(3, 1, 3, 3, 3), guides = "collect")
dev.off()

tiff("04_figures/18_02_Select_physio3_scatterplots.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
wc_all + plot_spacer() + wc_aa + wc_ps + wc_pp + 
  chl_all_select + plot_spacer() + chl_aa + chl_ps + chl_pp + 
  d13c_all_select + plot_spacer() + d13c_aa + d13c_ps + d13c_pp + 
  guide_area() + 
  plot_layout(ncol = 5, widths = c(3, 1, 3, 3, 3), guides = "collect")
dev.off()