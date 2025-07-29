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
## 6.1.- Height ####

h_all <- make_scatter_plot(clean_target, yvar = "height", ylab_txt = "Height (m)", 
                           tag = "A", show_y = TRUE)
h_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "height")
h_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "height")
h_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "height")

h_all <- h_all + ggtitle("All") + theme(plot.title = element_text(size = 30))
h_aa <- h_aa + ggtitle("A. alba") + theme(plot.title = element_text(size = 30, face = "italic"))
h_ps <- h_ps + ggtitle("P. sylv.") + theme(plot.title = element_text(size = 30, face = "italic"))
h_pp <- h_pp + ggtitle("P. pinea") + theme(plot.title = element_text(size = 30, face = "italic"))

## 6.2.- DBH ####

dbh_all <- make_scatter_plot(clean_target, yvar = "dbh", ylab_txt = "d.b.h. (cm)", 
                             tag = "B", show_y = TRUE)
dbh_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "dbh")
dbh_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "dbh")
dbh_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "dbh")

## 6.3.- C ####

c_all <- make_scatter_plot(clean_target, yvar = "percent_c", ylab_txt = "C content (%)", 
                           tag = "C", show_y = TRUE)
c_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "percent_c")
c_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "percent_c")
c_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "percent_c")

## 6.4.- N ####

n_all <- make_scatter_plot(clean_target, yvar = "percent_n", ylab_txt = "N content (%)", 
                           tag = "D", show_y = TRUE)
n_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "percent_n")
n_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "percent_n")
n_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "percent_n")

## 6.5.- C:N ####

cn_all <- make_scatter_plot(clean_target, yvar = "cn", ylab_txt = "C:N ratio", 
                            tag = "E", show_y = TRUE)
cn_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "cn")
cn_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "cn")
cn_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "cn")

## 6.6.- SLA  ####

sla_all <- make_scatter_plot(clean_target, yvar = "sla_22", ylab_txt = "SLA", 
                             tag = "F", show_y = TRUE)
sla_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "sla_22")
sla_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "sla_22")
sla_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "sla_22")

## 6.7.- Age ####

age_all <- make_scatter_plot(clean_target, yvar = "age", ylab_txt = "Age (years)",
                             tag = "G", show_y = TRUE)
age_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "age")
age_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "age")
age_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "age")

## 6.8.- Hegyi Index  ####

hegyi_all <- make_scatter_plot(clean_target, yvar = "hegyi_index", ylab_txt = "Hegyi Index", 
                               tag = "H", show_y = TRUE, show_x = TRUE)
hegyi_aa  <- make_scatter_plot(filter(clean_target, sp_id == "Abialba"),  yvar = "hegyi_index",
                               show_x = TRUE)
hegyi_ps  <- make_scatter_plot(filter(clean_target, sp_id == "Pinsylv"), yvar = "hegyi_index", 
                               show_x = TRUE)
hegyi_pp  <- make_scatter_plot(filter(clean_target, sp_id == "Pinpine"), yvar = "hegyi_index", 
                               show_x = TRUE)

hegyi_all <- hegyi_all + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        legend.spacing.x = unit(1, 'cm'),
        legend.key.size = unit(2, "cm"),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 7.- Select morpho ####
## 7.1.- Height ####
# No need to change it :)

## 7.2.- N ####

n_all_select <- n_all + labs(tag = "B")

## 7.3.- SLA ####

sla_all_select <- sla_all + labs(tag = "C")

## 7.4.- Age ####

age_all_select <- age_all + labs(tag = "D")

## 7.5.- Hegyi Index ####

hegyi_all_select <- hegyi_all + labs(tag = "E") +
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

tiff("04_figures/18_01_All_morpho3_scatterplots.tiff", units = "mm", width = 400, height = 700,
     res = 400, compression = "lzw")
h_all + plot_spacer() + h_aa + h_ps + h_pp + 
  dbh_all + plot_spacer() + dbh_aa + dbh_ps + dbh_pp + 
  c_all + plot_spacer() + c_aa + c_ps + c_pp + 
  n_all + plot_spacer() + n_aa + n_ps + n_pp + 
  cn_all + plot_spacer() + cn_aa + cn_ps + cn_pp + 
  sla_all + plot_spacer() + sla_aa + sla_ps + sla_pp + 
  age_all + plot_spacer() + age_aa + age_ps + age_pp + 
  hegyi_all + plot_spacer() + hegyi_aa + hegyi_ps + hegyi_pp + 
  guide_area() + 
  plot_layout(ncol = 5, widths = c(3, 1, 3, 3, 3), guides = "collect")
dev.off()

tiff("04_figures/18_01_Select_morpho3_scatterplots.tiff", units = "mm", width = 400, height = 500,
     res = 400, compression = "lzw")
h_all + plot_spacer() + h_aa + h_ps + h_pp + 
  n_all_select + plot_spacer() + n_aa + n_ps + n_pp + 
  sla_all_select + plot_spacer() + sla_aa + sla_ps + sla_pp + 
  age_all_select + plot_spacer() + age_aa + age_ps + age_pp + 
  hegyi_all_select + plot_spacer() + hegyi_aa + hegyi_ps + hegyi_pp + 
  guide_area() + 
  plot_layout(ncol = 5, widths = c(3, 1, 3, 3, 3), guides = "collect")
dev.off()