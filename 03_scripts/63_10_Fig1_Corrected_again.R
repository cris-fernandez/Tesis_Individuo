rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "lme4", "lmerTest", "emmeans", "mgcv", "broom.mixed", "xlsx") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages


setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading data ####

ci_df_2 <- read.csv("02_clean_data/63_01_AICc_discrete2.csv", 
                    header = T, sep = ",") %>% dplyr::select(-X)

ci_df_3 <- read.csv("02_clean_data/63_02_AICc_discrete3.csv", 
                    header = T, sep = ",") %>% dplyr::select(-X)

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv",
                         header = T, sep = ",") %>% dplyr::select(-X) %>%
  mutate(site = substr(plot_id, 1, 3))
# 
# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3))

# 2.- Variable tidying ####

ci_df_2$spot_status <- factor(ci_df_2$spot_status, levels = c("coldspot", "hotspot"))
ci_df_2$sp_id <- factor(ci_df_2$sp_id, levels = c("Abialba", "Pinsylv", "Pinpine"))

ci_df_3$vigor_id <- factor(ci_df_3$vigor_id, levels = c("hot_healthy", "hot_damaged"))
ci_df_3$sp_id <- factor(ci_df_3$sp_id, levels = c("Abialba", "Pinsylv", "Pinpine"))

# 3.- Clean target data tidying ####

clean_target <- clean_target %>% 
  dplyr::select(-contains("_23")) %>% 
  filter(mean_def_obs < 100)

# Adding T290 defoliation info:
clean_target <- clean_target %>% 
  mutate(mean_def_obs = ifelse(tree_number == "T290", 15, mean_def_obs))

# Additional IDs ####
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

# Data corrections #####
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

# 4.- Plot level ####
# 4.1.- Height ####
h_df <- ci_df_2 %>% filter(variable == "height")

fig_2_h <- ggplot(h_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(data = clean_target, (aes(x = sp_id, y = height, colour = spot_status,
                                       shape = pair_id)), 
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05), size = 1.5, alpha = 0.15) + 
  scale_x_discrete(expand = c(0.1, 0.1)) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("Height (m)") +  
  # ylim(10, 35) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.2.- SLA ####

sla_df <- ci_df_2 %>% filter(variable == "sla_22")

fig_2_sla <- ggplot(sla_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(data = clean_target, (aes(x = sp_id, y = sla_22, colour = spot_status,
                                       shape = pair_id)), 
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05), size = 1.5, alpha = 0.15) + 
  scale_x_discrete(expand = c(0.1, 0.1)) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(expression(paste("SLA (cm² g"^"-1", ")"))) + 
  # ylim(39, 61) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.3.- N ####

n_df <- ci_df_2 %>% filter(variable == "percent_n")

fig_2_n <- ggplot(n_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(data = clean_target, (aes(x = sp_id, y = percent_n, colour = spot_status,
                                       shape = pair_id)), 
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05), size = 1.5, alpha = 0.15) + 
  scale_x_discrete(expand = c(0.1, 0.1)) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "A. alba",
                              "Pinsylv" = "P. sylvestris",
                              "Pinpine" = "P. pinea")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("N content (%)") + 
  # ylim(0.8, 2) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 25, vjust = 1, angle = 45, hjust = 1,
                                   face = "italic"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.4.- Chl. ####

chl_df <- ci_df_2 %>% filter(variable == "total_chl_fw_22")

fig_2_chl <- ggplot(chl_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(data = clean_target, (aes(x = sp_id, y = total_chl_fw_22, colour = spot_status,
                                       shape = pair_id)), 
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05), size = 1.5, alpha = 0.15) + 
  scale_x_discrete(expand = c(0.1, 0.1)) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(expression(paste("Chl. (μg g"^"-1", ")"))) +
  # ylim(350, 2700) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.5.- Car. ####

xc_df <- ci_df_2 %>% filter(variable == "xc_fw_22")

fig_2_xc <- ggplot(xc_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(data = clean_target, (aes(x = sp_id, y = xc_fw_22, colour = spot_status,
                                       shape = pair_id)), 
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05), size = 1.5, alpha = 0.15) + 
  scale_x_discrete(expand = c(0.1, 0.1)) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(expression(paste("Car. (μg g"^"-1", ")"))) +
  # ylim(20, 60) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.6.- d13C ####

d13c_df <- ci_df_2 %>% filter(variable == "leaf_d13c")

fig_2_d13c <- ggplot(d13c_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(data = clean_target, (aes(x = sp_id, y = leaf_d13c, colour = spot_status,
                                       shape = pair_id)), 
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05), size = 1.5, alpha = 0.15) + 
  scale_x_discrete(expand = c(0.1, 0.1)) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(bquote("δ"~C^13~"(‰)")) +
  # ylim(-30, -23.5) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.7.- d18O ####

d18o_df <- ci_df_2 %>% filter(variable == "leaf_d18o_corrected")

fig_2_d18o <- ggplot(d18o_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(data = clean_target, (aes(x = sp_id, y = leaf_d18o_corrected, colour = spot_status,
                                       shape = pair_id)), 
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05), size = 1.5, alpha = 0.15) + 
  scale_x_discrete(expand = c(0.1, 0.1)) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "A. alba",
                              "Pinsylv" = "P. sylvestris",
                              "Pinpine" = "P. pinea")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(bquote("δ"~O^18~"(‰)")) +
  # ylim(-30, -23.5) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 25, vjust = 1, angle = 45, hjust = 1,
                                   face = "italic"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.8.- BAI80 ####

bai80_df <- ci_df_2 %>% filter(variable == "mean_1980")

fig_2_bai80 <- ggplot(bai80_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.5)) + 
  scale_x_discrete(expand = c(0.1, 0.1)) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.5)) +
  geom_point(data = clean_target, (aes(x = sp_id, y = mean_1980, colour = spot_status,
                                       shape = pair_id)), 
             position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.05), size = 1.5, alpha = 0.15) + 
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + 
  # ylim(350, 2700) + 
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 5.- Plotting defoliation ####

hot_target <- clean_target %>% filter(!vigor_id == "cold_healthy") %>% 
  droplevels()

# 5.1.- Height ####

fig_3_h <- ggplot(hot_target) + 
  geom_point(aes(x = mean_def_obs, y = height, colour = sp_id), size = 2, alpha = 0) + 
  geom_smooth(data = hot_target %>% filter(sp_id == "Pinsylv"),
              aes(x = mean_def_obs, y = height, colour = sp_id, fill = sp_id), 
              linewidth = 2, alpha = 0.3, method = "lm") +
  geom_point(aes(x = mean_def_obs, y = height, colour = sp_id,
                 shape = pair_id), size = 2, alpha = 0.4) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  xlab("") + 
  ylab("") +  
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal",
        legend.position = "none")

# 5.2.- SLA ####

fig_3_sla <- ggplot(hot_target) + 
  geom_point(aes(x = mean_def_obs, y = sla_22, colour = sp_id), size = 2, alpha = 0) + 
  geom_point(aes(x = mean_def_obs, y = sla_22, colour = sp_id,
                 shape = pair_id), size = 2, alpha = 0.4) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  xlab("") + 
  ylab("") +  
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal",
        legend.position = "none")

# 5.3.- N ####

fig_3_n <- ggplot(hot_target) + 
  geom_point(aes(x = mean_def_obs, y = percent_n, colour = sp_id), size = 2, alpha = 0) + 
  geom_smooth(data = hot_target %>% filter(sp_id == "Pinsylv"),
              aes(x = mean_def_obs, y = percent_n, colour = sp_id, fill = sp_id), 
              linewidth = 2, alpha = 0.3, method = "lm") +
  geom_point(aes(x = mean_def_obs, y = percent_n, colour = sp_id,
                 shape = pair_id), size = 2, alpha = 0.4) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  xlab("Defoliation (%)") + 
  ylab("") +  
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 25),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal",
        legend.position = "none")

# 5.4.- Chl. ####

fig_3_chl <- ggplot(hot_target) + 
  geom_point(aes(x = mean_def_obs, y = total_chl_fw_22, colour = sp_id), size = 2, alpha = 0) + 
  geom_smooth(data = hot_target %>% filter(sp_id != "Abialba"),
              aes(x = mean_def_obs, y = total_chl_fw_22, colour = sp_id, fill = sp_id), 
              linewidth = 2, alpha = 0.3, method = "lm") +
  geom_point(aes(x = mean_def_obs, y = total_chl_fw_22, colour = sp_id,
                 shape = pair_id), size = 2, alpha = 0.4) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  xlab("") + 
  ylab("") +  
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal",
        legend.position = "none")

# 5.5.- Car. ####

fig_3_xc <- ggplot(hot_target) + 
  geom_point(aes(x = mean_def_obs, y = xc_fw_22, colour = sp_id), size = 2, alpha = 0) + 
  geom_smooth(data = hot_target %>% filter(sp_id == "Pinpine"),
              aes(x = mean_def_obs, y = xc_fw_22, colour = sp_id, fill = sp_id), 
              linewidth = 2, alpha = 0.3, method = "lm") +
  geom_point(aes(x = mean_def_obs, y = xc_fw_22, colour = sp_id,
                 shape = pair_id), size = 2, alpha = 0.4) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  xlab("") + 
  ylab("") +  
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal",
        legend.position = "none")

# 5.6.- d13C ####

fig_3_d13c <- ggplot(hot_target) + 
  geom_point(aes(x = mean_def_obs, y = leaf_d13c, colour = sp_id), size = 2, alpha = 0) + 
  geom_point(aes(x = mean_def_obs, y = leaf_d13c, colour = sp_id,
                 shape = pair_id), size = 2, alpha = 0.4) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  xlab("") + 
  ylab("") +  
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal",
        legend.position = "none")

# 5.7.- d18O ####

fig_3_d18o <- ggplot(hot_target) +
  geom_point(aes(x = mean_def_obs, y = leaf_d18o_corrected, colour = sp_id), size = 2, alpha = 0) +
  geom_point(aes(x = mean_def_obs, y = leaf_d18o_corrected, colour = sp_id,
                 shape = pair_id), size = 2, alpha = 0.4) +
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  xlab("Defoliation (%)") +
  ylab("") +
  theme_classic() +
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_text(size = 25),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 5.8.- BAI80 ####

fig_3_bai80 <- ggplot(hot_target) + 
  geom_point(aes(x = mean_def_obs, y = mean_1980, colour = sp_id), size = 2, alpha = 0) + 
  geom_point(aes(x = mean_def_obs, y = mean_1980, colour = sp_id,
                 shape = pair_id), size = 2, alpha = 0.4) + 
  scale_shape_manual(values = c(16,16,16,15,15,17,18)) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("Abies alba",
                                "Pinus sylvestris",
                                "Pinus pinea"),
                     name = "") + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#785EF0",
                               "Pinsylv" = "#FFB000",
                               "Pinpine" = "#990000"),
                    labels = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea"),
                    name = "") + 
  xlab("") + 
  ylab("") +  
  theme_classic() + 
  theme(axis.ticks.length = unit(-5, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal",
        legend.position = "none")

# 6.- Plotting ####

tiff("04_figures/63_10_Fig1_fixed_V6.tiff",
     units = "mm", width = 600, height = 550,
     res = 500, compression = "lzw")
left_panel <- (
  fig_2_h + fig_2_bai80 + fig_2_sla + fig_2_n +
    guide_area()
) +
  plot_layout(
    ncol = 1,
    guides = "collect",
    heights = c(1, 1, 1, 1, 0.35)
  ) &
  guides(
    alpha = "none",
    color = "none",
    shape = "none"
  )

mid2_panel <- (
  fig_3_h + fig_3_bai80 + fig_3_sla + fig_3_n  +
    guide_area()
) +
  plot_layout(
    ncol = 1,
    guides = "collect",
    heights = c(1, 1, 1, 1, 0.59)
  ) &
  guides(
    alpha = "none",
    color = "none",
    shape = "none"
  )

mid3_panel <- (
  fig_2_chl + fig_2_xc + fig_2_d13c + fig_2_d18o +
    guide_area()
) +
  plot_layout(
    ncol = 1,
    guides = "collect",
    heights = c(1, 1, 1, 1, 0.35)
  ) &
  guides(
    alpha = "none",
    color = guide_legend(nrow = 2),
    shape = "none"
  )

right_panel <- (
  fig_3_chl + fig_3_xc + fig_3_d13c + fig_3_d18o +
    guide_area()
) +
  plot_layout(
    ncol = 1,
    guides = "collect",
    heights = c(1, 1, 1, 1, 0.59)
  ) &
  guides(
    alpha = "none",
    color = guide_legend(nrow = 3,
                         override.aes = list(alpha = 1, size = 6)), 
    shape = "none"
  )
# 
# final_plot <- left_panel | mid2_panel| mid3_panel | right_panel 

final_plot <- (left_panel | plot_spacer() | 
                 mid2_panel | plot_spacer() | 
                 mid3_panel | plot_spacer() | right_panel) + 
  plot_layout(widths = c(3, -0.1, 3, 0.2, 3, -0.1, 3))


final_plot

dev.off()

