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

# 2.- Variable tidying ####

ci_df_2$spot_status <- factor(ci_df_2$spot_status, levels = c("coldspot", "hotspot"))
ci_df_2$sp_id <- factor(ci_df_2$sp_id, levels = c("Abialba", "Pinsylv", "Pinpine"))

ci_df_3$vigor_id <- factor(ci_df_3$vigor_id, levels = c("hot_healthy", "hot_damaged"))
ci_df_3$sp_id <- factor(ci_df_3$sp_id, levels = c("Abialba", "Pinsylv", "Pinpine"))

# 3.- Plot level ####
# 3.1.- Height ####
h_df <- ci_df_2 %>% filter(variable == "height")

fig_2_h <- ggplot(h_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
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
  ylim(10, 35) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 3.2.- SLA ####

sla_df <- ci_df_2 %>% filter(variable == "sla_22")

fig_2_sla <- ggplot(sla_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
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
  ylim(39, 61) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 3.3.- N ####

n_df <- ci_df_2 %>% filter(variable == "percent_n")

fig_2_n <- ggplot(n_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
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
  ylab("N content (%)") + 
  ylim(0.8, 2) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 3.4.- Chl. ####

chl_df <- ci_df_2 %>% filter(variable == "total_chl_fw_22")

fig_2_chl <- ggplot(chl_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
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
  ylim(500, 1700) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 3.5.- Car. ####

xc_df <- ci_df_2 %>% filter(variable == "xc_fw_22")

fig_2_xc <- ggplot(xc_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
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
  ylim(20, 60) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 3.6.- d13C ####

d13c_df <- ci_df_2 %>% filter(variable == "leaf_d13c")

fig_2_d13c <- ggplot(d13c_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
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
  ylim(-30, -23.5) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 3.7.- BAI80 ####

bai80_df <- ci_df_2 %>% filter(variable == "mean_1980")

fig_2_bai80 <- ggplot(bai80_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = spot_status,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  scale_x_discrete(expand = c(0.3, 0.3)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
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
  ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + 
  ylim(350, 2700) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 25, vjust = 1, angle = 45, hjust = 1,
                                   face = "italic"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.- Plotting ####
# 4.1.- Height ####
h_df <- ci_df_3 %>% filter(variable == "height")

fig_3_h <- ggplot(h_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("") +  
  ylim(10, 35) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.2.- SLA ####

sla_df <- ci_df_3 %>% filter(variable == "sla_22")

fig_3_sla <- ggplot(sla_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("") +
  ylim(39, 61) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.3.- N ####

n_df <- ci_df_3 %>% filter(variable == "percent_n")

fig_3_n <- ggplot(n_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("") + 
  ylim(0.8, 2) +
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.4.- Chl. ####

chl_df <- ci_df_3 %>% filter(variable == "total_chl_fw_22")

fig_3_chl <- ggplot(chl_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("") + 
  ylim(500, 1700) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.5.- Car. ####

xc_df <- ci_df_3 %>% filter(variable == "xc_fw_22")

fig_3_xc <- ggplot(xc_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("") +
  ylim(20, 60) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.6.- d13C ####

d13c_df <- ci_df_3 %>% filter(variable == "leaf_d13c")

fig_3_d13c <- ggplot(d13c_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") +  
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("") + 
  ylim(-30, -23.5) + 
  theme_classic() + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")

# 4.7.- BAI80 ####

bai80_df <- ci_df_3 %>% filter(variable == "mean_1980")

fig_3_bai80 <- ggplot(bai80_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 4.5, 
             position = position_dodge(width = 0.23)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 2, width = 0,
                position = position_dodge(width = 0.23)) +
  scale_x_discrete(expand = c(0.3, 0.3)) + 
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "A. alba",
                              "Pinsylv" = "P. sylvestris",
                              "Pinpine" = "P. pinea")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.3),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("") + 
  ylim(350, 2700) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 25, vjust = 1, angle = 45, hjust = 1,
                                   face = "italic"),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35),
        legend.direction = "horizontal")


# 5.- Plotting ####

tiff("04_figures/63_05_Fig1_fused.tiff", units = "mm", width = 300, height = 800,
     res = 500, compression = "lzw")
(fig_2_h + fig_2_sla + fig_2_n + fig_2_chl + fig_2_xc + fig_2_d13c + fig_2_bai80 + 
  guide_area() +
  plot_layout(ncol = 1, 
              guides = "collect", 
              heights = c(1, 1, 1, 1, 1, 1, 1, 0.35)) & 
  guides(alpha = "none", color = guide_legend(nrow = 2))  &
  theme(legend.position = "bottom")) | (fig_3_h + fig_3_sla + fig_3_n + fig_3_chl + fig_3_xc + fig_3_d13c + fig_3_bai80 + 
     guide_area() +
     plot_layout(ncol = 1, 
                 guides = "collect", 
                 heights = c(1, 1, 1, 1, 1, 1, 1, 0.35)) & 
     guides(alpha = "none", color = guide_legend(nrow = 2))  &
     theme(legend.position = "bottom"))

dev.off()

# tiff("04_figures/63_05_Fig1_fused.tiff", units = "mm", width = 300, height = 800,
#      res = 500, compression = "lzw")
# fig_2_h + fig_3_h + 
#   fig_2_sla + fig_3_sla + 
#   fig_2_n + fig_3_n + 
#   fig_2_chl + fig_3_chl +
#   fig_2_xc + fig_3_xc +  
#   fig_2_d13c + fig_3_d13c + 
#   fig_2_bai80 + fig_3_bai80 + 
#   guide_area() +
#   plot_layout(ncol = 2, 
#               guides = "collect", 
#               heights = c(1, 1, 1, 1, 1, 1, 1, 0.35)) & 
#   guides(alpha = "none")
# 
# dev.off()