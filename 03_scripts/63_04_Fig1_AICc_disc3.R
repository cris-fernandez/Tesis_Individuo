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

ci_df <- read.csv("02_clean_data/63_02_AICc_discrete3.csv", 
                  header = T, sep = ",") %>% dplyr::select(-X)

# 2.- Variable tidying ####

ci_df$vigor_id <- factor(ci_df$vigor_id, levels = c("hot_healthy", "hot_damaged"))
ci_df$sp_id <- factor(ci_df$sp_id, levels = c("Abialba", "Pinsylv", "Pinpine"))

# 3.- Plotting ####
# 3.1.- Height ####
h_df <- ci_df %>% filter(variable == "height")

fig_h <- ggplot(h_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "Aa",
                              "Pinsylv" = "Ps",
                              "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.4),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("Height (m)") +  
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35))

# 3.2.- SLA ####

sla_df <- ci_df %>% filter(variable == "sla_22")

fig_sla <- ggplot(sla_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "Aa",
                              "Pinsylv" = "Ps",
                              "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.4),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(expression(paste("SLA (cm² g"^"-1", ")"))) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35))

# 3.3.- N ####

n_df <- ci_df %>% filter(variable == "percent_n")

fig_n <- ggplot(n_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "Aa",
                              "Pinsylv" = "Ps",
                              "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.4),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab("N content (%)") + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35))

# 3.4.- Chl. ####

chl_df <- ci_df %>% filter(variable == "total_chl_fw_22")

fig_chl <- ggplot(chl_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "Aa",
                              "Pinsylv" = "Ps",
                              "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.4),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(expression(paste("Chl. (μg g"^"-1", ")"))) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35))

# 3.5.- Car. ####

xc_df <- ci_df %>% filter(variable == "xc_fw_22")

fig_xc <- ggplot(xc_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "Aa",
                              "Pinsylv" = "Ps",
                              "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.4),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(expression(paste("Car. (μg g"^"-1", ")"))) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35))

# 3.6.- d13C ####

d13c_df <- ci_df %>% filter(variable == "leaf_d13c")

fig_d13c <- ggplot(d13c_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "Aa",
                              "Pinsylv" = "Ps",
                              "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.4),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(bquote("δ"~C^13~"(‰)")) +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35))

# 3.7.- BAI80 ####

bai80_df <- ci_df %>% filter(variable == "mean_1980")

fig_bai80 <- ggplot(bai80_df) + 
  geom_point(aes(x = sp_id, y = emmean, colour = vigor_id,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = sp_id, ymin = lower.CL, ymax = upper.CL, 
                    colour = vigor_id, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("hot_healthy", "hot_damaged"),
                     values = c("hot_healthy" = "#D71515",
                                "hot_damaged" = "#650304"),
                     labels = c("Healthy",
                                "Damaged"),
                     name = "") + 
  scale_x_discrete(labels = c("Abialba" = "Aa",
                              "Pinsylv" = "Ps",
                              "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1,
                                "no" = 0.4),
                     name = "",
                     labels = "") + 
  xlab("") + 
  ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 35))

# 4.- Plotting ####

tiff("04_figures/63_04_Fig1_d3.tiff", units = "mm", width = 320, height = 800,
     res = 400, compression = "lzw")
fig_h / fig_sla / fig_n / fig_chl / fig_xc / fig_d13c / fig_bai80 / guide_area() +
  plot_layout(guides = "collect") & guides(alpha = "none")

dev.off()