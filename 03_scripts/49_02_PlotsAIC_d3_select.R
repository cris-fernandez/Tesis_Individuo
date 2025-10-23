rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "lme4", "lmerTest", "emmeans") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading model outputs ####

model_df <- read.csv("02_clean_data/46_09_Models3_discrete_aic.csv") %>% 
  mutate(significant = ifelse(delta_aic > 1.999, "yes", "no"),
         estimate_damaged = estimate_damaged + estimate_healthy)

model_df_long <- model_df %>%
  pivot_longer(cols = ends_with("_damaged") | ends_with("_healthy"),
               names_to = c(".value", "status"),
               names_pattern = "(.*)_(healthy|damaged)") %>% 
  mutate(sp_id = factor(sp_id, levels = c("aa", "ps", "pp")))

# 2.-Morphological variables ####
## 2.1.- Height ####

model_df2 <- model_df_long %>% filter(variable == "height")
h_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  labs(tag = "A") +
  ylab("Height (m)") + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 2.2.- d.b.h. ####

model_df2 <- model_df_long %>% filter(variable == "dbh")

dbh_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "B") +
  ylab("d.b.h. (cm)") + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 2.3.- Age ####

model_df2 <- model_df_long %>% filter(variable == "age")

age_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  labs(tag = "C") +
  ylab("Age (years)") + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 2.4.- Hegyi Index ####

model_df2 <- model_df_long %>% filter(variable == "hegyi_index")

hegyi_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "D") +
  ylab("Hegyi Index") + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

# 3.- Physiological variables ####

## 3.1.- LWC ####

model_df2 <- model_df_long %>% filter(variable == "wc_22")

wc_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "E") +
  ylab("LWC (%)") + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 3.2.- Chl. ####

model_df2 <- model_df_long %>% filter(variable == "total_chl_fw_22")

chl_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "F") +
  ylab(expression(paste("Chl. (μg g"^"-1", ")"))) +
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 3.3.- Chl. / xc ####

model_df2 <- model_df_long %>% filter(variable == "chl_xc_22")

chlxc_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "G") +
  ylab("Chl. / car.") + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

## 3.4.- d15N ####

model_df2 <- model_df_long %>% filter(variable == "leaf_d15n")

d15n_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  labs(tag = "H") +
  ylab(bquote("δ"~N^15~"(‰)")) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 12),
        plot.tag = element_text(size = 25))

# 4.- Whole-tree variables ####

## 4.1.- BAI 1980 ####

model_df2 <- model_df_long %>% filter(variable == "mean_1980")

bai80_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "I") +
  ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

## 4.2.- BAI05 ####

model_df2 <- model_df_long %>% filter(variable == "mean_05")

bai05_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "J") +
  ylab(expression(paste("BAI05 (mm² year"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

## 4.3.- Rt12 ####

model_df2 <- model_df_long %>% filter(variable == "Rt12")

rt12_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "K") +
  ylab("Rt 2012") + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

## 4.4.- Rt22 ####

model_df2 <- model_df_long %>% filter(variable == "Rt22")

rt22_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) +  
  labs(tag = "L") +
  ylab("Rt 2022") + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 8.- Plotting ####

tiff("04_figures/49_02_PlotsAIC_d3_select.tiff", units = "mm", width = 530, height = 400,
     res = 400, compression = "lzw")
h_plot + dbh_plot + age_plot + hegyi_plot + 
  wc_plot + chl_plot + chlxc_plot + d15n_plot + 
  bai80_plot + bai05_plot + rt12_plot + rt22_plot + 
  plot_layout(ncol = 3, guides = "collect")
dev.off()