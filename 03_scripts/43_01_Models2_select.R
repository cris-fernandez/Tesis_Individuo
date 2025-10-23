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

model_all <- read.csv("02_clean_data/40_01_models_2way.csv") %>% 
  dplyr::select(-X)

model_all_long <- model_all %>% 
  pivot_longer(cols = -variable,
               names_to = c(".value", "status"),  # .value: parte compartida del nombre
               names_pattern = "(.*)_(cold|hot)")

model_all_long$status <- as.factor(model_all_long$status)
model_all_long$sp_id <- "all"

model_aa <- read.csv("02_clean_data/40_02_models_2way_aa.csv") %>% 
  dplyr::select(-X)

model_aa_long <- model_aa %>% 
  pivot_longer(cols = -variable,
               names_to = c(".value", "status"),  # .value: parte compartida del nombre
               names_pattern = "(.*)_(cold|hot)")

model_aa_long$status <- as.factor(model_aa_long$status)
model_aa_long$sp_id <- "Abialba"

model_ps <- read.csv("02_clean_data/40_03_models_2way_ps.csv") %>% 
  dplyr::select(-X)

model_ps_long <- model_ps %>% 
  pivot_longer(cols = -variable,
               names_to = c(".value", "status"),  # .value: parte compartida del nombre
               names_pattern = "(.*)_(cold|hot)")

model_ps_long$status <- as.factor(model_ps_long$status)
model_ps_long$sp_id <- "Pinsylv"

model_pp <- read.csv("02_clean_data/40_04_models_2way_pp.csv") %>% 
  dplyr::select(-X)

model_pp_long <- model_pp %>% 
  pivot_longer(cols = -variable,
               names_to = c(".value", "status"),  # .value: parte compartida del nombre
               names_pattern = "(.*)_(cold|hot)")

model_pp_long$status <- as.factor(model_pp_long$status)
model_pp_long$sp_id <- "Pinpine"

model_df_long <- do.call("rbind", list(model_all_long, model_aa_long, 
                                       model_ps_long, model_pp_long))

model_df_long$sp_id <- factor(model_df_long$sp_id, 
                              levels = c("all", "Abialba", "Pinsylv", "Pinpine"))

model_df_long <- model_df_long %>%
  group_by(variable, sp_id) %>%
  mutate(p_val = p_val[status == "hot"][1]) %>%
  ungroup()

model_df_long <- model_df_long %>% 
  mutate(significant = ifelse(p_val < 0.05, "yes", "no"))

# 2.-Morphological variables ####
## 2.1.- N ####

model_df2 <- model_df_long %>% filter(variable == "percent_n")

n_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  labs(tag = "A") +
  ylab("N content (%)") + 
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

## 2.2.- Age ####

model_df2 <- model_df_long %>% filter(variable == "age")

age_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  labs(tag = "B") +
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

## 2.3.- Hegyi Index ####

model_df2 <- model_df_long %>% filter(variable == "hegyi_index")

hegyi_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  labs(tag = "C") +
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  labs(tag = "D") +
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  labs(tag = "E") +
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

## 3.3.- Carotenoids ####

model_df2 <- model_df_long %>% filter(variable == "xc_fw_22")

xc_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  labs(tag = "F") +
  ylab(expression(paste("Caroten. (μg g"^"-1", ")"))) +
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "G") +
  ylab(bquote("δ"~N^15~"(‰)")) +
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

# 4.- Whole-tree variables ####
## 4.1.- BAI 1980 ####

model_df2 <- model_df_long %>% filter(variable == "mean_1980")

bai80_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, alpha = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, alpha = significant, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "H") +
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "I") +
  ylab(expression(paste("BAI05 (mm² year"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() + 
  theme(legend.position = "right",
        legend.key.size = unit(2, "cm"),  
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.text.y = element_text(size = 22),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 25),
        plot.tag = element_text(size = 25))

# 5.- Plotting ####

tiff("04_figures/43_01_Model2_select.tiff", units = "mm", width = 530, height = 400,
     res = 400, compression = "lzw")
n_plot + age_plot + hegyi_plot + plot_spacer() + 
  wc_plot + chl_plot + xc_plot + d15n_plot + 
  bai80_plot + bai80_plot + guide_area() + plot_spacer() + 
  plot_layout(ncol = 4, guides = "collect")
dev.off()