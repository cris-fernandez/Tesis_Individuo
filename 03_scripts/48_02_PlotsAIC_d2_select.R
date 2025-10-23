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

model_df <- read.csv("02_clean_data/46_09_Models2_discrete_aic.csv") %>% 
  mutate(significant = ifelse(delta_aic > 1.999, "yes", "no"),
         estimate_hot = estimate_hot + estimate_cold)

model_df_long <- model_df %>%
  pivot_longer(cols = ends_with("_cold") | ends_with("_hot"),
               names_to = c(".value", "status"),
               names_pattern = "(.*)_(cold|hot)") %>% 
  mutate(sp_id = factor(sp_id, levels = c("aa", "ps", "pp")))

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

## 2.2.- C:N ####

model_df2 <- model_df_long %>% filter(variable == "cn")

cn_plot <- ggplot(model_df2) +
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
  ylab("C:N ratio") + 
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "aa" = "Aa",
                            "ps" = "Ps",
                            "pp" = "Pp")) + 
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "aa" = "Aa",
                            "ps" = "Ps",
                            "pp" = "Pp")) + 
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
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
  labs(tag = "G") +
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
                            "aa" = "Aa",
                            "ps" = "Ps",
                            "pp" = "Pp")) + 
  labs(tag = "H") +
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
  scale_x_discrete(labels=c("all" = "All", 
                            "aa" = "Aa",
                            "ps" = "Ps",
                            "pp" = "Pp")) + 
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
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_x_discrete(labels=c("all" = "All", 
                            "aa" = "Aa",
                            "ps" = "Ps",
                            "pp" = "Pp")) + 
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

# 8.- Plotting ####

tiff("04_figures/48_02_PlotsAIC_d2_select.tiff", units = "mm", width = 530, height = 400,
     res = 400, compression = "lzw")
n_plot + cn_plot + age_plot + hegyi_plot + 
  wc_plot + chl_plot + xc_plot + d15n_plot + 
  bai80_plot + bai05_plot +  guide_area() + plot_spacer() + plot_layout(ncol = 3, guides = "collect")
dev.off()