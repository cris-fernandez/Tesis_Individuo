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
  mutate(estimate_hot = estimate_hot + estimate_cold) %>% 
  dplyr::select(-c(delta_aic, X))

aic_cont <- read.csv("02_clean_data/47_05_Models_continuous_aic.csv") %>% 
  dplyr::select(-X) %>% 
  rename(aa = delta_aic_aa,
         ps = delta_aic_ps,
         pp = delta_aic_pp) %>% 
  pivot_longer(cols = 2:4, names_to = "sp_id", values_to = "delta_aic") %>% 
  mutate(sp_id = factor(sp_id, levels = c("aa", "ps", "pp")))

model_df2 <- full_join(model_df, aic_cont, by = c("variable", "sp_id"))

model_df_long <- model_df2 %>%
  pivot_longer(cols = ends_with("_cold") | ends_with("_hot"),
               names_to = c(".value", "status"),
               names_pattern = "(.*)_(cold|hot)") %>% 
  mutate(sp_id = factor(sp_id, levels = c("aa", "ps", "pp")),
         significant = ifelse(delta_aic > 1.8, "yes", "no"))

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

## 3.4.- d13C ####

model_df2 <- model_df_long %>% filter(variable == "leaf_d13c")

d13c_plot <- ggplot(model_df2) +
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
  ylab(bquote("δ"~C^13~"(‰)")) +
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

## 3.5.- d15N ####

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

## 4.3.- Rt12 ####

model_df2 <- model_df_long %>% filter(variable == "Rt12")

rt12_plot <- ggplot(model_df2) +
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

## 4.4.- Rt17 ####

model_df2 <- model_df_long %>% filter(variable == "Rt17")

rt17_plot <- ggplot(model_df2) +
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
  labs(tag = "L") +
  ylab("Rt 2017") + 
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

## 4.5.- Rt22 ####

model_df2 <- model_df_long %>% filter(variable == "Rt22")

rt22_plot <- ggplot(model_df2) +
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
  labs(tag = "M") +
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

## 4.6.- Rs12 ####

model_df2 <- model_df_long %>% filter(variable == "Rs12")

rs12_plot <- ggplot(model_df2) +
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
  labs(tag = "N") +
  ylab("Rs 2012") + 
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

## 4.7.- Rs17 ####

model_df2 <- model_df_long %>% filter(variable == "Rs17")

rs17_plot <- ggplot(model_df2) +
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
  labs(tag = "O") +
  ylab("Rs 2017") + 
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

tiff("04_figures/50_02_PlotsAIC_cont_select.tiff", units = "mm", width = 950, height = 400,
     res = 400, compression = "lzw")
n_plot + cn_plot +  age_plot + plot_spacer() + plot_spacer() + plot_spacer() + guide_area() + 
  wc_plot + chl_plot + xc_plot + d13c_plot + d15n_plot + plot_spacer() + plot_spacer() +
  bai80_plot + bai05_plot + rt12_plot + rt17_plot + rt22_plot + rs12_plot + rs17_plot + 
   plot_layout(ncol = 7, guides = "collect")
dev.off()