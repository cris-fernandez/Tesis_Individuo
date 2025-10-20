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

all_count <- model_df_long %>% filter(sp_id == "all") %>% 
  mutate(significant = as.factor(significant))
summary(all_count)

# 2.-Morphological variables ####
## 2.1.- Height ####

model_df2 <- model_df_long %>% filter(variable == "height")
h_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                    values = c("cold" = "#2274A5",
                               "hot" = "#D71515"),
                    labels = c("Non-declining site",
                               "Declining site"),
                    name = "") + 
  labs(tag = "A") +
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
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
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
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

## 2.3.- C ####

model_df2 <- model_df_long %>% filter(variable == "percent_c")

c_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "C") +
  ylab("C content (%)") + 
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

## 2.4.- N ####

model_df2 <- model_df_long %>% filter(variable == "percent_n")

n_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "D") +
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

## 2.5.- C:N ####

model_df2 <- model_df_long %>% filter(variable == "cn")

cn_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "E") +
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

## 2.6.- SLA ####

model_df2 <- model_df_long %>% filter(variable == "sla_22")

sla_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "F") +
  ylab(expression(paste("SLA (cm² g"^"-1", ")"))) + 
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

## 2.7.- Age ####

model_df2 <- model_df_long %>% filter(variable == "age")

age_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "G") +
  ylab("Age (years)") + 
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

## 2.8.- Hegyi Index ####

model_df2 <- model_df_long %>% filter(variable == "hegyi_index")

hegyi_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "H") +
  ylab("Hegyi Index") + 
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

# 3.- Physiological variables ####

## 3.1.- LWC ####

model_df2 <- model_df_long %>% filter(variable == "wc_22")

wc_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "A") +
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
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "B") +
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

## 3.3.- Chl. a/b ####

model_df2 <- model_df_long %>% filter(variable == "chla_chlb_22")

chlab_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "C") +
  ylab("Chl. a/b") + 
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

## 3.4.- Carotenoids ####

model_df2 <- model_df_long %>% filter(variable == "xc_fw_22")

xc_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "D") +
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

## 3.5.- Chl. / xc ####

model_df2 <- model_df_long %>% filter(variable == "chl_xc_22")

chlxc_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "E") +
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

## 3.6.- d13C ####

model_df2 <- model_df_long %>% filter(variable == "leaf_d13c")

d13c_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "F") +
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

## 3.7.- d15N ####

model_df2 <- model_df_long %>% filter(variable == "leaf_d15n")

d15n_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
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

## 3.8.- d18O ####

model_df2 <- model_df_long %>% filter(variable == "leaf_d18o_corrected")

d18o_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "H") +
  ylab(bquote("δ"~O^18~"(‰)")) +
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

# 4.- Whole-tree variables ####

## 4.1.- BAI 1980 ####

model_df2 <- model_df_long %>% filter(variable == "mean_1980")

bai80_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "A") +
  ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + 
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

## 4.2.- BAI05 ####

model_df2 <- model_df_long %>% filter(variable == "mean_05")

bai05_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "B") +
  ylab(expression(paste("BAI05 (mm² year"^"-1", ")"))) + 
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

## 4.3.- Rt12 ####

model_df2 <- model_df_long %>% filter(variable == "Rt12")

rt12_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "C") +
  ylab("Rt 2012") + 
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

## 4.4.- Rt17 ####

model_df2 <- model_df_long %>% filter(variable == "Rt17")

rt17_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "D") +
  ylab("Rt 2017") + 
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

## 4.5.- Rt22 ####

model_df2 <- model_df_long %>% filter(variable == "Rt22")

rt22_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "E") +
  ylab("Rt 2022") + 
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

## 4.6.- Rs12 ####

model_df2 <- model_df_long %>% filter(variable == "Rs12")

rs12_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  labs(tag = "F") +
  ylab("Rs 2012") + 
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

## 4.7.- Rs17 ####

model_df2 <- model_df_long %>% filter(variable == "Rs17")

rs17_plot <- ggplot(model_df2) +
  geom_point(aes(x = sp_id, y = estimate, col = status, shape = significant), position = position_dodge(width = 0.5), size = 4.5) +
  geom_linerange(aes(x = sp_id, ymin = ci_lower, ymax = ci_upper, 
                     col = status), position = position_dodge(width = 0.5), size = 1.5) +
  scale_color_manual(breaks = c("cold", "hot"),
                     values = c("cold" = "#2274A5",
                                "hot" = "#D71515"),
                     labels = c("Non-declining site",
                                "Declining site"),
                     name = "") + 
  scale_shape_manual(values = c("yes" = 20, "no" = 1)) + 
  scale_x_discrete(labels=c("all" = "All", 
                            "Abialba" = "Aa",
                            "Pinsylv" = "Ps",
                            "Pinpine" = "Pp")) + 
  labs(tag = "G") +
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

tiff("04_figures/41_01_Model2_all_morpho2.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
h_plot + dbh_plot + c_plot + n_plot + cn_plot + sla_plot + age_plot + hegyi_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

tiff("04_figures/41_02_Model2_all_physio2.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
wc_plot + chl_plot + chlab_plot + xc_plot + chlxc_plot + d13c_plot + d15n_plot + d18o_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

tiff("04_figures/41_03_Model2_all_whole2.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
bai80_plot + bai05_plot + rt12_plot + rt17_plot + rt22_plot + rs12_plot + rs17_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()