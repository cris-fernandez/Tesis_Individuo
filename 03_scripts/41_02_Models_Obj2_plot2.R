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

model_df <- read.csv("02_clean_data/40_02_models_3way.csv") %>% 
  dplyr::select(-X)

model_df_long <- model_df %>% 
  pivot_longer(cols = -variable,
               names_to = c(".value", "status"),  # .value: parte compartida del nombre
               names_pattern = "(.*)_(healthy|damaged)")

model_df_long$status <- as.factor(model_df_long$status)

# 2.-Morphological variables ####
## 2.1.- Height ####

model_df2 <- model_df_long %>% filter(variable == "height")
h_plot <- ggplot(model_df2) +
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_x_discrete(labels = c("cold" = "Healthy", 
                              "hot" = "Declining")) + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_x_discrete(labels = c("cold" = "Healthy", 
                              "hot" = "Declining")) + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_x_discrete(labels = c("cold" = "Healthy", 
                              "hot" = "Declining")) + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_x_discrete(labels = c("cold" = "Healthy", 
                              "hot" = "Declining")) + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
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
  geom_point(aes(x = status, y = estimate, col = status), size = 3) +
  geom_linerange(aes(x = status, ymin = ci_lower, ymax = ci_upper, 
                     col = status), size = 1.5) +
  scale_color_manual(breaks = c("healthy", "damaged"),
                     values = c("healthy" = "#D71515",
                                "damaged" = "#650304"),
                     labels = c("Healthy trees",
                                "Damaged trees"),
                     name = "") + 
  scale_x_discrete(labels = c("cold" = "Healthy", 
                              "hot" = "Declining")) + 
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

tiff("04_figures/42_01_Model3_all_morpho3.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
h_plot + dbh_plot + c_plot + n_plot + cn_plot + sla_plot + age_plot + hegyi_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

tiff("04_figures/42_02_Model3_all_physio3.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
wc_plot + chl_plot + chlab_plot + xc_plot + chlxc_plot + d13c_plot + d15n_plot + d18o_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

tiff("04_figures/42_03_Model3_all_whole3.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
bai80_plot + bai05_plot + rt12_plot + rt17_plot + rt22_plot + rs12_plot + rs17_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()