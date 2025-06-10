rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading target data ####

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv", 
                         header = T, sep = ",") %>% select(-X)

# 2.- Removing 2023 data ####
# So I can have in the same column 2022 and 2023 values

clean_target <- clean_target %>% 
  select(-contains("_23"))

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
                                ifelse(clean_target$mean_def_obs < 30,
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
  mutate(sp_id = factor(sp_id))

clean_target <- clean_target %>% filter(!mean_def_obs == 100)

clean_target2 <- clean_target %>% 
  mutate(sp_id = "all")

clean_target <- rbind(clean_target, clean_target2)
clean_target$sp_id <- as.factor(clean_target$sp_id)
clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine", "all"))

# 5.- Boxplots ####

## 5.1.- Height ####

box_height <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = height, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "A") +
  xlab("") + 
  ylab("Tree height (m)") +
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.2.- d.b.h. ####

box_dbh <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = dbh, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "B") +
  ylab(expression(paste("Tree d.b.h. (cm)"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.3.- Age ####

box_age <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = age, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "C") +
  ylab("Age (years)") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.4.- Hegyi index ####

box_hegyi <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = hegyi_index, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "A") +
  ylab("Hegyi index") +
  xlab("") + 
  ylim(0, 75) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.5.- SLA ####

box_sla <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = sla_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "B") +
  ylab(expression(paste("Tree average SLA (cm² g"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.6.- BAI 1980 ####

box_bai80 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = mean_1980, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "A") +
  ylab(expression(paste("Tree mean BAI since \n1980 (mm² year"^"-1", ")"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.7.- Rt 2012 ####

box_rt12 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = Rt12, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "B") +
  ylab(expression(paste("Tree 2012 Resistance"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.8.- Rt 2017 ####

box_rt17 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = Rt17, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "C") +
  ylab(expression(paste("Tree 2017 Resistance"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.9.- Rt 2022 ####

box_rt22 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = Rt22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "D") +
  ylab(expression(paste("Tree 2022 Resistance"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.10.- Rs 2012 ####

box_rs12 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = Rs12, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "E") +
  ylab(expression(paste("Tree 2012 Resilience"))) + 
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.11.- Rs 2017 ####

box_rs17 <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = Rs17, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine", "all"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02",
                               "all" = "#5e5e6d"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea",
                               "All"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "F") +
  ylab(expression(paste("Tree 2017 Resilience"))) + 
  xlab("") + 
  ylim(0, 3.5) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

# 6.- Leaf traits plotting ####

# tiff("04_figures/04_04_Vuln_boxplots.tiff", units = "mm", width = 450, height = 500,
#      res = 800, compression = "lzw")
# box_height + box_hegyi + box_age + box_sla + box_bai80 + box_rt12 +
# box_rt17 + box_rt22 + box_rs12 + box_rs17 + 
#   guide_area() + plot_layout(guides = 'collect', ncol = 3)
# dev.off()

# 7.- Plotting separately ####
# Charlotte prefers it that way 

tiff("04_figures/04_04_Vuln_size_boxplots.tiff", units = "mm", width = 500, height = 100,
     res = 800, compression = "lzw")
box_height + box_dbh + box_age + 
  guide_area() + plot_layout(guides = 'collect', ncol = 4)
dev.off()

tiff("04_figures/04_04_Vuln_traits_boxplots.tiff", units = "mm", width = 400, height = 100,
     res = 800, compression = "lzw")
box_hegyi + box_sla + 
  guide_area() + plot_layout(guides = 'collect', ncol = 4)
dev.off()

tiff("04_figures/04_04_Vuln_growth_boxplots.tiff", units = "mm", width = 400, height = 200,
     res = 800, compression = "lzw")
box_bai80 + box_rt12 +
box_rt17 + box_rt22 + box_rs12 + box_rs17 + 
  guide_area() + plot_layout(guides = 'collect', ncol = 4)
dev.off()