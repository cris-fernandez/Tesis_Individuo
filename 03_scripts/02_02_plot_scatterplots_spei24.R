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
                         header = T, sep = ",") %>% select(-X) %>% 
  mutate(site = substr(plot_id, 1, 3))

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
                                ifelse(clean_target$prec < 30,
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

clean_target$sp_id <- fct_relevel(clean_target$sp_id, "Abialba", "Pinsylv", "Pinpine")

# 5.- Reading SPEI data ####

spei <- read.csv("02_clean_data/02_00_spei_series.csv") %>% 
  select(-X) %>% filter(month == 7)

spei24 <- spei %>% 
  select(c(plot_id, year, spei24)) %>% 
  group_by(plot_id) %>% 
  summarise(mean_spei24 = mean(spei24, na.rm = T))

clean_target <- full_join(clean_target, spei24, by = "plot_id")

clean_target <- clean_target %>% filter(!sp_id == "Pinpine") %>% 
  filter(!is.na(sp_id))

# In order to separate hot and coldspots per species, a new id is required.
# This id will allow the points to have different colours, as using different 
# alpha values does not provide satisfying results...

clean_target$sp_status <- paste(clean_target$sp_id, clean_target$spot_status, sep = "_")
clean_target$sp_status <- fct_relevel(clean_target$sp_status, 
                                      "Abialba_coldspot", "Abialba_hotspot",
                                      "Pinsylv_coldspot", "Pinsylv_hotspot")

# 4.- Leaf traits scatterplots ####

# Scatterplots will be grouped in leaf variables and dendro variables,
# as they will be analysed separately

# y variable in leaf traits will be defoliation

## 4.1.- SPEI24 ~ height ####

spei24_height <- ggplot(clean_target) + 
  geom_point(aes(x = mean_spei24, y = height, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(x = mean_spei24, y = height, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  guides(fill = "none") + 
  labs(tag = "A") +
  ylab("Tree height (m)") +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.2.- SPEI24 ~ dbh ####

spei24_dbh <- ggplot(clean_target) + 
  geom_point(aes(y = dbh, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = dbh, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "B") +
  ylab("Tree d.b.h. (cm)") +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.3.- SPEI24 ~ Hegyi ####

spei24_hegyi <- ggplot(clean_target) + 
  geom_point(aes(y = hegyi_index, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = hegyi_index, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "C") +
  ylim(0, 75) + 
  ylab("Hegyi index") +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.4.- SPEI24 ~ C ####

spei24_c <- ggplot(clean_target) + 
  geom_point(aes(y = percent_c, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = percent_c, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "D") +
  ylab(expression(paste("Leaf C content (%)"))) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.5.- SPEI24 ~ N ####

spei24_n <- ggplot(clean_target) + 
  geom_point(aes(y = percent_n, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = percent_n, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "E") +
  ylab(expression(paste("Leaf N content (%)"))) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  ylim(0, 4) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.6.- SPEI24 ~ dC13 ####

spei24_d13c <- ggplot(clean_target) + 
  geom_point(aes(y = d13c, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = d13c, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "F") +
  ylab(bquote("Leaves δ"~C^13~"(‰)")) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.7.- SPEI24 ~ dN15 ####

spei24_d15n <- ggplot(clean_target) + 
  geom_point(aes(y = d15n, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = d15n, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "G") +
  ylab(bquote("Leaves δ"~N^15~"(‰)")) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.8.- SPEI24 ~ dO18 ####

spei24_d18o <- ggplot(clean_target) + 
  geom_point(aes(y = d18o, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = d18o, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "H") +
  ylab(bquote("Leaves δ"~O^18~"(‰)")) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.9.- SPEI24 ~ water content ####

spei24_wc <- ggplot(clean_target) + 
  geom_point(aes(y = wc_22, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = wc_22, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "I") +
  ylab(expression(paste("Leaf water content (%)"))) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  ylim(35, 80) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.10.- SPEI24 ~ total chl ####

spei24_chl_fw <- ggplot(clean_target) + 
  geom_point(aes(y = total_chl_fw_22, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = total_chl_fw_22, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "J") +
  ylab(expression(paste("Leaf chlorophyll content (μg g"^"-1", ")"))) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  ylim(250, 2500) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 


## 4.11.- SPEI24 ~ carotenoids ####

spei24_xc_fw <- ggplot(clean_target) + 
  geom_point(aes(y = xc_fw_22, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = xc_fw_22, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "K") +
  ylab(expression(paste("Leaf carotenoids content (μg g"^"-1", ")"))) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  ylim(10, 80) + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.24.- SPEI24 ~ chla/b ####

spei24_chl_ab <- ggplot(clean_target) + 
  geom_point(aes(y = chla_chlb_22, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = chla_chlb_22, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "L") +
  ylab(expression(paste("Chlorophyll a/b ratio"))) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  ylim(1.4, 3) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.13.- SPEI24 ~ chl/xc ####

spei24_chl_xc <- ggplot(clean_target) + 
  geom_point(aes(y = chl_xc_22, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = chl_xc_22, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "M") +
  ylab(expression(paste("Chlorophylls/carotenoids ratio"))) +
  xlab(expression(paste("July 24 months-SPEI"))) + 
  ylim(15, 40) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22))

## 4.14.- SPEI24 ~ SLA ####

spei24_sla <- ggplot(clean_target) + 
  geom_point(aes(y = sla_22, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = sla_22, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "N") +
  ylab(expression(paste("Tree average SLA (cm² g"^"-1", ")"))) + 
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22))

## 4.15.- SPEI24 ~ BAI05 ####

spei24_bai05 <- ggplot(clean_target) + 
  geom_point(aes(y = mean_05, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = mean_05, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "O") +
  ylab(expression(paste("Tree average growth 05 (mm² year"^"-1", ")"))) + 
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22))

## 4.16.- SPEI24 ~ BAI10 ####

spei24_bai10 <- ggplot(clean_target) + 
  geom_point(aes(y = mean_10, x = mean_spei24, col = sp_status), 
             position = position_jitter(width = 0.002, height = 0)) + 
  geom_smooth(aes(y = mean_10, x = mean_spei24, col = sp_status, fill = sp_status),
              method = "lm", show.legend = FALSE) + 
  scale_color_manual(values = c("Abialba_coldspot" = "#6863a0",
                                "Abialba_hotspot" = "#aba8d0",
                                "Pinsylv_coldspot" = "#188e6b",
                                "Pinsylv_hotspot" = "#76c4ad"),
                     labels = c("A. alba - Healthy",
                                "A. alba - Damaged",
                                "P. sylvestris - Healthy",
                                "P. sylvestris - Damaged"),
                     name = "") +
  scale_fill_manual(values = c("Abialba_coldspot" = "#6863a0",
                               "Abialba_hotspot" = "#aba8d0",
                               "Pinsylv_coldspot" = "#188e6b",
                               "Pinsylv_hotspot" = "#76c4ad"),
                    labels = c("A. alba - Healthy",
                               "A. alba - Damaged",
                               "P. sylvestris - Healthy",
                               "P. sylvestris - Damaged"),
                    name = "") +
  labs(tag = "P") +
  ylab(expression(paste("Tree average growth 10 (mm² year"^"-1", ")"))) + 
  xlab(expression(paste("July 24 months-SPEI"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22))

# 5.- Leaf traits plotting ####

tiff("04_figures/04_02_spei24_leaf_scatter_sp.tiff", units = "mm", width = 450, height = 400,
     res = 800, compression = "lzw")
spei24_height + spei24_dbh + spei24_hegyi + spei24_c + 
  spei24_n + spei24_d13c + spei24_d15n + spei24_d18o +  
  spei24_wc + spei24_chl_fw + spei24_xc_fw + spei24_chl_ab +  
  spei24_chl_xc + spei24_sla + spei24_bai05 + spei24_bai10 +
  plot_layout(guides = 'collect', ncol = 4)
dev.off()