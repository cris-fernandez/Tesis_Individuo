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

# 5.- Reading climate data ####

climate <- read.csv("02_clean_data/02_00_climate_series.csv") %>% 
  select(-X) %>% 
  filter(year > 2011)

prcp <- climate %>% 
  select(c(site, year, Prcp)) %>% 
  group_by(site) %>% 
  summarise(prec = mean(Prcp))

clean_target <- full_join(clean_target, prcp, by = "site")

# 4.- Leaf traits scatterplots ####

# Scatterplots will be grouped in leaf variables and dendro variables,
# as they will be analysed separately

# y variable in leaf traits will be defoliation

## 4.1.- MAP10 ~ height ####

prcp_height <- ggplot(clean_target) + 
  geom_point(aes(x = prec, y = log(height), col = sp_id)) + 
  geom_smooth(aes(x = prec, y = log(height), col = sp_id, fill = sp_id),
              method = "loess") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "A") +
  ylab("Tree height (m)") +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.2.- MAP10 ~ dbh ####

prcp_dbh <- ggplot(clean_target) + 
  geom_point(aes(y = log(dbh), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(dbh), x = prec, col = sp_id, fill = sp_id),
              method = "loess") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "B") +
  ylab("Tree d.b.h. (cm)") +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.3.- MAP10 ~ Hegyi ####

prcp_hegyi <- ggplot(clean_target) + 
  geom_point(aes(y = log(hegyi_index), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(hegyi_index), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "C") +
  ylab("Hegyi index") +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.4.- MAP10 ~ C ####

prcp_c <- ggplot(clean_target) + 
  geom_point(aes(y = log(percent_c), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(percent_c), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "D") +
  ylab(expression(paste("Leaf C content (%)"))) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.5.- MAP10 ~ N ####

prcp_n <- ggplot(clean_target) + 
  geom_point(aes(y = log(percent_n), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(percent_n), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "E") +
  ylab(expression(paste("Leaf N content (%)"))) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.6.- MAP10 ~ dC13 ####

prcp_d13c <- ggplot(clean_target) + 
  geom_point(aes(y = log(-1*d13c), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(-1*d13c), x = prec, col = sp_id, fill = sp_id),
              method = "loess") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "F") +
  ylab(bquote("Leaves δ"~C^13~"(‰)")) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.7.- MAP10 ~ dN15 ####

prcp_d15n <- ggplot(clean_target) + 
  geom_point(aes(y = log(-1*d15n), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(-1*d15n), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "G") +
  ylab(bquote("Leaves δ"~N^15~"(‰)")) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.8.- MAP10 ~ dO18 ####

prcp_d18o <- ggplot(clean_target) + 
  geom_point(aes(y = log(d18o), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(d18o), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "H") +
  ylab(bquote("Leaves δ"~O^18~"(‰)")) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.9.- MAP10 ~ water content ####

prcp_wc <- ggplot(clean_target) + 
  geom_point(aes(y = log(wc_22), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(wc_22), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "I") +
  ylab(expression(paste("Leaf water content (%)"))) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.10.- MAP10 ~ total chl ####

prcp_chl_fw <- ggplot(clean_target) + 
  geom_point(aes(y = log(total_chl_fw_22), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(total_chl_fw_22), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "J") +
  ylab(expression(paste("Leaf chlorophyll content (μg g"^"-1", ")"))) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 


## 4.11.- MAP10 ~ carotenoids ####

prcp_xc_fw <- ggplot(clean_target) + 
  geom_point(aes(y = log(xc_fw_22), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(xc_fw_22), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "K") +
  ylab(expression(paste("Leaf carotenoids content (μg g"^"-1", ")"))) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.12.- MAP10 ~ chla/b ####

prcp_chl_ab <- ggplot(clean_target) + 
  geom_point(aes(y = log(chla_chlb_22), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(chla_chlb_22), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "L") +
  ylab(expression(paste("Chlorophyll a/b ratio"))) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.13.- MAP10 ~ chl/xc ####

prcp_chl_xc <- ggplot(clean_target) + 
  geom_point(aes(y = log(chl_xc_22), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(chl_xc_22), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "M") +
  ylab(expression(paste("Chlorophylls/carotenoids ratio"))) +
  xlab(expression(paste("MAP_10 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22))

## 4.14.- MAP10 ~ SLA ####

prcp_sla <- ggplot(clean_target) + 
  geom_point(aes(y = log(sla_22), x = prec, col = sp_id)) + 
  geom_smooth(aes(y = log(sla_22), x = prec, col = sp_id, fill = sp_id),
              method = "lm") + 
  scale_color_manual(values = c("Abialba" = "#746fb2",
                                "Pinsylv" = "#1b9e77",
                                "Pinpine" = "#db5f02"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_fill_manual(values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "N") +
  ylab(expression(paste("Tree average SLA (cm² g"^"-1", ")"))) + 
  xlab(expression(paste("MAP_10 (mm)"))) + 
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

tiff("04_figures/04_02_prcp10_leaf_scatter_sp_tf.tiff", units = "mm", width = 450, height = 400,
     res = 800, compression = "lzw")
prcp_height + prcp_dbh + prcp_hegyi + prcp_c + 
  prcp_n + prcp_d13c + prcp_d15n + prcp_d18o +  
  prcp_wc + prcp_chl_fw + prcp_xc_fw + prcp_chl_ab +  
  prcp_chl_xc + prcp_sla + 
  plot_layout(guides = 'collect', ncol = 4)
dev.off()