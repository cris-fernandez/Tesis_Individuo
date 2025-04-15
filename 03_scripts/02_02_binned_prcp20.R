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
  filter(year > 2001)

prcp <- climate %>% 
  select(c(site, year, Prcp)) %>% 
  group_by(site) %>% 
  summarise(prec = mean(Prcp))

clean_target <- full_join(clean_target, prcp, by = "site")

## 4.1.- MAP20 ~ height ####

clean_target2 <- clean_target %>%
  filter(!is.na(height)) %>% 
  group_by(sp_id) %>%
  mutate(height_cat = cut(height,
                          breaks = quantile(height, 
                                            probs = c(0, 1/3, 2/3, 1), 
                                            na.rm = T),
                          include.lowest = T,
                          labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_height <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = height_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "A") +
  xlab("Tree height (m)") +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.2.- MAP20 ~ dbh ####

clean_target2 <- clean_target %>%
  filter(!is.na(dbh)) %>% 
  group_by(sp_id) %>%
  mutate(dbh_cat = cut(dbh,
                       breaks = quantile(dbh, 
                                         probs = c(0, 1/3, 2/3, 1), 
                                         na.rm = T),
                       include.lowest = T,
                       labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_dbh <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = dbh_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "B") +
  xlab("Tree d.b.h. (cm)") +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.3.- MAP20 ~ Hegyi ####

clean_target2 <- clean_target %>%
  filter(!is.na(hegyi_index)) %>% 
  group_by(sp_id) %>%
  mutate(hegyi_cat = cut(hegyi_index,
                         breaks = quantile(hegyi_index, 
                                           probs = c(0, 1/3, 2/3, 1), 
                                           na.rm = T),
                         include.lowest = T,
                         labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_hegyi <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = hegyi_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "C") +
  xlab("Hegyi index") +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.4.- MAP20 ~ C ####

clean_target2 <- clean_target %>%
  filter(!is.na(percent_c)) %>% 
  group_by(sp_id) %>%
  mutate(c_cat = cut(percent_c,
                     breaks = quantile(percent_c, 
                                       probs = c(0, 1/3, 2/3, 1), 
                                       na.rm = T),
                     include.lowest = T,
                     labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_c <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = c_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "D") +
  xlab(expression(paste("Leaf C content (%)"))) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.5.- MAP20 ~ N ####

clean_target2 <- clean_target %>%
  filter(!is.na(percent_n)) %>% 
  group_by(sp_id) %>%
  mutate(n_cat = cut(percent_n,
                     breaks = quantile(percent_n, 
                                       probs = c(0, 1/3, 2/3, 1), 
                                       na.rm = T),
                     include.lowest = T,
                     labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_n <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = n_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "E") +
  xlab(expression(paste("Leaf N content (%)"))) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.6.- MAP20 ~ dC13 ####

clean_target2 <- clean_target %>%
  filter(!is.na(d13c)) %>% 
  group_by(sp_id) %>%
  mutate(d13c_cat = cut(d13c,
                        breaks = quantile(d13c, 
                                          probs = c(0, 1/3, 2/3, 1), 
                                          na.rm = T),
                        include.lowest = T,
                        labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_d13c <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = d13c_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "F") +
  xlab(bquote("Leaves δ"~C^13~"(‰)")) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.7.- MAP20 ~ dN15 ####

clean_target2 <- clean_target %>%
  filter(!is.na(d15n)) %>% 
  group_by(sp_id) %>%
  mutate(d15n_cat = cut(d15n,
                        breaks = quantile(d15n, 
                                          probs = c(0, 1/3, 2/3, 1), 
                                          na.rm = T),
                        include.lowest = T,
                        labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_d15n <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = d15n_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "G") +
  xlab(bquote("Leaves δ"~N^15~"(‰)")) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.8.- MAP20 ~ dO18 ####

clean_target2 <- clean_target2 %>%
  filter(!is.na(d18o)) %>% 
  group_by(sp_id) %>%
  mutate(d18o_cat = cut(d18o,
                        breaks = quantile(d18o, 
                                          probs = c(0, 1/3, 2/3, 1), 
                                          na.rm = T),
                        include.lowest = T,
                        labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_d18o <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = d18o_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "H") +
  xlab(bquote("Leaves δ"~O^18~"(‰)")) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.9.- MAP20 ~ water content ####

clean_target2 <- clean_target2 %>%
  filter(!is.na(wc_22)) %>% 
  group_by(sp_id) %>%
  mutate(wc_cat = cut(wc_22,
                      breaks = quantile(wc_22, 
                                        probs = c(0, 1/3, 2/3, 1), 
                                        na.rm = T),
                      include.lowest = T,
                      labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_wc <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = wc_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "I") +
  xlab(expression(paste("Leaf water content (%)"))) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.10.- MAP20 ~ total chl ####

clean_target2 <- clean_target2 %>%
  filter(!is.na(total_chl_fw_22)) %>% 
  group_by(sp_id) %>%
  mutate(chl_cat = cut(total_chl_fw_22,
                       breaks = quantile(total_chl_fw_22, 
                                         probs = c(0, 1/3, 2/3, 1), 
                                         na.rm = T),
                       include.lowest = T,
                       labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_chl_fw <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = chl_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "J") +
  xlab(expression(paste("Leaf chlorophyll content (μg g"^"-1", ")"))) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.11.- MAP20 ~ carotenoids ####

clean_target2 <- clean_target2 %>%
  filter(!is.na(xc_fw_22)) %>% 
  group_by(sp_id) %>%
  mutate(xc_cat = cut(xc_fw_22,
                      breaks = quantile(xc_fw_22, 
                                        probs = c(0, 1/3, 2/3, 1), 
                                        na.rm = T),
                      include.lowest = T,
                      labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_xc_fw <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = xc_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  labs(tag = "K") +
  xlab(expression(paste("Leaf carotenoids content (μg g"^"-1", ")"))) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.12.- MAP20 ~ chla/b ####

clean_target2 <- clean_target2 %>%
  filter(!is.na(chla_chlb_22)) %>% 
  group_by(sp_id) %>%
  mutate(chl_ab_cat = cut(chla_chlb_22,
                          breaks = quantile(chla_chlb_22, 
                                            probs = c(0, 1/3, 2/3, 1), 
                                            na.rm = T),
                          include.lowest = T,
                          labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_chl_ab <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = chl_ab_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "L") +
  xlab(expression(paste("Chlorophyll a/b ratio"))) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.13.- MAP20 ~ chl/xc ####

clean_target2 <- clean_target2 %>%
  filter(!is.na(chl_xc_22)) %>% 
  group_by(sp_id) %>%
  mutate(chl_xc_cat = cut(chl_xc_22,
                          breaks = quantile(chl_xc_22, 
                                            probs = c(0, 1/3, 2/3, 1), 
                                            na.rm = T),
                          include.lowest = T,
                          labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_chl_xc <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = chl_xc_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "M") +
  xlab(expression(paste("Chlorophylls/carotenoids ratio"))) +
  ylab(expression(paste("MAP_20 (mm)"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.14.- MAP20 ~ SLA ####

clean_target2 <- clean_target2 %>%
  filter(!is.na(sla_22)) %>% 
  group_by(sp_id) %>%
  mutate(sla_cat = cut(sla_22,
                       breaks = quantile(sla_22, 
                                         probs = c(0, 1/3, 2/3, 1), 
                                         na.rm = T),
                       include.lowest = T,
                       labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

prcp_sla <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = sla_cat, y = prec, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +  
  labs(tag = "N") +
  xlab(expression(paste("Tree average SLA (cm² g"^"-1", ")"))) + 
  ylab(expression(paste("MAP_20 (mm)"))) + 
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

tiff("04_figures/04_02_leaf_prcp20.tiff", units = "mm", width = 450, height = 400,
     res = 800, compression = "lzw")
prcp_height + prcp_dbh + prcp_hegyi + prcp_c + 
  prcp_n + prcp_d13c + prcp_d15n + prcp_d18o +  
  prcp_wc + prcp_chl_fw + prcp_xc_fw + prcp_chl_ab +  
  prcp_chl_xc + prcp_sla + 
  plot_layout(guides = 'collect', ncol = 4)
dev.off()