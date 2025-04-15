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

clean_target$sp_id <- fct_relevel(clean_target$sp_id, "Abialba", "Pinsylv", "Pinpine")

# 5.- Structure - BAI scatterplots ####

## 5.1.- BAI ~ height ####

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

bai_height <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = height_cat, y = mean, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("Tree height (m)") +
  ylab(expression(paste("BAI (cm² year"^"-1", ")"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.2.- BAI80 ~ height ####

bai80_height <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = height_cat, y = mean_1980, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab(expression(paste("BAI 1980 (cm² year"^"-1", ")"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.3.- BAI20 ~ height ####

bai20_height <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = height_cat, y = mean_20, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab(expression(paste("BAI 20 (cm² year"^"-1", ")"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22))

## 5.4.- BAI15 ~ height ####

bai15_height <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = height_cat, y = mean_15, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab(expression(paste("BAI 15 (cm² year"^"-1", ")"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.5.- BAI10 ~ height ####

bai10_height <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = height_cat, y = mean_10, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab(expression(paste("BAI 10 (cm² year"^"-1", ")"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22))

## 5.6.- BAI80 ~ height ####

bai05_height <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = height_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab(expression(paste("BAI 05 (cm² year"^"-1", ")"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.7.- BAI ~ dbh ####

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

bai_dbh <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = dbh_cat, y = mean, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.8.- BAI80 ~ dbh ####

bai80_dbh <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = dbh_cat, y = mean_1980, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.9.- BAI20 ~ dbh ####

bai20_dbh <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = dbh_cat, y = mean_20, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.10.- BAI15 ~ dbh ####

bai15_dbh <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = dbh_cat, y = mean_15, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.11.- BAI10 ~ dbh ####

bai10_dbh <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = dbh_cat, y = mean_10, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.12.- BAI05 ~ dbh ####

bai05_dbh <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = dbh_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("Tree dbh (cm)") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.13.- BAI ~ Hegyi ####

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

bai_hegyi <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = hegyi_cat, y = mean, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.14.- BAI80 ~ Hegyi ####

bai80_hegyi <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = hegyi_cat, y = mean_1980, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.15.- BAI20 ~ Hegyi ####

bai20_hegyi <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = hegyi_cat, y = mean_20, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.16.- BAI15 ~ Hegyi ####

bai15_hegyi <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = hegyi_cat, y = mean_15, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.17.- BAI10 ~ Hegyi ####

bai10_hegyi <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = hegyi_cat, y = mean_10, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.18.- BAI05 ~ Hegyi ####

bai05_hegyi <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = hegyi_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("Hegyi Index") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.19.- BAI ~ age ####

clean_target2 <- clean_target %>%
  filter(!is.na(age)) %>% 
  group_by(sp_id) %>%
  mutate(age_cat = cut(age,
                         breaks = quantile(age, 
                                           probs = c(0, 1/3, 2/3, 1), 
                                           na.rm = T),
                         include.lowest = T,
                         labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

bai_age <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = age_cat, y = mean, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.20.- BAI80 ~ age ####

bai80_age <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = age_cat, y = mean_1980, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.21.- BAI20 ~ age ####

bai20_age <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = age_cat, y = mean_20, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.22.- BAI15 ~ age ####

bai15_age <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = age_cat, y = mean_15, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.23.- BAI10 ~ age ####

bai10_age <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = age_cat, y = mean_10, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 5.24.- BAI05 ~ Hegyi ####

bai05_age <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = age_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("Tree age (years)") +
  ylab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

# 6.- Dendro structure plotting ####

tiff("04_figures/04_02_dendro_structure_sp.tiff", units = "mm", width = 450, height = 400,
     res = 800, compression = "lzw")
bai_height + bai_dbh + bai_hegyi + bai_age + 
  bai80_height + bai80_dbh + bai80_hegyi + bai80_age + 
  bai20_height + bai20_dbh + bai20_hegyi + bai20_age +   
  bai15_height + bai15_dbh + bai15_hegyi + bai15_age + 
  bai10_height + bai10_dbh + bai10_hegyi + bai10_age + 
  bai05_height + bai05_dbh + bai05_hegyi + bai05_age + 
  plot_layout(guides = 'collect', ncol = 4, )
dev.off()