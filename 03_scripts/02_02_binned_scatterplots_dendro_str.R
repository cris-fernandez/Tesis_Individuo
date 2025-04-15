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

# 5.- Dendro indices scatterplots ####

## 5.1.- BAI ~ Rt12 ####

clean_target2 <- clean_target %>%
  filter(!is.na(Rt12)) %>% 
  group_by(sp_id) %>%
  mutate(Rt12_cat = cut(Rt12,
                        breaks = quantile(Rt12, 
                                            probs = c(0, 1/3, 2/3, 1), 
                                            na.rm = T),
                        include.lowest = T,
                        labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

bai_rt12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt12_cat, y = mean, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("") +
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

## 5.2.- BAI80 ~ Rt12 ####

bai80_rt12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt12_cat, y = mean_1980, fill = sp_id)) + 
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

## 5.3.- BAI20 ~ Rt12 ####

bai20_rt12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt12_cat, y = mean_20, fill = sp_id)) + 
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

## 5.4.- BAI15 ~ Rt12 ####

bai15_rt12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt12_cat, y = mean_15, fill = sp_id)) + 
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

## 5.5.- BAI10 ~ Rt12 ####

bai10_rt12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt12_cat, y = mean_10, fill = sp_id)) + 
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

## 5.6.- BAI05 ~ Rt12 ####

bai05_rt12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt12_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("2012 Resistance") +
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

## 5.7.- BAI ~ Rs12 ####

clean_target2 <- clean_target %>%
  filter(!is.na(Rs12)) %>% 
  group_by(sp_id) %>%
  mutate(Rs12_cat = cut(Rs12,
                        breaks = quantile(Rs12, 
                                          probs = c(0, 1/3, 2/3, 1), 
                                          na.rm = T),
                        include.lowest = T,
                        labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

bai_rs12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs12_cat, y = mean, fill = sp_id)) + 
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

## 5.8.- BAI80 ~ Rs12 ####

bai80_rs12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs12_cat, y = mean_1980, fill = sp_id)) + 
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

## 5.9.- BAI20 ~ Rs12 ####

bai20_rs12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs12_cat, y = mean_20, fill = sp_id)) + 
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

## 5.10.- BAI15 ~ Rs12 ####

bai15_rs12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs12_cat, y = mean_15, fill = sp_id)) + 
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

## 5.11.- BAI10 ~ Rs12 ####

bai10_rs12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs12_cat, y = mean_10, fill = sp_id)) + 
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

## 5.12.- BAI05 ~ Rs12 ####

bai05_rs12 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs12_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("2012 Resilience") +
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

## 5.13.- BAI ~ Rt17 ####

clean_target2 <- clean_target %>%
  filter(!is.na(Rt17)) %>% 
  group_by(sp_id) %>%
  mutate(Rt17_cat = cut(Rt17,
                        breaks = quantile(Rs12, 
                                          probs = c(0, 1/3, 2/3, 1), 
                                          na.rm = T),
                        include.lowest = T,
                        labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

bai_rt17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt17_cat, y = mean, fill = sp_id)) + 
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

## 5.14.- BAI80 ~ Rt17 ####

bai80_rt17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt17_cat, y = mean_1980, fill = sp_id)) + 
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

## 5.15.- BAI20 ~ Rt17 ####

bai20_rt17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt17_cat, y = mean_20, fill = sp_id)) + 
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

## 5.16.- BAI15 ~ Rt17 ####

bai15_rt17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt17_cat, y = mean_15, fill = sp_id)) + 
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

## 5.17.- BAI10 ~ Rt17 ####

bai10_rt17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt17_cat, y = mean_10, fill = sp_id)) + 
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

## 5.18.- BAI05 ~ Rt17 ####

bai05_rt17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt17_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("2017 Resistance") +
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

## 5.19.- BAI ~ Rs17 ####

clean_target2 <- clean_target %>%
  filter(!is.na(Rs17)) %>% 
  group_by(sp_id) %>%
  mutate(Rs17_cat = cut(Rs17,
                        breaks = quantile(Rs17, 
                                          probs = c(0, 1/3, 2/3, 1), 
                                          na.rm = T),
                        include.lowest = T,
                        labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

bai_rs17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs17_cat, y = mean, fill = sp_id)) + 
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

## 5.20.- BAI80 ~ Rs17 ####

bai80_rs17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs17_cat, y = mean_1980, fill = sp_id)) + 
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

## 5.21.- BAI20 ~ Rs17 ####

bai20_rs17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs17_cat, y = mean_20, fill = sp_id)) + 
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

## 5.22.- BAI15 ~ Rs17 ####

bai15_rs17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs17_cat, y = mean_15, fill = sp_id)) + 
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

## 5.23.- BAI10 ~ Rs17 ####

bai10_rs17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs17_cat, y = mean_10, fill = sp_id)) + 
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

## 5.24.- BAI05 ~ Rs17 ####

bai05_rs17 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rs17_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("2017 Resilience") +
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

## 5.25.- BAI ~ Rt22 ####

clean_target2 <- clean_target %>%
  filter(!is.na(Rt22)) %>% 
  group_by(sp_id) %>%
  mutate(Rt22_cat = cut(Rs12,
                        breaks = quantile(Rt22, 
                                          probs = c(0, 1/3, 2/3, 1), 
                                          na.rm = T),
                        include.lowest = T,
                        labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup()

bai_rt22 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt22_cat, y = mean, fill = sp_id)) + 
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

## 5.26.- BAI80 ~ Rt22 ####

bai80_rt22 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt22_cat, y = mean_1980, fill = sp_id)) + 
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

## 5.27.- BAI20 ~ Rt22 ####

bai20_rt22 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt22_cat, y = mean_20, fill = sp_id)) + 
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

## 5.28.- BAI15 ~ Rt22 ####

bai15_rt22 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt22_cat, y = mean_15, fill = sp_id)) + 
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

## 5.29.- BAI10 ~ Rt22 ####

bai10_rt22 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt22_cat, y = mean_10, fill = sp_id)) + 
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

## 5.30.- BAI05 ~ Rt22 ####

bai05_rt22 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = Rt22_cat, y = mean_05, fill = sp_id)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77",
                               "Pinpine" = "#db5f02"),
                    labels = c("A. alba",
                               "P. sylvestris",
                               "P. pinea"),
                    name = "") +
  xlab("2022 Resistance") +
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

# 6.- Dendro indices plotting ####

tiff("04_figures/04_02_dendro_lloret_sp.tiff", units = "mm", width = 450, height = 400,
     res = 800, compression = "lzw")
bai_rt12 + bai_rs12 + bai_rt17 + bai_rs17 + bai_rt22 + 
  bai80_rt12 + bai80_rs12 + bai80_rt17 + bai80_rs17 + bai80_rt22 + 
  bai20_rt12 + bai20_rs12 + bai20_rt17 + bai20_rs17 + bai20_rt22 + 
  bai15_rt12 + bai15_rs12 + bai15_rt17 + bai15_rs17 + bai15_rt22 + 
  bai10_rt12 + bai10_rs12 + bai10_rt17 + bai10_rs17 + bai10_rt22 + 
  bai05_rt12 + bai05_rs12 + bai05_rt17 + bai05_rs17 + bai05_rt22 + 
  plot_layout(guides = 'collect', ncol = 5)
dev.off()