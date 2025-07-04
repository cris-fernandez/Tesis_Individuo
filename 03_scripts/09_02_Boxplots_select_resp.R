rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading target data ####

clean_target <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_03_result_target.csv", 
                         header = T, sep = ",") %>% dplyr::select(-X) %>% 
  mutate(site = substr(plot_id, 1, 3))

# 2.- Removing 2023 data ####
# So I can have in the same column 2022 and 2023 values

clean_target <- clean_target %>% 
  dplyr::select(-contains("_23"))

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

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target2 <- clean_target %>% 
  mutate(sp_id = "all")

clean_target <- rbind(clean_target, clean_target2)
clean_target$sp_id <- as.factor(clean_target$sp_id)
clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine", "all"))

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(mean_def_obs, height, dbh, total_chl_fw_22, xc_fw_22, 
                  chlor_a_22, chlor_b_22, chla_chlb_22,
                  chl_xc_22, percent_c, percent_n, cn_ratio, leaf_d13c, 
                  leaf_d18o, leaf_d15n, wood_d13c_17, wood_d13c_22, sla_22,
                  age, hegyi_index, mean_bai, mean_1980, mean_20, mean_15,
                  mean_10, mean_05, Rt12, Rt17, Rt22, Rs12, Rs17, 
                  tree_number, sp_id, wc_22, spot_status)) %>% 
  dplyr::select(sort(names(.)))

# 6.- Plotting ####
## 6.1.- Defoliation ####

box_defo <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = mean_def_obs, fill = sp_id, alpha = spot_status)) + 
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
  ylab("Tree mean defoliation (%)") +
  xlab("") + 
  ylim(0, 90) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.2.- Water content ####

box_wc <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = wc_22, fill = sp_id, alpha = spot_status)) + 
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
  ylab("Leaves water content (%)") +
  xlab("") + 
  # ylim(0, 90) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.3.- Chl/xc ####

box_chl_xc <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = chl_xc_22, fill = sp_id, alpha = spot_status)) + 
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
  ylab(expression(paste("Chlorophylls/carotenoids ratio"))) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.4.- d13c ####

box_d13c <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = leaf_d13c, fill = sp_id, alpha = spot_status)) + 
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
  ylab(bquote("Leaves δ"~C^13~"(‰)")) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 6.5.- d15N ####

box_d15n <- ggplot(clean_target) + 
  geom_boxplot(aes(x = sp_id, y = leaf_d15n, fill = sp_id, alpha = spot_status)) + 
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
  ylab(bquote("Leaves δ"~N^15~"(‰)")) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 


# 7.- Leaf traits plotting ####

tiff("04_figures/09_02_select_boxplots_resp.tiff", units = "mm", width = 500, height = 300,
     res = 800, compression = "lzw")
box_defo + box_wc + box_chl_xc + 
  box_d13c + box_d15n +  plot_layout(guides = 'collect', ncol = 3)
dev.off()