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

# Adding T290 defoliation info:

clean_target <- clean_target %>% 
  mutate(mean_def_obs = ifelse(tree_number == "T290", 15, mean_def_obs))

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
                                ifelse(clean_target$mean_def_obs < 25,
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

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target$sp_id <- as.factor(clean_target$sp_id)
clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine",),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(mean_def_obs, height, dbh, total_chl_fw_22, xc_fw_22, 
                  chlor_a_22, chlor_b_22, chla_chlb_22,
                  chl_xc_22, percent_c, percent_n, cn_ratio, leaf_d13c, 
                  leaf_d18o, leaf_d15n, sla_22,
                  age, hegyi_index, mean_bai, mean_1980, mean_20, mean_15,
                  mean_10, mean_05, Rt12, Rt17, Rt22, Rs12, Rs17, 
                  tree_number, sp_id, wc_22, spot_status, vigor_id)) %>% 
  dplyr::select(sort(names(.)))


# 5.- Calculating the mean values ####

# Mean values are calculated by vigor_id, as they are needed 
# for the calculation of the response ratio 

rr_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  group_by(spot_status, sp_id) %>% 
  summarise(mean_height = mean(height, na.rm = T),
            mean_dbh = mean(dbh, na.rm = T),
            mean_wc = mean(wc_22, na.rm = T),
            mean_chl = mean(total_chl_fw_22, na.rm = T),
            mean_xc = mean(xc_fw_22, na.rm = T),
            mean_chlab = mean(chla_chlb_22, na.rm = T),
            mean_chlxc = mean(chl_xc_22, na.rm = T),
            mean_c = mean(percent_c, na.rm = T),
            mean_n = mean(percent_n, na.rm = T),
            mean_cn = mean(cn_ratio, na.rm = T),
            mean_d13c = mean(leaf_d13c, na.rm = T),
            mean_d15n = mean(leaf_d15n, na.rm = T),
            mean_d18o = mean(leaf_d18o, na.rm = T),
            mean_sla = mean(sla_22, na.rm = T),
            mean_age = mean(age, na.rm = T),
            mean_hegyi = mean(hegyi_index, na.rm = T),
            mean_bai = mean(mean_bai, na.rm = T),
            mean_bai80 = mean(mean_1980, na.rm = T),
            mean_bai20 = mean(mean_20, na.rm = T),
            mean_bai15 = mean(mean_15, na.rm = T),
            mean_bai10 = mean(mean_10, na.rm = T),
            mean_bai05 = mean(mean_05, na.rm = T),
            mean_rt12 = mean(Rt12, na.rm = T),
            mean_rt17 = mean(Rt17, na.rm = T),
            mean_rt22 = mean(Rt22, na.rm = T),
            mean_rs12 = mean(Rs12, na.rm = T),
            mean_rs17 = mean(Rs17, na.rm = T))

# The standard deviations per group for the calculation of SE later:

sd_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  group_by(spot_status, sp_id) %>% 
  summarise(se_height = sd(height, na.rm = T) / sqrt(n()),
            se_dbh = sd(dbh, na.rm = T) / sqrt(n()),
            se_wc = sd(wc_22, na.rm = T) / sqrt(n()),
            se_chl = sd(total_chl_fw_22, na.rm = T) / sqrt(n()),
            se_xc = sd(xc_fw_22, na.rm = T) / sqrt(n()),
            se_chlab = sd(chla_chlb_22, na.rm = T) / sqrt(n()),
            se_chlxc = sd(chl_xc_22, na.rm = T) / sqrt(n()),
            se_c = sd(percent_c, na.rm = T) / sqrt(n()),
            se_n = sd(percent_n, na.rm = T) / sqrt(n()),
            se_cn = sd(cn_ratio, na.rm = T) / sqrt(n()),
            se_d13c = sd(leaf_d13c, na.rm = T) / sqrt(n()),
            se_d15n = sd(leaf_d15n, na.rm = T) / sqrt(n()),
            se_d18o = sd(leaf_d18o, na.rm = T) / sqrt(n()),
            se_sla = sd(sla_22, na.rm = T) / sqrt(n()),
            se_age = sd(age, na.rm = T) / sqrt(n()),
            se_hegyi = sd(hegyi_index, na.rm = T) / sqrt(n()),
            se_bai = sd(mean_bai, na.rm = T) / sqrt(n()),
            se_bai80 = sd(mean_1980, na.rm = T) / sqrt(n()),
            se_bai20 = sd(mean_20, na.rm = T) / sqrt(n()),
            se_bai15 = sd(mean_15, na.rm = T) / sqrt(n()),
            se_bai10 = sd(mean_10, na.rm = T) / sqrt(n()),
            se_bai05 = sd(mean_05, na.rm = T) / sqrt(n()),
            se_rt12 = sd(Rt12, na.rm = T) / sqrt(n()),
            se_rt17 = sd(Rt17, na.rm = T) / sqrt(n()),
            se_rt22 = sd(Rt22, na.rm = T) / sqrt(n()),
            se_rs12 = sd(Rs12, na.rm = T) / sqrt(n()),
            se_rs17 = sd(Rs17, na.rm = T) / sqrt(n()))


# Data wrangling to obtain the desired structure: two columns 
# with every variable in different rows

rownames(rr_target) <- paste0(rr_target$spot_status, "_", 
                              rr_target$sp_id)
rr_target <- rr_target %>% 
  t() %>% 
  as.data.frame()

rownames(sd_target) <- paste0(sd_target$spot_status, "_", 
                              sd_target$sp_id)
sd_target <- sd_target %>% 
  t() %>% 
  as.data.frame

# Removal of the former sp_status column 

rr_target <- rr_target[-c(which(rownames(rr_target) == "spot_status"),
                          which(rownames(rr_target) == "sp_id")), ]
sd_target <- sd_target[-c(which(rownames(sd_target) == "spot_status"),
                          which(rownames(sd_target) == "sp_id")), ]

# 6.- Joining data frames ####

# First, we need to make sure the "var" column matches both dataframes

rr_target <- rr_target %>% 
  mutate(var = rownames(rr_target)) %>% 
  mutate(var = gsub("mean_", "", var))

sd_target <- sd_target %>% 
  mutate(var = rownames(sd_target)) %>% 
  mutate(var = gsub("se_", "", var)) # So var in both dfs are equal

# Now, we can join by var:

rr_df <- full_join(rr_target, sd_target, by = "var")
colnames(rr_df) <- c("mean_coldspot_Abialba", "mean_coldspot_Pinsylv", "mean_coldspot_Pinpine",
                     "mean_hotspot_Abialba", "mean_hotspot_Pinsylv", "mean_hotspot_Pinpine",
                     "var", 
                     "se_coldspot_Abialba", "se_coldspot_Pinsylv", "se_coldspot_Pinpine",
                     "se_hotspot_Abialba", "se_hotspot_Pinsylv", "se_hotspot_Pinpine")

rr_df2 <- rr_df %>%
  pivot_longer(cols = -var, names_to = c("stat", "vigor_id", "sp_id"),
               names_pattern = "(mean|se)_(coldspot|hotspot)_(.*)") %>% 
  mutate(var_status = paste0(stat, "_", vigor_id)) %>% 
  dplyr::select(-c(stat, vigor_id)) %>% 
  pivot_wider(names_from = "var_status",
              values_from = "value") %>% 
  mutate_at(vars(mean_coldspot, mean_hotspot, se_coldspot, se_hotspot), as.numeric)

# 7.- Calculating the log response ratio ####

rr_df2$response_ratio <- abs(log(rr_df2$mean_hotspot / rr_df2$mean_coldspot))

# 8.- Calculating SE ####

# The SE of a response ratio equales the square root of the sum of squares of 
# the quotient of SE and the mean of each set (hot and coldspot) 

rr_df2 <- rr_df2 %>% 
  mutate(se_rr = sqrt((se_hotspot / mean_hotspot)^2 + (se_coldspot / mean_coldspot)^2))

# 9.- Adding a column to reorder by Pinsylv values ####

rr_psy <- rr_df2 %>% 
  filter(sp_id == "Pinsylv") %>% 
  dplyr::select(var, response_ratio) %>% 
  rename(psy_rr = response_ratio)
rr_df3 <- full_join(rr_df2, rr_psy, by = "var") %>% 
  filter()

# 9.- Plotting 
# varnames <- c("BAI 10 years", "BAI 05 years", "BAI 15 years", "BAI 20 years", 
#                        "BAI since 1980", "BAI", "Hegyi Index", "Chlorophylls content", 
#                        "Leaf δ15N", "Rs 2012", "Rt 2012", "Age", "SLA", "N content", 
#                        "Chl / carotenoids", "Carotenoids content", "Leaf C:N", "d.b.h.", 
#                        "Chl a / Chl b", "Rt 2022", "Rt 2017", "Leaf δ18O",  "Leaf δ13C", 
#                        "Height", "Rs 2017", "C content", "Wood δ13C 2017", 
#                        "Wood δ13C 2022") %>% rev()

# 10.- Plotting morphological responses ####

morfo_vars <- c("height", "dbh", "c", "n", "cn", "sla", "age", "hegyi")

morfo_df3 <- rr_df3 %>% filter(var %in% morfo_vars)  %>%
  mutate(var_ord = fct_reorder(var, psy_rr)) 

morfo_names <- c("Age", "Hegyi Index", "Height", "N content", "d.b.h.", "Leaf C:N", 
                 "C content", "SLA") %>% rev()

morfo_rr_plot <- ggplot(morfo_df3) + 
  geom_point(aes(y = var_ord, x = response_ratio, col = sp_id), 
             size = 2.5, position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmax = response_ratio + se_rr, xmin = response_ratio - se_rr, 
                     y = var_ord, col = sp_id), height = 0, size = 1.1, 
                 position = position_dodge(width = 0.3)) + 
  geom_vline(xintercept = 0, linetype = "dashed", 
             color = "gray35", size = .15) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_y_discrete(labels = morfo_names) +
  xlab("log(Response ratio)") + 
  ylab("") + 
  labs(tag = "A") +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        plot.tag = element_text(size = 25),
        legend.position = "right",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 20))

tiff("04_figures/34_02_ranked_rr_morpho.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
morfo_rr_plot
dev.off()

# 11.- Plotting physiological responses ####

physio_vars <- c("chl", "xc", "chlab", "chlxc", "d13c", "d15n", "d18o", 
                 "wc")

physio_df3 <- rr_df3 %>% filter(var %in% physio_vars)

physio_names <- c("Leaf δ15N", "Chlorophylls content", "Carotenoids content", 
                  "LWC", "Chl a / Chl b", "Leaf δ18O", "Chl / carotenoids", 
                  "Leaf δ13C") %>% rev()

physio_rr_plot <- ggplot(physio_df3) + 
  geom_point(aes(y = fct_reorder(var, psy_rr), x = response_ratio, col = sp_id), 
             size = 2.5, position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmax = response_ratio + se_rr, xmin = response_ratio - se_rr, 
                     y = fct_reorder(var, response_ratio), col = sp_id), height = 0, size = 1.1, 
                 position = position_dodge(width = 0.3)) + 
  geom_vline(xintercept = 0, linetype = "dashed", 
             color = "gray35", size = .15) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_y_discrete(labels = physio_names) +
  xlab("log(Response ratio)") + 
  ylab("") + 
  labs(tag = "B") +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        plot.tag = element_text(size = 25),
        legend.position = "right",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 20))

tiff("04_figures/34_02_ranked_rr_physio.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
physio_rr_plot
dev.off()

# 12.- Plotting whole-plant responses ####

whole_vars <- c("bai80", "bai05", "rt12", 
                "rt17", "rt22", "rs12", "rs17")

whole_df3 <- rr_df3 %>% filter(var %in% whole_vars)

whole_names <- c("BAI since 1980", "BAI 05 years", "Rs 2012", "Rt 2012", "Rs 2017", 
                 "Rt 2017", "Rt 2022") %>% rev()

whole_rr_plot <- ggplot(whole_df3) + 
  geom_point(aes(y = fct_reorder(var, psy_rr), x = response_ratio, col = sp_id), 
             size = 2.5, position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmax = response_ratio + se_rr, xmin = response_ratio - se_rr, 
                     y = fct_reorder(var, response_ratio), col = sp_id), height = 0, size = 1.1, 
                 position = position_dodge(width = 0.3)) + 
  geom_vline(xintercept = 0, linetype = "dashed", 
             color = "gray35", size = .15) + 
  scale_color_manual(breaks = c("Abialba", "Pinsylv", "Pinpine"),
                     values = c("Abialba" = "#785EF0",
                                "Pinsylv" = "#FFB000",
                                "Pinpine" = "#990000"),
                     labels = c("A. alba",
                                "P. sylvestris",
                                "P. pinea"),
                     name = "") +
  scale_y_discrete(labels = whole_names) +
  xlab("log(Response ratio)") + 
  ylab("") + 
  labs(tag = "C") +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 16),
        plot.tag = element_text(size = 25),
        legend.position = "right",
        legend.key.size = unit(2, "cm"),
        legend.text = element_text(size = 20))

tiff("04_figures/34_02_ranked_rr_whole.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
whole_rr_plot
dev.off()

# 13.- All together ####

tiff("04_figures/34_02_ranked_rr_panels_spot.tiff", units = "mm", 
     width = 400, height = 400,
     res = 700, compression = "lzw")
morfo_rr_plot + physio_rr_plot + whole_rr_plot + guide_area() + 
  plot_layout(guides = 'collect', ncol = 2)
dev.off()