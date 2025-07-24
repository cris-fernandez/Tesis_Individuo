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

# 5.- Calculating the mean values ####

# Mean values are calculated by spot status, as they are needed 
# for the calculation of the response ratio 

rr_target <- clean_target %>% 
  group_by(spot_status, vigor_id) %>% 
  summarise(mean_height = mean(height, na.rm = T),
            mean_dbh = mean(dbh, na.rm = T),
            mean_sla = mean(sla_22, na.rm = T),
            mean_age = mean(age, na.rm = T),
            mean_hegyi = mean(hegyi_index, na.rm = T),
            mean_bai = mean(mean, na.rm = T),
            mean_bai80 = mean(mean_1980, na.rm = T),
            mean_rt12 = mean(Rt12, na.rm = T),
            mean_rt17 = mean(Rt17, na.rm = T),
            mean_rs12 = mean(Rs12, na.rm = T),
            mean_rs17 = mean(Rs17, na.rm = T))

# The standard deviations per group for the calculation of SE later:

sd_target <- clean_target %>% 
  group_by(spot_status, vigor_id) %>% 
  summarise(se_height = sd(height, na.rm = T) / sqrt(n()),
            se_dbh = sd(dbh, na.rm = T) / sqrt(n()),
            se_sla = sd(sla_22, na.rm = T) / sqrt(n()),
            se_age = sd(age, na.rm = T) / sqrt(n()),
            se_hegyi = sd(hegyi_index, na.rm = T) / sqrt(n()),
            se_bai = sd(mean, na.rm = T) / sqrt(n()),
            se_bai80 = sd(mean_1980, na.rm = T) / sqrt(n()),
            se_rt12 = sd(Rt12, na.rm = T) / sqrt(n()),
            se_rt17 = sd(Rt17, na.rm = T) / sqrt(n()),
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

# Then, we need to make sure the "var" column matches both dataframes

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
  pivot_longer(cols = -var, names_to = c("stat", "spot_status", "sp_id"),
               names_pattern = "(mean|se)_(coldspot|hotspot)_(.*)") %>% 
  mutate(var_status = paste0(stat, "_", spot_status)) %>% 
  dplyr::select(-c(stat, spot_status)) %>% 
  pivot_wider(names_from = "var_status",
              values_from = "value") %>% 
  mutate_at(vars(mean_hotspot, mean_coldspot, se_hotspot, se_coldspot), as.numeric)

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
rr_df2 <- full_join(rr_df2, rr_psy, by = "var")

# 10.- Plotting ####

varnames <- c("BAI since 1980", 
              "BAI 05 years", "BAI", "Rs 2012", "Rt 2012", "Age", "Rs 2017", 
              "Hegyi Index", "Height", "Rt 2017", "d.b.h.", "SLA") %>% rev()

rr_plot <- ggplot(rr_df2) + 
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
  scale_y_discrete(labels = varnames) +
  xlab("log(Response ratio)") + 
  ylab("") + 
  theme_classic() + 
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))

tiff("04_figures/04_04_Vuln_response_ratios_sp.tiff", units = "mm", 
     width = 200, height = 200,
     res = 700, compression = "lzw")
rr_plot
dev.off()

# # 10.- Plotting after var. selection ####
# 
# # These variables are discarded after making the correlogram, so we avoid 
# # using redundant variables
# 
# discarded_vars <- c("bai", "bai20", "bai15", "bai10", "bai05", "dbh")
# rr_df2 <- rr_df %>% 
#   filter(!var %in% discarded_vars)
# 
# # 9.- Plotting ####
# 
# varnames2 <- c("BAI since 1980", "Hegyi Index", "Height", "Age", "Rt 2012", 
#                "Rs 2017", "Rt 2022", "SLA", "Rs 2012", "Rt 2017") %>% rev()
# 
# rr_plot2 <- ggplot(rr_df2) + 
#   geom_point(aes(y = fct_reorder(var, response_ratio), x = response_ratio), 
#              size = 2.5, position = position_dodge(width = 0.9)) +
#   geom_errorbarh(aes(xmax = response_ratio + se_rr, xmin = response_ratio - se_rr, 
#                      y = fct_reorder(var, response_ratio)), height = 0, size = 1.1, 
#                  position = position_dodge(width = 0.9)) + 
#   geom_vline(xintercept = 0, linetype = "dashed", 
#              color = "gray35", size = .15) + 
#   # scale_y_discrete(labels = varnames2) + 
#   xlab("log(Response ratio)") + 
#   ylab("") + 
#   theme_classic() + 
#   theme(panel.grid.major.y = element_line(),
#         panel.grid.minor.y = element_line(),
#         axis.text.x = element_text(size = 16),
#         axis.text.y = element_text(size = 16),
#         axis.title.x = element_text(size = 16))
# 
# tiff("04_figures/04_04_Vuln_response_ratios_selection.tiff", units = "mm", 
#      width = 200, height = 200,
#      res = 700, compression = "lzw")
# rr_plot2
# dev.off()