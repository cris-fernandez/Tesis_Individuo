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

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

# 5.- Calculating the mean values ####

# Mean values are calculated by spot status, as they are needed 
# for the calculation of the response ratio 

rr_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  group_by(spot_status) %>% 
  summarise(mean_height = mean(height, na.rm = T),
            mean_dbh = mean(dbh, na.rm = T),
            mean_height = mean(height, na.rm = T),
            mean_chl = mean(chl_fw_22, na.rm = T),
            mean_xc = mean(xc_fw_22, na.rm = T),
            mean_chlab = mean(chla_chlb_22, na.rm = T),
            mean_chlxc = mean(chl_xc_22, na.rm = T),
            mean_c = mean(percent_c, na.rm = T),
            mean_n = mean(percent_n, na.rm = T),
            mean_cn = mean(cn_ratio, na.rm = T),
            mean_d13c = mean(leaf_d13c, na.rm = T),
            mean_d15n = mean(leaf_d15n, na.rm = T),
            mean_d18o = mean(leaf_d18o, na.rm = T),
            mean_d13c_17 = mean(wood_d13c_17, na.rm = T),
            mean_d13c_22 = mean(wood_d13c_22, na.rm = T),
            mean_sla = mean(sla_22, na.rm = T),
            mean_age = mean(age, na.rm = T),
            mean_hegyi = mean(hegyi_index, na.rm = T),
            mean_bai = mean(mean, na.rm = T),
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
  group_by(spot_status) %>% 
  summarise(se_height = sd(height, na.rm = T) / sqrt(n()),
            se_dbh = sd(dbh, na.rm = T) / sqrt(n()),
            se_height = sd(height, na.rm = T) / sqrt(n()),
            se_chl = sd(chl_fw_22, na.rm = T) / sqrt(n()),
            se_xc = sd(xc_fw_22, na.rm = T) / sqrt(n()),
            se_chlab = sd(chla_chlb_22, na.rm = T) / sqrt(n()),
            se_chlxc = sd(chl_xc_22, na.rm = T) / sqrt(n()),
            se_c = sd(percent_c, na.rm = T) / sqrt(n()),
            se_n = sd(percent_n, na.rm = T) / sqrt(n()),
            se_cn = sd(cn_ratio, na.rm = T) / sqrt(n()),
            se_d13c = sd(leaf_d13c, na.rm = T) / sqrt(n()),
            se_d15n = sd(leaf_d15n, na.rm = T) / sqrt(n()),
            se_d18o = sd(leaf_d18o, na.rm = T) / sqrt(n()),
            se_d13c_17 = sd(wood_d13c_17, na.rm = T) / sqrt(n()),
            se_d13c_22 = sd(wood_d13c_22, na.rm = T) / sqrt(n()),
            se_sla = sd(sla_22, na.rm = T) / sqrt(n()),
            se_age = sd(age, na.rm = T) / sqrt(n()),
            se_hegyi = sd(hegyi_index, na.rm = T) / sqrt(n()),
            se_bai = sd(mean, na.rm = T) / sqrt(n()),
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

rownames(rr_target) <- rr_target$spot_status
rr_target <- rr_target %>% 
  t() %>% 
  as.data.frame()

rownames(sd_target) <- sd_target$spot_status
sd_target <- sd_target %>% 
  t() %>% 
  as.data.frame

# Removal of the former sp_status column 

rr_target <- rr_target[-which(rownames(rr_target) == "spot_status"), ]
sd_target <- sd_target[-which(rownames(sd_target) == "spot_status"), ]

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
colnames(rr_df) <- c("mean_hotspot", "mean_coldspot", "var", "se_hotspot", "se_coldspot")

# 7.- Calculating the log response ratio ####

# We first need to transform all variables but "var" to numeric

rr_df <- rr_df %>% 
  mutate_at(vars(mean_hotspot, mean_coldspot, se_hotspot, se_coldspot), as.numeric)

rr_df$response_ratio <- abs(log(rr_df$mean_hotspot / rr_df$mean_coldspot))

# 8.- Calculating SE ####

# The SE of a response ratio equales the square root of the sum of squares of 
# the quotient of SE and the mean of each set (hot and coldspot) 

rr_df <- rr_df %>% 
  mutate(se_rr = sqrt((se_hotspot / mean_hotspot)^2 + (se_coldspot / mean_coldspot)^2))
  
# 9.- Plotting`####

varnames <- c("BAI since 1980", "BAI", "BAI 20 years", "BAI 15 years", "BAI 10 years", 
              "BAI 05 years", "Hegyi Index", "Height", "Age", "Rt 2012", "Rs 2017", "Rt 2022",
              "d.b.h.", "Carotenoids content", "SLA", "Rs 2012", "N content", 
              "Leaf δ15N", "Chlorophylls content", "Chl / carotenoids", "Chl a / Chl b",
              "Leaf C:N", "Rt 2017", "Leaf δ13C", "Wood δ13C 2017", "Wood δ13C 2022", 
              "Leaf δ18C", "C content") %>% rev()

rr_plot <- ggplot(rr_df) + 
  geom_point(aes(y = fct_reorder(var, response_ratio), x = response_ratio), 
             size = 2.5) +
  geom_errorbarh(aes(xmax = response_ratio + se_rr, xmin = response_ratio - se_rr, 
                     y = fct_reorder(var, response_ratio)), height = 0, size = 1.1) + 
  geom_vline(xintercept = 0, linetype = "dashed", 
               color = "gray35", size = .15) + 
  scale_y_discrete(labels = varnames) + 
  xlab("log(Response ratio)") + 
  ylab("") + 
  theme_classic() + 
  theme(panel.grid.major.y = element_line(),
        panel.grid.minor.y = element_line(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 16))

tiff("04_figures/04_03_ranked_response_ratios.tiff", units = "mm", 
     width = 200, height = 300,
     res = 700, compression = "lzw")
  rr_plot
dev.off()
