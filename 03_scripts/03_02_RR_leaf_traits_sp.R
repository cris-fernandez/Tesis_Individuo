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

clean_target$sp_status <- paste0(clean_target$sp_id, "_", clean_target$spot_status)

# 5.- Calculating the mean values ####

# Mean values are calculated by species and spot status, as they are needed 
# for the calculation of the response ratio 

rr_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  group_by(sp_status) %>% 
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
            mean_d13c = mean(d13c, na.rm = T),
            mean_d15n = mean(d15n, na.rm = T),
            mean_d18o = mean(d18o, na.rm = T),
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

# Data wrangling to obtain the desired structure: three columns (one per sp.) 
# with every variable in different rows

rownames(rr_target) <- rr_target$sp_status
rr_target <- rr_target %>% 
  t() %>% 
  as.data.frame()

# Removal of the former sp_status column 
rr_target <- rr_target[-which(rownames(rr_target) == "sp_status"), ]

# 6.- Calculating the log response ratio ####

rr_target <- rr_target %>% 
  mutate_if(is.character, as.numeric) %>% 
  mutate(Abialba = Abialba_hotspot / Abialba_coldspot,
         Pinsylv = Pinsylv_hotspot / Pinsylv_coldspot,
         Pinpine = Pinpine_hotspot / Pinpine_coldspot) %>% 
  dplyr::select(c(Abialba, Pinsylv, Pinpine)) %>% 
  mutate(var = rownames(rr_target)) %>% 
  mutate(Abialba = log(Abialba),
         Pinsylv = log(Pinsylv),
         Pinpine = log(Pinpine)) %>% 
  pivot_longer(-var, names_to = "sp_id", values_to = "response_ratio")

# 7.- Plotting`####

ggplot(rr_target) + 
  geom_col(aes(y = var, x = response_ratio, fill = sp_id))