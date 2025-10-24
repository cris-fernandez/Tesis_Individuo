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

clean_target$cn <- clean_target$percent_c / clean_target$percent_n

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine"),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"),
         spot_status = fct_relevel(spot_status, "coldspot", "hotspot"))

# 5.- N per category ####

clean_target %>% count(sp_id)
clean_target %>% count(vigor_id)

# 6.- Variables distribution ####

aa_target <- clean_target %>% filter(sp_id == "Abialba") %>% 
  mutate(across(where(is.numeric) & !matches("mean_def_obs"),
                ~ (.x - min(.x, na.rm = TRUE)) / 
                  (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))))

aa_target <- aa_target %>% 
  mutate(across(where(is.numeric) & !matches("mean_def_obs"),
                ~ log1p(.x)))
ps_target <- clean_target %>% filter(sp_id == "Pinsylv") %>% 
  mutate(across(where(is.numeric) & !matches("mean_def_obs"),
                ~ (.x - min(.x, na.rm = TRUE)) / 
                  (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))))

ps_target <- ps_target %>% 
  mutate(across(where(is.numeric) & !matches("mean_def_obs"),
                ~ log1p(.x)))

pp_target <- clean_target %>% filter(sp_id == "Pinpine") %>% 
  mutate(across(where(is.numeric) & !matches("mean_def_obs"),
                ~ (.x - min(.x, na.rm = TRUE)) / 
                  (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))))

pp_target <- pp_target %>% 
  mutate(across(where(is.numeric) & !matches("mean_def_obs"),
                ~ log1p(.x)))

plot_theme <- list(ylab(""),
                   theme_classic(), 
                   theme(axis.text.x = element_text(size = 20),
                         axis.title.x = element_text(size = 20),
                         legend.text = element_text(size = 20)))

## 6.1.- Morphological variables ####
### 6.1.1.- Height ####

h_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, height), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, height), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Height (m)") + plot_theme
h_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, height), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, height), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Height (m)") + plot_theme
h_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, height), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, height), method = "loess", fill = "#990000", col = "#990000") + ylab("Height (m)") + plot_theme

### 6.1.2.- d.b.h. ####

dbh_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, dbh), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, dbh), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("d.b.h. (cm)") + plot_theme
dbh_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, dbh), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, dbh), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("d.b.h. (cm)") + plot_theme
dbh_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, dbh), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, dbh), method = "loess", fill = "#990000", col = "#990000") + ylab("d.b.h. (cm)") + plot_theme

### 6.1.3.- C ####

c_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, percent_c), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, percent_c), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("C content (%)") + plot_theme
c_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, percent_c), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, percent_c), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("C content (%)") + plot_theme
c_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, percent_c), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, percent_c), method = "loess", fill = "#990000", col = "#990000") + ylab("C content (%)") + plot_theme

### 6.1.4.- N ####

n_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, percent_n), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, percent_n), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("N content (%)") + plot_theme
n_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, percent_n), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, percent_n), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("N content (%)") + plot_theme
n_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, percent_n), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, percent_n), method = "loess", fill = "#990000", col = "#990000") + ylab("N content (%)") + plot_theme

### 6.1.5.- C:N ####

cn_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, cn), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, cn), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("C:N ratio") + plot_theme
cn_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, cn), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, cn), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("C:N ratio") + plot_theme
cn_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, cn), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, cn), method = "loess", fill = "#990000", col = "#990000") + ylab("C:N ratio") + plot_theme

### 6.1.6.- SLA ####

sla_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, sla_22), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, sla_22), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("SLA") + plot_theme
sla_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, sla_22), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, sla_22), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("SLA") + plot_theme
sla_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, sla_22), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, sla_22), method = "loess", fill = "#990000", col = "#990000") + ylab("SLA") + plot_theme

### 6.1.7.- Age ####

age_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, age), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, age), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Age (years)") + plot_theme
age_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, age), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, age), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Age (years)") + plot_theme
age_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, age), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, age), method = "loess", fill = "#990000", col = "#990000") + ylab("Age (years)") + plot_theme

### 6.1.8.- Hegyi Index ####

hegyi_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, hegyi_index), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, hegyi_index), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Hegyi Index") + plot_theme
hegyi_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, hegyi_index), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, hegyi_index), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Hegyi Index") + plot_theme
hegyi_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, hegyi_index), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, hegyi_index), method = "loess", fill = "#990000", col = "#990000") + ylab("Hegyi Index") + plot_theme

## 6.2.- Physiological variables ####
### 6.2.1.- LWC ####

wc_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, wc_22), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, wc_22), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("LWC (%)") + plot_theme
wc_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, wc_22), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, wc_22), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("LWC (%)") + plot_theme
wc_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, wc_22), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, wc_22), method = "loess", fill = "#990000", col = "#990000") + ylab("LWC (%)") + plot_theme

### 6.2.2.- Chl. ####

chl_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, total_chl_fw_22), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, total_chl_fw_22), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab(expression(paste("Chl. (μg g"^"-1", ")"))) + plot_theme
chl_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, total_chl_fw_22), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, total_chl_fw_22), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab(expression(paste("Chl. (μg g"^"-1", ")"))) + plot_theme
chl_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, total_chl_fw_22), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, total_chl_fw_22), method = "loess", fill = "#990000", col = "#990000") + ylab(expression(paste("Chl. (μg g"^"-1", ")"))) + plot_theme

### 6.2.3.- Chl. a/b ####

chlab_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, chla_chlb_22), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, chla_chlb_22), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Chl. a/b") + plot_theme
chlab_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, chla_chlb_22), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, chla_chlb_22), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Chl. a/b") + plot_theme
chlab_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, chla_chlb_22), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, chla_chlb_22), method = "loess", fill = "#990000", col = "#990000") + ylab("Chl. a/b") + plot_theme

### 6.2.4.- Chl. a/b ####

xc_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, xc_fw_22), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, xc_fw_22), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab(expression(paste("Caroten. (μg g"^"-1", ")"))) + plot_theme
xc_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, xc_fw_22), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, xc_fw_22), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab(expression(paste("Caroten. (μg g"^"-1", ")"))) + plot_theme
xc_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, xc_fw_22), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, xc_fw_22), method = "loess", fill = "#990000", col = "#990000") + ylab(expression(paste("Caroten. (μg g"^"-1", ")"))) + plot_theme

### 6.2.5.- Chl. / xc ####

chlxc_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, chl_xc_22), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, chl_xc_22), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Chl. / car.") + plot_theme
chlxc_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, chl_xc_22), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, chl_xc_22), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Chl. / car.") + plot_theme
chlxc_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, chl_xc_22), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, chl_xc_22), method = "loess", fill = "#990000", col = "#990000") + ylab("Chl. / car.") + plot_theme

### 6.2.6.- d13C ####

d13c_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, leaf_d13c), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, leaf_d13c), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab(bquote("δ"~C^13~"(‰)")) + plot_theme
d13c_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, leaf_d13c), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, leaf_d13c), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab(bquote("δ"~C^13~"(‰)")) + plot_theme
d13c_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, leaf_d13c), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, leaf_d13c), method = "loess", fill = "#990000", col = "#990000") + ylab(bquote("δ"~C^13~"(‰)")) + plot_theme

### 6.2.7.- d15N ####

d15n_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, leaf_d15n), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, leaf_d15n), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab(bquote("δ"~N^15~"(‰)")) + plot_theme
d15n_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, leaf_d15n), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, leaf_d15n), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab(bquote("δ"~N^15~"(‰)")) + plot_theme
d15n_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, leaf_d15n), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, leaf_d15n), method = "loess", fill = "#990000", col = "#990000") + ylab(bquote("δ"~N^15~"(‰)")) + plot_theme

### 6.2.8.- d18O ####

d18o_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, leaf_d18o_corrected), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, leaf_d18o_corrected), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab(bquote("δ"~O^18~"(‰)")) + plot_theme
d18o_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, leaf_d18o_corrected), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, leaf_d18o_corrected), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab(bquote("δ"~O^18~"(‰)")) + plot_theme
d18o_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, leaf_d18o_corrected), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, leaf_d18o_corrected), method = "loess", fill = "#990000", col = "#990000") + ylab(bquote("δ"~O^18~"(‰)")) + plot_theme

## 6.3.- Whole-tree variables ####

### 6.3.1.- BAI 1980 ####

bai80_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, mean_1980), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, mean_1980), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + plot_theme
bai80_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, mean_1980), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, mean_1980), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + plot_theme
bai80_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, mean_1980), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, mean_1980), method = "loess", fill = "#990000", col = "#990000") + ylab(expression(paste("BAI80 (mm² year"^"-1", ")"))) + plot_theme

### 6.3.2.- BAI05 ####

bai05_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, mean_05), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, mean_05), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab(expression(paste("BAI05 (mm² year"^"-1", ")"))) + plot_theme
bai05_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, mean_05), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, mean_05), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab(expression(paste("BAI05 (mm² year"^"-1", ")"))) + plot_theme
bai05_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, mean_05), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, mean_05), method = "loess", fill = "#990000", col = "#990000") + ylab(expression(paste("BAI05 (mm² year"^"-1", ")"))) + plot_theme

### 6.3.3.- Rt12 ####

rt12_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, Rt12), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, Rt12), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Rt 12") + plot_theme
rt12_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, Rt12), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, Rt12), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Rt 12") + plot_theme
rt12_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, Rt12), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, Rt12), method = "loess", fill = "#990000", col = "#990000") + ylab("Rt 12") + plot_theme

### 6.3.4.- Rt17 ####

rt17_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, Rt17), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, Rt17), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Rt 17") + plot_theme
rt17_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, Rt17), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, Rt17), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Rt 17") + plot_theme
rt17_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, Rt17), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, Rt17), method = "loess", fill = "#990000", col = "#990000") + ylab("Rt 17") + plot_theme

### 6.3.5.- Rt22 ####

rt22_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, Rt22), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, Rt22), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Rt 22") + plot_theme
rt22_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, Rt22), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, Rt22), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Rt 22") + plot_theme
rt22_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, Rt22), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, Rt22), method = "loess", fill = "#990000", col = "#990000") + ylab("Rt 22") + plot_theme

### 6.3.6.- Rs12 ####

rs12_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, Rs12), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, Rs12), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Rs 12") + plot_theme
rs12_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, Rs12), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, Rs12), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Rs 12") + plot_theme
rs12_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, Rs12), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, Rs12), method = "loess", fill = "#990000", col = "#990000") + ylab("Rs 12") + plot_theme

### 6.3.7.- Rs17 ####

rs17_aa <- ggplot(aa_target) + geom_point(aes(mean_def_obs, Rs17), colour = "#785EF0") +
  geom_smooth(aes(mean_def_obs, Rs17), method = "loess", fill = "#785EF0", col = "#785EF0") + ylab("Rs 17") + plot_theme
rs17_ps <- ggplot(ps_target) + geom_point(aes(mean_def_obs, Rs17), colour = "#FFB000") +
  geom_smooth(aes(mean_def_obs, Rs17), method = "loess", fill = "#FFB000", col = "#FFB000") + ylab("Rs 17") + plot_theme
rs17_pp <- ggplot(pp_target) + geom_point(aes(mean_def_obs, Rs17), colour = "#990000") +
  geom_smooth(aes(mean_def_obs, Rs17), method = "loess", fill = "#990000", col = "#990000") + ylab("Rs 17") + plot_theme

# 8.- Plotting ####

tiff("04_figures/44_06_Scatters_logy_aa.tiff", units = "mm", width = 1200, height = 600,
     res = 400, compression = "lzw")
h_aa + dbh_aa + c_aa + n_aa + cn_aa + sla_aa + age_aa + hegyi_aa + 
  wc_aa + chl_aa + chlab_aa + xc_aa + chlxc_aa + d13c_aa + d15n_aa + d18o_aa + 
  bai80_aa + bai05_aa + rt12_aa + rt17_aa + rt22_aa + rs12_aa + rs17_aa + 
  plot_layout(ncol = 8)
dev.off()

tiff("04_figures/44_06_Scatters_logy_ps.tiff", units = "mm", width = 1200, height = 600,
     res = 400, compression = "lzw")
h_ps + dbh_ps + c_ps + n_ps + cn_ps + sla_ps + age_ps + hegyi_ps + 
  wc_ps + chl_ps + chlab_ps + xc_ps + chlxc_ps + d13c_ps + d15n_ps + d18o_ps + 
  bai80_ps + bai05_ps + rt12_ps + rt17_ps + rt22_ps + rs12_ps + rs17_ps + 
  plot_layout(ncol = 8)
dev.off()

tiff("04_figures/44_06_Scatters_logy_pp.tiff", units = "mm", width = 1200, height = 600,
     res = 400, compression = "lzw")
h_pp + dbh_pp + c_pp + n_pp + cn_pp + sla_pp + age_pp + hegyi_pp + 
  wc_pp + chl_pp + chlab_pp + xc_pp + chlxc_pp + d13c_pp + d15n_pp + d18o_pp + 
  bai80_pp + bai05_pp + rt12_pp + rt17_pp + rt22_pp + rs12_pp + rs17_pp + 
  plot_layout(ncol = 8)
dev.off()
