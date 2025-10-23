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

aa_target <- clean_target %>% filter(sp_id == "Abialba")
ps_target <- clean_target %>% filter(sp_id == "Pinsylv")
pp_target <- clean_target %>% filter(sp_id == "Pinpine")

plot_theme <- list(ylab(""),
                   theme_classic(), 
                   theme(axis.text.x = element_text(size = 20),
                           axis.title.x = element_text(size = 20),
                           legend.text = element_text(size = 20)))

## 6.1.- Morphological variables ####
### 6.1.1.- Height ####

h_aa <- ggplot(aa_target) + geom_histogram(aes(height), fill = "#785EF0") +
  xlab("Height (m)") + plot_theme
h_ps <- ggplot(ps_target) + geom_histogram(aes(height), fill = "#FFB000") +
  xlab("Height (m)") + plot_theme
h_pp <- ggplot(pp_target) + geom_histogram(aes(height), fill = "#990000") +
  xlab("Height (m)") + plot_theme

### 6.1.2.- d.b.h. ####

dbh_aa <- ggplot(aa_target) + geom_histogram(aes(dbh), fill = "#785EF0") +
  xlab("d.b.h. (cm)") + plot_theme
dbh_ps <- ggplot(ps_target) + geom_histogram(aes(dbh), fill = "#FFB000") +
  xlab("d.b.h. (cm)") + plot_theme
dbh_pp <- ggplot(pp_target) + geom_histogram(aes(dbh), fill = "#990000") +
  xlab("d.b.h. (cm)") + plot_theme

### 6.1.3.- C ####

c_aa <- ggplot(aa_target) + geom_histogram(aes(percent_c), fill = "#785EF0") +
  xlab("C content (%)") + plot_theme
c_ps <- ggplot(ps_target) + geom_histogram(aes(percent_c), fill = "#FFB000") +
  xlab("C content (%)") + plot_theme
c_pp <- ggplot(pp_target) + geom_histogram(aes(percent_c), fill = "#990000") +
  xlab("C content (%)") + plot_theme

### 6.1.4.- N ####

n_aa <- ggplot(aa_target) + geom_histogram(aes(percent_n), fill = "#785EF0") +
  xlab("N content (%)") + plot_theme
n_ps <- ggplot(ps_target) + geom_histogram(aes(percent_n), fill = "#FFB000") +
  xlab("N content (%)") + plot_theme
n_pp <- ggplot(pp_target) + geom_histogram(aes(percent_n), fill = "#990000") +
  xlab("N content (%)") + plot_theme

### 6.1.5.- C:N ####

cn_aa <- ggplot(aa_target) + geom_histogram(aes(cn), fill = "#785EF0") +
  xlab("C:N ratio") + plot_theme
cn_ps <- ggplot(ps_target) + geom_histogram(aes(cn), fill = "#FFB000") +
  xlab("C:N ratio") + plot_theme
cn_pp <- ggplot(pp_target) + geom_histogram(aes(cn), fill = "#990000") +
  xlab("C:N ratio") + plot_theme

### 6.1.6.- SLA ####

sla_aa <- ggplot(aa_target) + geom_histogram(aes(sla_22), fill = "#785EF0") +
  xlab("SLA") + plot_theme
sla_ps <- ggplot(ps_target) + geom_histogram(aes(sla_22), fill = "#FFB000") +
  xlab("SLA") + plot_theme
sla_pp <- ggplot(pp_target) + geom_histogram(aes(sla_22), fill = "#990000") +
  xlab("SLA") + plot_theme

### 6.1.7.- Age ####

age_aa <- ggplot(aa_target) + geom_histogram(aes(age), fill = "#785EF0") +
  xlab("Age (years)") + plot_theme
age_ps <- ggplot(ps_target) + geom_histogram(aes(age), fill = "#FFB000") +
  xlab("Age (years)") + plot_theme
age_pp <- ggplot(pp_target) + geom_histogram(aes(age), fill = "#990000") +
  xlab("Age (years)") + plot_theme

### 6.1.8.- Hegyi Index ####

hegyi_aa <- ggplot(aa_target) + geom_histogram(aes(hegyi_index), fill = "#785EF0") +
  xlab("Hegyi Index") + plot_theme
hegyi_ps <- ggplot(ps_target) + geom_histogram(aes(hegyi_index), fill = "#FFB000") +
  xlab("Hegyi Index") + plot_theme
hegyi_pp <- ggplot(pp_target) + geom_histogram(aes(hegyi_index), fill = "#990000") +
  xlab("Hegyi Index") + plot_theme

## 7.1.- Physiological variables ####
### 7.1.1.- LWC ####

wc_aa <- ggplot(aa_target) + geom_histogram(aes(wc_22), fill = "#785EF0") +
  xlab("LWC (%)") + plot_theme
wc_ps <- ggplot(ps_target) + geom_histogram(aes(wc_22), fill = "#FFB000") +
  xlab("LWC (%)") + plot_theme
wc_pp <- ggplot(pp_target) + geom_histogram(aes(wc_22), fill = "#990000") +
  xlab("LWC (%)") + plot_theme

### 7.1.2.- Chl. ####

chl_aa <- ggplot(aa_target) + geom_histogram(aes(total_chl_fw_22), fill = "#785EF0") +
  xlab("LWC (%)") + plot_theme
## 3.3.- Chl. a/b ####

chlab_plot <- ggplot() + geom_histogram(aes(clean_target$chla_chlb_22))

## 3.4.- Carotenoids ####

xc_plot <- ggplot() + geom_histogram(aes(clean_target$xc_fw_22))

## 3.5.- Chl. / xc ####

chlxc_plot <- ggplot() + geom_histogram(aes(clean_target$chl_xc_22))

## 3.6.- d13C ####

d13c_plot <- ggplot() + geom_histogram(aes(clean_target$leaf_d13c))

## 3.7.- d15N ####

d15n_plot <- ggplot() + geom_histogram(aes(clean_target$leaf_d15n))

## 3.8.- d18O ####

d18o_plot <- ggplot() + geom_histogram(aes(clean_target$leaf_d18o_corrected))

# 4.- Whole-tree variables ####

## 4.1.- BAI 1980 ####

bai80_plot <- ggplot() + geom_histogram(aes(clean_target$mean_1980))

## 4.2.- BAI05 ####

bai05_plot <- ggplot() + geom_histogram(aes(clean_target$mean_05))

## 4.3.- Rt12 ####

rt12_plot <- ggplot() + geom_histogram(aes(clean_target$Rt12))

## 4.4.- Rt17 ####

rt17_plot <- ggplot() + geom_histogram(aes(clean_target$Rt17))

## 4.5.- Rt22 ####

rt22_plot <- ggplot() + geom_histogram(aes(clean_target$Rt22))

## 4.6.- Rs12 ####

rs12_plot <- ggplot() + geom_histogram(aes(clean_target$Rs12))

## 4.7.- Rs17 ####

rs17_plot <- ggplot() + geom_histogram(aes(clean_target$Rs17))

# 8.- Plotting ####

tiff("04_figures/44_01_Distribs_morpho.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
h_plot + dbh_plot + c_plot + n_plot + cn_plot + sla_plot + age_plot + hegyi_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

tiff("04_figures/44_01_Distribs_physio.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
wc_plot + chl_plot + chlab_plot + xc_plot + chlxc_plot + d13c_plot + d15n_plot + d18o_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

tiff("04_figures/44_01_Distribs_whole2.tiff", units = "mm", width = 400, height = 400,
     res = 400, compression = "lzw")
bai80_plot + bai05_plot + rt12_plot + rt17_plot + rt22_plot + rs12_plot + rs17_plot + 
  guide_area() + plot_layout(ncol = 3, guides = "collect")
dev.off()

