rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "cowplot") #list of packages
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

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(height, dbh, percent_c, percent_n, cn_ratio, sla_22, age, hegyi_index, 
                  wc_22, total_chl_fw_22, chla_chlb_22, chlor_a_fw_22, chlor_b_fw_22,
                  xc_fw_22, chl_xc_22, leaf_d13c, leaf_d15n, leaf_d18o_corrected, mean_1980, mean_05, 
                  Rt12, Rt17, Rt22, Rs12, Rs17, mean_def_obs, sp_id))

colnames(clean_target) <- c("Height", "d.b.h.", "C content", "N content", 
                            "Leaf C:N", "SLA", "Age", "Hegyi Index",  "LWC", 
                            "Chl.", "Chl. a / b", "Chl. a", "Chl. b", "Car.", 
                            "Chl. / Car.", "Leaf δ13C", "Leaf δ15N", "Leaf δ18O", 
                            "BAI 1980", "BAI 05", "Rt 2012", "Rt 2017", "Rt 2022",
                            "Rs 2012", "Rs 2017", "Defoliation", "sp_id")

# Filtering per species separately

aa_target <- clean_target %>% filter(sp_id == "Abialba") %>% 
  dplyr::select(-sp_id)
ps_target <- clean_target %>% filter(sp_id == "Pinsylv") %>% 
  dplyr::select(-sp_id)
pp_target <- clean_target %>% filter(sp_id == "Pinpine") %>% 
  dplyr::select(-sp_id)

# 6.- Correlograms ####
# First I need to remove na values from the correlogram 

## 6.1.- Abies alba ####

aa_target <- na.omit(aa_target)

### 6.1.1.- Size ####

aa_size <- aa_target %>% 
  dplyr::select(c("Height", "d.b.h.", "Age", "Hegyi Index")) %>% 
  cor()
aa_size_plot <- corrplot::corrplot.mixed(aa_size, upper = "ellipse")

### 6.1.2.- Leaf morphology ####

aa_leaf_morpho <- aa_target %>% 
  dplyr::select(c("C content", "Leaf C:N", "SLA")) %>% 
  cor()
aa_leaf_morpho_plot <- corrplot::corrplot.mixed(aa_leaf_morpho, upper = "ellipse")

### 6.1.3.- Leaf nutrients ####

aa_leaf_nutr <- aa_target %>% 
  dplyr::select(c("N content", "Leaf C:N", "Leaf δ15N")) %>% 
  cor()
aa_leaf_nutr_plot <- corrplot::corrplot.mixed(aa_leaf_nutr, upper = "ellipse")

### 6.1.4.- Leaf pigments ####

aa_photosynthesis <- aa_target %>% 
  dplyr::select(c("Chl.", "Chl. a / b", "Chl. a", "Chl. b", "Car.",
                "Chl. / Car.")) %>% 
  cor()
aa_photosynthesis_plot <- corrplot::corrplot.mixed(aa_photosynthesis, upper = "ellipse")

### 6.1.5.- WUE ####

aa_wue <- aa_target %>% 
  dplyr::select(c("LWC", "Leaf δ13C", "Leaf δ18O")) %>% 
  cor()
aa_wue_plot <- corrplot::corrplot.mixed(aa_wue, upper = "ellipse")

### 6.1.6.- Growth ####

aa_growth <- aa_target %>% 
  dplyr::select(c("BAI 1980", "BAI 05", "Rt 2012", "Rt 2017", "Rt 2022",
                "Rs 2012", "Rs 2017")) %>% 
  cor()
aa_growth_plot <- corrplot::corrplot.mixed(aa_growth, upper = "ellipse")

#### 6.1.7.- Plotting ####

tiff("04_figures/30_04_grouped_correlograms_aa.tiff", 
     units = "mm", width = 200, height = 300,
     res = 700, compression = "lzw")

par(mfrow = c(3, 2))
corrplot::corrplot.mixed(aa_size, upper = "ellipse", main = "Size")
corrplot::corrplot.mixed(aa_leaf_morpho, upper = "ellipse", main = "Leaf morphology")
corrplot::corrplot.mixed(aa_leaf_nutr, upper = "ellipse", main = "Leaf nutrients")
corrplot::corrplot.mixed(aa_photosynthesis, upper = "ellipse", main = "Photosynthesis")
corrplot::corrplot.mixed(aa_wue, upper = "ellipse", main = "WUE")
corrplot::corrplot.mixed(aa_growth, upper = "ellipse", main = "Growth")

dev.off()

## 6.2.- Pinus sylvestris ####

ps_target <- na.omit(ps_target)

### 6.2.1.- Size ####

ps_size <- ps_target %>% 
  dplyr::select(c("Height", "d.b.h.", "Age", "Hegyi Index")) %>% 
  cor()
ps_size_plot <- corrplot::corrplot.mixed(ps_size, upper = "ellipse")

### 6.2.2.- Leaf morphology ####

ps_leaf_morpho <- ps_target %>% 
  dplyr::select(c("C content", "Leaf C:N", "SLA")) %>% 
  cor()
ps_leaf_morpho_plot <- corrplot::corrplot.mixed(ps_leaf_morpho, upper = "ellipse")

### 6.2.3.- Leaf nutrients ####

ps_leaf_nutr <- ps_target %>% 
  dplyr::select(c("N content", "Leaf C:N", "Leaf δ15N")) %>% 
  cor()
ps_leaf_nutr_plot <- corrplot::corrplot.mixed(ps_leaf_nutr, upper = "ellipse")

### 6.2.4.- Leaf pigments ####

ps_photosynthesis <- ps_target %>% 
  dplyr::select(c("Chl.", "Chl. a / b", "Chl. a", "Chl. b", "Car.",
                  "Chl. / Car.")) %>% 
  cor()
ps_photosynthesis_plot <- corrplot::corrplot.mixed(ps_photosynthesis, upper = "ellipse")

### 6.2.5.- WUE ####

ps_wue <- ps_target %>% 
  dplyr::select(c("LWC", "Leaf δ13C", "Leaf δ18O")) %>% 
  cor()
ps_wue_plot <- corrplot::corrplot.mixed(ps_wue, upper = "ellipse")

### 6.2.6.- Growth ####

ps_growth <- ps_target %>% 
  dplyr::select(c("BAI 1980", "BAI 05", "Rt 2012", "Rt 2017", "Rt 2022",
                  "Rs 2012", "Rs 2017")) %>% 
  cor()
ps_growth_plot <- corrplot::corrplot.mixed(ps_growth, upper = "ellipse")

### 6.2.7.- Plotting ####

tiff("04_figures/30_04_grouped_correlograms_ps.tiff", 
     units = "mm", width = 200, height = 300,
     res = 700, compression = "lzw")

par(mfrow = c(3, 2))
corrplot::corrplot.mixed(ps_size, upper = "ellipse", main = "Size")
corrplot::corrplot.mixed(ps_leaf_morpho, upper = "ellipse", main = "Leaf morphology")
corrplot::corrplot.mixed(ps_leaf_nutr, upper = "ellipse", main = "Leaf nutrients")
corrplot::corrplot.mixed(ps_photosynthesis, upper = "ellipse", main = "Photosynthesis")
corrplot::corrplot.mixed(ps_wue, upper = "ellipse", main = "WUE")
corrplot::corrplot.mixed(ps_growth, upper = "ellipse", main = "Growth")

dev.off()

## 6.3.- Pinus pinea ####

pp_target <- na.omit(pp_target)

### 6.3.1.- Size ####

pp_size <- pp_target %>% 
  dplyr::select(c("Height", "d.b.h.", "Age", "Hegyi Index")) %>% 
  cor()
pp_size_plot <- corrplot::corrplot.mixed(pp_size, upper = "ellipse")

### 6.3.2.- Leaf morphology ####

pp_leaf_morpho <- pp_target %>% 
  dplyr::select(c("C content", "Leaf C:N", "SLA")) %>% 
  cor()
pp_leaf_morpho_plot <- corrplot::corrplot.mixed(pp_leaf_morpho, upper = "ellipse")

### 6.3.3.- Leaf nutrients ####

pp_leaf_nutr <- pp_target %>% 
  dplyr::select(c("N content", "Leaf C:N", "Leaf δ15N")) %>% 
  cor()
pp_leaf_nutr_plot <- corrplot::corrplot.mixed(pp_leaf_nutr, upper = "ellipse")

### 6.3.4.- Leaf pigments ####

pp_photosynthesis <- pp_target %>% 
  dplyr::select(c("Chl.", "Chl. a / b", "Chl. a", "Chl. b", "Car.",
                  "Chl. / Car.")) %>% 
  cor()
pp_photosynthesis_plot <- corrplot::corrplot.mixed(pp_photosynthesis, upper = "ellipse")

### 6.3.5.- WUE ####

pp_wue <- pp_target %>% 
  dplyr::select(c("LWC", "Leaf δ13C", "Leaf δ18O")) %>% 
  cor()
pp_wue_plot <- corrplot::corrplot.mixed(pp_wue, upper = "ellipse")

### 6.3.6.- Growth ####

pp_growth <- pp_target %>% 
  dplyr::select(c("BAI 1980", "BAI 05", "Rt 2012", "Rt 2017", "Rt 2022",
                  "Rs 2012", "Rs 2017")) %>% 
  cor()
pp_growth_plot <- corrplot::corrplot.mixed(pp_growth, upper = "ellipse")

### 6.3.7.- Plotting ####

tiff("04_figures/30_04_grouped_correlograms_pp.tiff", 
     units = "mm", width = 200, height = 300,
     res = 700, compression = "lzw")

par(mfrow = c(3, 2))
corrplot::corrplot.mixed(pp_size, upper = "ellipse", main = "Size")
corrplot::corrplot.mixed(pp_leaf_morpho, upper = "ellipse", main = "Leaf morphology")
corrplot::corrplot.mixed(pp_leaf_nutr, upper = "ellipse", main = "Leaf nutrients")
corrplot::corrplot.mixed(pp_photosynthesis, upper = "ellipse", main = "Photosynthesis")
corrplot::corrplot.mixed(pp_wue, upper = "ellipse", main = "WUE")
corrplot::corrplot.mixed(pp_growth, upper = "ellipse", main = "Growth")

dev.off()
