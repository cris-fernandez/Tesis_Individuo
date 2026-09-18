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

clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "Abialba", "Pinsylv", "Pinpine"),
         spot_status = fct_relevel(spot_status, "Coldspot", "Hotspot")) %>% 
  filter(mean_def_obs < 100)


# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean,
         "2017" = wood_d13c_17,
         "2022" = wood_d13c_22) %>% 
  dplyr::select(c("2017", "2022", mean_def_obs, tree_number, 
                  sp_id, spot_status, vigor_id))

summary(clean_target)

# Pivot longer

long_target <- clean_target %>% 
  pivot_longer(cols = c("2017", "2022"),
               names_to = "year",
               values_to = "wood_d13c")

# 6.- Filtering per species ####
# 
aa_target <- long_target %>% filter(sp_id == "Abialba")
ps_target <- long_target %>% filter(sp_id == "Pinsylv")
pp_target <- long_target %>% filter(sp_id == "Pinpine")

# 7.- Figure ####
## 7.1.- Aa ####
aa_d13c <- ggplot(aa_target) + 
  geom_jitter(aes(x = year, y = wood_d13c, col = spot_status),
              width = 0.15, size = 1.2) + 
  geom_smooth(
    aes(x = year, y = wood_d13c,
        colour = spot_status,
        group = spot_status),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1
  ) + 
  scale_color_manual(breaks = c("Coldspot", "Hotspot"),
                     values = c("Coldspot" = "#2274A5",
                                "Hotspot" = "#D71515"),
                     labels = c("Non-declining stands",
                                "Declining stands"),
                     name = "") + 
  labs(tag = "A") +
  ylab(bquote("Wood δ"~C^13~"(‰)")) +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        # axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 7.2.- Ps ####
ps_d13c <- ggplot(ps_target) + 
  geom_jitter(aes(x = year, y = wood_d13c, col = spot_status),
              width = 0.15, size = 1.2) + 
  geom_smooth(
    aes(x = year, y = wood_d13c,
        colour = spot_status,
        group = spot_status),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1
  ) + 
  scale_color_manual(breaks = c("Coldspot", "Hotspot"),
                     values = c("Coldspot" = "#2274A5",
                                "Hotspot" = "#D71515"),
                     labels = c("Non-declining stands",
                                "Declining stands"),
                     name = "") + 
  labs(tag = "B") +
  ylab("") +
  xlab("") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        # axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 7.3.- Pp ####
pp_d13c <- ggplot(pp_target) + 
  geom_jitter(aes(x = year, y = wood_d13c, col = spot_status),
              width = 0.15, size = 1.2) + 
  geom_smooth(
    aes(x = year, y = wood_d13c,
        colour = spot_status,
        group = spot_status),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 1
  ) + 
  scale_color_manual(breaks = c("Coldspot", "Hotspot"),
                     values = c("Coldspot" = "#2274A5",
                                "Hotspot" = "#D71515"),
                     labels = c("Non-declining stands",
                                "Declining stands"),
                     name = "") + 
  labs(tag = "C") +
  ylab(bquote("Wood δ"~C^13~"(‰)")) +
  xlab("") + 
  guides(colour = "none") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        # axis.text.x = element_blank(),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

# Plotting

tiff("04_figures/97_01_wood_d13c.tiff", units = "mm", width = 150, height = 150,
     res = 800, compression = "lzw")
aa_d13c + ps_d13c + pp_d13c + guide_area() +
  plot_layout(guides = 'collect', ncol = 2)
dev.off()