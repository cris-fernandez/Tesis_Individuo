rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "lme4", "lmerTest", "emmeans", "mgcv", "broom.mixed", "xlsx") #list of packages
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

# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3))

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

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target$cn <- clean_target$percent_c / clean_target$percent_n

clean_target <- clean_target %>% filter(mean_def_obs < 100)

clean_target$site <- as.factor(clean_target$site)

# Outlayers deletion:

clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 4783, NA, clean_target$mean_1980)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 60 & clean_target$sp_id == "Abialba",
                                    NA, clean_target$mean_def_obs)
clean_target$sla_22 <- ifelse(clean_target$sla_22 > 99 & clean_target$sp_id == "Pinsylv",
                              NA, clean_target$sla_22)
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 < 75 & clean_target$sp_id == "Pinsylv",
                                       NA, clean_target$total_chl_fw_22)
clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 < 40 & clean_target$sp_id == "Pinpine",
                                       NA, clean_target$total_chl_fw_22)
clean_target$mean_def_obs <- ifelse(clean_target$mean_def_obs > 58 & clean_target$sp_id == "Pinpine",
                                    NA, clean_target$mean_def_obs)
clean_target$mean_1980 <- ifelse(clean_target$mean_1980 > 3000 & clean_target$sp_id == "Abialba" & clean_target$spot_status == "hotspot",
                                 NA, clean_target$mean_1980)


# Transforming spot status into a factor so it can be modellised:

clean_target$spot_status <- as.factor(clean_target$spot_status)

# Filtering by species: 
# Pinus pinean not needed as there is only one site...

aa_target <- clean_target %>% 
  filter(sp_id == "Abialba")
ps_target <- clean_target %>% 
  filter(sp_id == "Pinsylv")

# 5.- Abies alba residuals ####

## 5.1.- Height ####

model_aa_height <- lmer(height ~ spot_status + (1|plot_id), data = aa_target)
aa_height <- aa_target %>%
  mutate(fitted = fitted(model_aa_height),
         residual = residuals(model_aa_height))

## 5.2.- BAI80 ####

model_aa_bai80 <- lmer(mean_1980 ~ spot_status + (1|plot_id), data = aa_target)
aa_bai80 <- aa_target %>%
  filter(complete.cases(mean_1980)) %>% 
  mutate(fitted = fitted(model_aa_bai80),
         residual = residuals(model_aa_bai80))

## 5.3.- SLA ####

model_aa_sla <- lmer(sla_22 ~ spot_status + (1|plot_id), data = aa_target)
aa_sla <- aa_target %>%
  filter(complete.cases(sla_22)) %>% 
  mutate(fitted = fitted(model_aa_sla),
         residual = residuals(model_aa_sla))

## 5.4.- Leaf N ####

model_aa_n <- lmer(percent_n ~ spot_status + (1|plot_id), data = aa_target)
aa_n <- aa_target %>%
  filter(complete.cases(percent_n)) %>% 
  mutate(fitted = fitted(model_aa_n),
         residual = residuals(model_aa_n))

## 5.5.- Leaf d13C ####

model_aa_d13c <- lmer(leaf_d13c ~ spot_status + (1|plot_id), data = aa_target)
aa_d13c <- aa_target %>%
  filter(complete.cases(leaf_d13c)) %>% 
  mutate(fitted = fitted(model_aa_d13c),
         residual = residuals(model_aa_d13c))

## 5.6.- Leaf d18O ####

model_aa_d18o <- lmer(leaf_d18o_corrected ~ spot_status + (1|plot_id), data = aa_target)
aa_d18o <- aa_target %>%
  filter(complete.cases(leaf_d18o_corrected)) %>% 
  mutate(fitted = fitted(model_aa_d18o),
         residual = residuals(model_aa_d18o))

## 5.7.- Chl. ####

model_aa_chl <- lmer(total_chl_fw_22 ~ spot_status + (1|plot_id), data = aa_target)
aa_chl <- aa_target %>%
  filter(complete.cases(total_chl_fw_22)) %>% 
  mutate(fitted = fitted(model_aa_chl),
         residual = residuals(model_aa_chl))

## 5.8.- Car ####

model_aa_xc <- lmer(xc_fw_22 ~ spot_status + (1|plot_id), data = aa_target)
aa_xc <- aa_target %>%
  filter(complete.cases(xc_fw_22)) %>% 
  mutate(fitted = fitted(model_aa_xc),
         residual = residuals(model_aa_xc))


# 6.- Pinus sylvestris residuals ####

## 6.1.- Height ####

model_ps_height <- lmer(height ~ spot_status + (1|plot_id), data = ps_target)
ps_height <- ps_target %>%
  filter(complete.cases(height)) %>% 
  mutate(fitted = fitted(model_ps_height),
         residual = residuals(model_ps_height))

## 6.2.- BAI80 ####

model_ps_bai80 <- lmer(mean_1980 ~ spot_status + (1|plot_id), data = ps_target)
ps_bai80 <- ps_target %>%
  filter(complete.cases(mean_1980)) %>% 
  mutate(fitted = fitted(model_ps_bai80),
         residual = residuals(model_ps_bai80))

## 6.3.- SLA ####

model_ps_sla <- lmer(sla_22 ~ spot_status + (1|plot_id), data = ps_target)
ps_sla <- ps_target %>%
  filter(complete.cases(sla_22)) %>% 
  mutate(fitted = fitted(model_ps_sla),
         residual = residuals(model_ps_sla))

## 6.4.- Leaf N ####

model_ps_n <- lmer(percent_n ~ spot_status + (1|plot_id), data = ps_target)
ps_n <- ps_target %>%
  filter(complete.cases(percent_n)) %>% 
  mutate(fitted = fitted(model_ps_n),
         residual = residuals(model_ps_n))

## 6.5.- Leaf d13C ####

model_ps_d13c <- lmer(leaf_d13c ~ spot_status + (1|plot_id), data = ps_target)
ps_d13c <- ps_target %>%
  filter(complete.cases(leaf_d13c)) %>% 
  mutate(fitted = fitted(model_ps_d13c),
         residual = residuals(model_ps_d13c))

## 6.6.- Leaf d18O ####

model_ps_d18o <- lmer(leaf_d18o_corrected ~ spot_status + (1|plot_id), data = ps_target)
ps_d18o <- ps_target %>%
  filter(complete.cases(leaf_d18o_corrected)) %>% 
  mutate(fitted = fitted(model_ps_d18o),
         residual = residuals(model_ps_d18o))

## 6.7.- Chl. ####

model_ps_chl <- lmer(total_chl_fw_22 ~ spot_status + (1|plot_id), data = ps_target)
ps_chl <- ps_target %>%
  filter(complete.cases(total_chl_fw_22)) %>% 
  mutate(fitted = fitted(model_ps_chl),
         residual = residuals(model_ps_chl))

## 6.8.- Car ####

model_ps_xc <- lmer(xc_fw_22 ~ spot_status + (1|plot_id), data = ps_target)
ps_xc <- ps_target %>%
  filter(complete.cases(xc_fw_22)) %>% 
  mutate(fitted = fitted(model_ps_xc),
         residual = residuals(model_ps_xc))

# 7.- Abies alba plots ####

site_theme <- list(scale_y_continuous(labels = scales::label_number(accuracy = 0.1)),
                   theme_classic(),
                   theme(legend.position = "right",
                         legend.key.size = unit(2, "cm"),  
                         legend.title = element_blank(),
                         axis.text.x = element_text(size = 20,
                                                    angle = 45,
                                                    hjust = 1),
                         axis.title.x = element_text(size = 22,
                                                     vjust = 0.5),
                         axis.text.y = element_text(size = 20),
                         axis.title.y = element_text(size = 22),
                         legend.text = element_text(size = 25),
                         plot.tag = element_text(size = 25,
                                                 face = "bold"),
                         plot.title = element_text(size = 22,
                                                   vjust= 1.9)))

## 7.1.- Height ####

plot_aa_height <- ggplot(aa_height) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#785EF0", alpha = 0.7) +
  scale_x_discrete(labels = c("Hue-Abialba" = "Huesca",
                              "Nav-Abialba" = "Navarra")) + 
  xlab("") +
  ylab("Residuals") +
  labs(tag = "(a)",
       title = expression(paste("Tree height"))) +
  site_theme + 
  theme(axis.text.x = element_blank(),
        plot.title = element_text(vjust= 1.9)) + 
  guides(shape = "none")

## 7.2.- BAI80 ####

plot_aa_bai80 <- ggplot(aa_bai80) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#785EF0", alpha = 0.7) +
  scale_x_discrete(labels = c("Hue-Abialba" = "Huesca",
                              "Nav-Abialba" = "Navarra")) + 
  xlab("") +
  ylab("Residuals") +
  labs(tag = "(b)",
       title = expression(paste("BAI80"))) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 7.3.- SLA ####

plot_aa_sla <- ggplot(aa_sla) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#785EF0", alpha = 0.7) +
  scale_x_discrete(labels = c("Hue-Abialba" = "Huesca",
                              "Nav-Abialba" = "Navarra")) + 
  xlab("") +
  ylab("Residuals") +
  labs(tag = "(c)",
       title = expression(paste("SLA"))) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 7.4.- Leaf N ####

plot_aa_n <- ggplot(aa_n) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#785EF0", alpha = 0.7) +
  scale_x_discrete(labels = c("Hue-Abialba" = "Huesca",
                              "Nav-Abialba" = "Navarra")) + 
  xlab("Study sites") +
  ylab("Residuals") +
  labs(tag = "(d)",
       title = expression(paste("Leaf N"))) +
  site_theme + 
  guides(shape = "site") + 
  theme(axis.title.x = element_text(size = 22,
                                    vjust = -3.2))

## 7.5.- Leaf d13C ####

plot_aa_d13c <- ggplot(aa_d13c) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#785EF0", alpha = 0.7) +
  scale_x_discrete(labels = c("Hue-Abialba" = "Huesca",
                              "Nav-Abialba" = "Navarra")) + 
  xlab("") +
  ylab("") +
  labs(tag = "(e)",
       title = bquote("δ"^13~C~"(‰)")) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 7.6.- Leaf d18O ####

plot_aa_d18o <- ggplot(aa_d18o) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#785EF0", alpha = 0.7) +
  scale_x_discrete(labels = c("Hue-Abialba" = "Huesca",
                              "Nav-Abialba" = "Navarra")) + 
  xlab("") +
  ylab("") +
  labs(tag = "(f)",
       title = bquote("Δ"^18~O~"(‰)")) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 7.7.- Chl. ####

plot_aa_chl <- ggplot(aa_chl) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#785EF0", alpha = 0.7) +
  scale_x_discrete(labels = c("Hue-Abialba" = "Huesca",
                              "Nav-Abialba" = "Navarra")) + 
  xlab("") +
  ylab("") +
  labs(tag = "(g)",
       title = expression(paste("Chlorophyll"))) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 7.8.- Car. ####

plot_aa_xc <- ggplot(aa_xc) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#785EF0", alpha = 0.7) +
  scale_x_discrete(labels = c("Hue-Abialba" = "Huesca",
                              "Nav-Abialba" = "Navarra")) + 
  xlab("Study sites") +
  ylab("") +
  labs(tag = "(h)",
       title = expression(paste("Carotenoids"))) +
  site_theme + 
  guides(shape = "none") + 
  theme(axis.title.x = element_text(size = 22,
                                    vjust = -3.2))

# 8.- Pinus sylvestris plots ####

## 8.1.- Height ####

plot_ps_height <- ggplot(ps_height) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#FFB000", alpha = 0.7) +
  scale_x_discrete(labels = c("Nav-Pinsylv" = "Navarra",
                              "Ter-Pinsylv" = "Teruel",
                              "Gua-Pinsylv" = "Guadalajara",
                              "Mad-Pinsylv" = "Madrid")) + 
  xlab("") +
  ylab("Residuals") +
  labs(tag = "(a)",
       title = expression(paste("Tree height"))) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 8.2.- BAI80 ####

plot_ps_bai80 <- ggplot(ps_bai80) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#FFB000", alpha = 0.7) +
  scale_x_discrete(labels = c("Nav-Pinsylv" = "Navarra",
                              "Ter-Pinsylv" = "Teruel",
                              "Gua-Pinsylv" = "Guadalajara",
                              "Mad-Pinsylv" = "Madrid")) + 
  xlab("") +
  ylab("Residuals") +
  labs(tag = "(b)",
       title = expression(paste("BAI80"))) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 8.3.- SLA ####

plot_ps_sla <- ggplot(ps_sla) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#FFB000", alpha = 0.7) +
  scale_x_discrete(labels = c("Nav-Pinsylv" = "Navarra",
                              "Ter-Pinsylv" = "Teruel",
                              "Gua-Pinsylv" = "Guadalajara",
                              "Mad-Pinsylv" = "Madrid")) + 
  xlab("") +
  ylab("Residuals") +
  labs(tag = "(c)",
       title = expression(paste("SLA"))) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 8.4.- Leaf N ####

plot_ps_n <- ggplot(ps_n) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#FFB000", alpha = 0.7) +
  scale_x_discrete(labels = c("Nav-Pinsylv" = "Navarra",
                              "Ter-Pinsylv" = "Teruel",
                              "Gua-Pinsylv" = "Guadalajara",
                              "Mad-Pinsylv" = "Madrid")) + 
  xlab("Study sites") +
  ylab("Residuals") +
  labs(tag = "(d)",
       title = expression(paste("Leaf N"))) +
  site_theme + 
  guides(shape = "none")

## 8.5.- Leaf d13C ####

plot_ps_d13c <- ggplot(ps_d13c) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#FFB000", alpha = 0.7) +
  scale_x_discrete(labels = c("Nav-Pinsylv" = "Navarra",
                              "Ter-Pinsylv" = "Teruel",
                              "Gua-Pinsylv" = "Guadalajara",
                              "Mad-Pinsylv" = "Madrid")) + 
  xlab("") +
  ylab("") +
  labs(tag = "(e)",
       title = bquote("δ"^13~C~"(‰)")) +
  site_theme +
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 8.6.- Leaf d18O ####

plot_ps_d18o <- ggplot(ps_d18o) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#FFB000", alpha = 0.7) +
  scale_x_discrete(labels = c("Nav-Pinsylv" = "Navarra",
                              "Ter-Pinsylv" = "Teruel",
                              "Gua-Pinsylv" = "Guadalajara",
                              "Mad-Pinsylv" = "Madrid")) + 
  xlab("") +
  ylab("") +
  labs(tag = "(f)",
       title = bquote("Δ"^18~O~"(‰)")) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 8.7.- Chl. ####

plot_ps_chl <- ggplot(ps_chl) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#FFB000", alpha = 0.7) +
  scale_x_discrete(labels = c("Nav-Pinsylv" = "Navarra",
                              "Ter-Pinsylv" = "Teruel",
                              "Gua-Pinsylv" = "Guadalajara",
                              "Mad-Pinsylv" = "Madrid")) + 
  xlab("") +
  ylab("") +
  labs(tag = "(g)",
       title = expression(paste("Chlorophyll"))) +
  site_theme + 
  theme(axis.text.x = element_blank()) + 
  guides(shape = "none")

## 8.8.- Car. ####

plot_ps_xc <- ggplot(ps_xc) + 
  geom_boxplot(aes(x = pair_id, y = residual), 
               fill = "#FFB000", alpha = 0.7) +
  scale_x_discrete(labels = c("Nav-Pinsylv" = "Navarra",
                              "Ter-Pinsylv" = "Teruel",
                              "Gua-Pinsylv" = "Guadalajara",
                              "Mad-Pinsylv" = "Madrid")) + 
  xlab("Study sites") +
  ylab("") +
  labs(tag = "(h)",
       title = expression(paste("Carotenoids"))) +
  site_theme + 
  guides(shape = "none")

# 9.- Plotting and saving ####

tiff("04_figures/63_15_Residuals_sitenospot_random_Abialba.tiff", units = "mm", width = 370, height = 500,
     res = 400, compression = "lzw")
plot_aa_height + plot_aa_d13c + 
  plot_aa_bai80 + plot_aa_d18o +
  plot_aa_sla + plot_aa_chl + 
  plot_aa_n + plot_aa_xc +  
  # guide_area() + 
  plot_layout(ncol = 2, guides = "collect")
dev.off()

tiff("04_figures/63_15_Residuals_sitenospot_random_Pinsylv.tiff", units = "mm", width = 370, height = 500,
     res = 400, compression = "lzw")
plot_ps_height + plot_ps_d13c + 
  plot_ps_bai80 + plot_ps_d18o +
  plot_ps_sla + plot_ps_chl + 
  plot_ps_n + plot_ps_xc +  
  # guide_area() + 
  plot_layout(ncol = 2, guides = "collect")
dev.off()