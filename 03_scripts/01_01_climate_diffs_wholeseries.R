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
# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3)) # PC office

clean_target$pair_id <- ifelse(grepl("NAV|PEL", clean_target$plot_id) == T, "Mad-Pinpine",
                               ifelse(grepl("GUA", clean_target$plot_id) == T, "Mad-Pinsylv",
                                      ifelse(grepl("ADO|TRA|ALU", clean_target$plot_id) == T, "Gua-Pinsylv",
                                             ifelse(grepl("COR|CED", clean_target$plot_id) == T, "Ter-Pinsylv",
                                                    ifelse(grepl("RON|URZ", clean_target$plot_id) == T, "Nav-Pinsylv",
                                                           ifelse(grepl("BAS|SAR", clean_target$plot_id) == T, "Nav-Abialba",
                                                                  ifelse(grepl("FAG|OZA", clean_target$plot_id) == T, "Hue-Abialba",
                                                                         "z")))))))


# 2.- Reading climate data ####
# Climate data is read from a workspace saving the data downloaded in easyclimate.
# This is therefore the only non-reproducible script, but the size of the data 
# and the time needed to redownload data with easyclimate everytime does not realisitcally 
# allow us to rerun the script everytime needed.

load("C:/Users/recup/Desktop/TESIS/CAPITULO_2_INDIVIDUO/Proyecto_completo/Tesis_Individuo/01_raw_data/workspace_easyclimate.RData")

# 3.- Joining climate and plots ####

maxtemp_coord <- full_join(maxtemp, coord_plots, by = (c("lon", "lat")))
mintemp_coord <- full_join(mintemp, coord_plots, by = (c("lon", "lat")))
precipitation_coord <- full_join(precipitation, coord_plots, by = (c("lon", "lat")))

# 4.- Adding spot_status and pair_id ####
# These are in clean_target:

clean_target <- clean_target %>% dplyr::select(c(plot_id, spot_status, pair_id)) %>% 
  unique()

maxtemp_coord <- maxtemp_coord %>% full_join(clean_target, by = "plot_id")
mintemp_coord <- mintemp_coord %>% full_join(clean_target, by = "plot_id")
precipitation_coord <- precipitation_coord %>% full_join(clean_target, by = "plot_id")

temp_coord <- maxtemp_coord
temp_coord$Tmin <- mintemp_coord$Tmin
temp_coord$Tmean <- (temp_coord$Tmax + temp_coord$Tmin) / 2


# 5.- Mean temperature per site ####

# After talking to Paloma, maybe it is convenient just to show the temperature and 
# precipitation values per site... (22/10/2026):

temp_mean <- temp_coord %>%
  group_by(pair_id) %>%
  summarise(mean_Tmean = mean(Tmean, na.rm = TRUE),
            sd_Tmean = sd(Tmean, na.rm = TRUE),
            min_Tmean = quantile(Tmean, probs = 0.025),
            max_Tmean = quantile(Tmean, probs = 0.975))

# 6.- MAP per site ####

precipitation_mean <- precipitation_coord %>% 
  group_by(plot_id, year) %>% 
  summarise(Prcp = sum(Prcp)) %>% 
  full_join(clean_target, by = "plot_id") %>% 
  summarise(mean_Prcp = mean(Prcp, na.rm = TRUE),
            sd_Prcp = sd(Prcp, na.rm = TRUE),
            min_Prcp = quantile(Prcp, probs = 0.025),
            max_Prcp = quantile(Prcp, probs = 0.975))
  
# Removing large datasets that will be no longer used:

rm(average_precipitation, climate_data, climate_data_series, climate_data_site,
   coord_plots, maxtemp, maxtemp_coord, mintemp, mintemp_coord, precipitation,
   precipitation_coord, precipitation_coords, precipitation_list)





# maxtemp_dailymean <- maxtemp_coord %>%
#   group_by(pair_id, spot_status, date) %>%
#   summarise(mean_Tmax = mean(Tmax, na.rm = TRUE))
# 
# mintemp_dailymean <- mintemp_coord %>%
#   group_by(pair_id, spot_status, date) %>%
#   summarise(mean_Tmin = mean(Tmin, na.rm = TRUE))
# 
# precipitation_dailymean <- precipitation_coord %>%
#   group_by(pair_id, spot_status, date) %>%
#   summarise(mean_Prcp = mean(Prcp, na.rm = TRUE))



# 
# avg_temp %>%
#   group_by(pair_id) %>%
#   summarise(p_value = wilcox.test(Coldspot, Hotspot, paired = TRUE)$p.value) %>% 
#   mutate(p_bonferroni = p.adjust(p_value, method = "bonferroni"))
# 
# 
# # Nav-Abialba
# navaa <- avg_temp %>% filter(pair_id == "Nav-Pinsylv")
# wilcox.test(navaa$Coldspot, navaa$Hotspot, paired = T)
# 
# # Hue-Abialba
# 
# avg_temp %>%
#   group_by(pair_id) %>%
#   summarise(
#     mean_diff = mean(Hotspot - Coldspot, na.rm = TRUE),
#     sd_diff = sd(Hotspot - Coldspot, na.rm = TRUE),
#     min_diff = min(Hotspot - Coldspot, na.rm = TRUE),
#     max_diff = max(Hotspot - Coldspot, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# ######
# 
# library(tidyverse)
# library(nlme)
# 
# gls_results <- avg_temp %>%
#   ungroup() %>%
#   group_by(pair_id) %>%
#   group_modify(~ {
#     
#     dat <- .x %>%
#       filter(is.finite(temp_diff)) %>%
#       arrange(year)
#     
#     model <- gls(
#       temp_diff ~ 1,
#       data = dat,
#       correlation = corAR1(form = ~ year),
#       method = "REML"
#     )
#     
#     coefs <- summary(model)$tTable
#     
#     tibble(
#       mean_diff = coefs[1, "Value"],
#       SE = coefs[1, "Std.Error"],
#       t_value = coefs[1, "t-value"],
#       p_value = coefs[1, "p-value"],
#       AR1 = coef(model$modelStruct$corStruct, unconstrained = FALSE)
#     )
#   }) %>%
#   ungroup() %>%
#   mutate(
#     p_holm = p.adjust(p_value, method = "holm"),
#     p_bonferroni = p.adjust(p_value, method = "bonferroni")
#   )
# 
# gls_results
# 


# .............

# <####################
# summary(model_full)
# 
# navaa <- navaa %>% 
#   dplyr::select(-temp_diff) %>% 
#   pivot_longer(cols = Coldspot:Hotspot,
#                names_to = "spot_status",
#                values_to = "Tmean")
# 
# lm(Tmean ~ year * spot_status, data = navaa)
# # ########################
# 
# library(nlme)
# 
# # Model without spot_status
# m0 <- gls(
#   Tmean ~ year,
#   data = navaa,
#   correlation = corAR1(form = ~ year | spot_status),
#   method = "ML"
# )
# 
# # Model including spot_status and its interaction with year
# m1 <- gls(
#   Tmean ~ year * spot_status,
#   data = navaa,
#   correlation = corAR1(form = ~ year | spot_status),
#   method = "ML"
# )
# 
# # Compare models
# anova(m0, m1)



