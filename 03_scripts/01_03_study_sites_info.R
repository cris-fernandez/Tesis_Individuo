rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate", "xlsx") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading plot info ####

plots <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/02_clean_data/02_01_clean_plot.csv", 
                  header = T, sep = ",")
plots <- plots %>% dplyr::select(c(site, plot_id, spot_status, geo_GPScm_UTM_elev)) %>% 
  rename(elevation = geo_GPScm_UTM_elev) %>% 
  mutate(pair_id = ifelse(site %in% c("Adobes", "Traid", "Alustante"), "Gua-Ps",
                   ifelse(site %in% c("Corbalan", "Cedrillas"), "Ter-Ps",
                   ifelse(site == "Guadarrama", "Mad-Ps",
                   ifelse(site %in% c("Roncal", "Urzainki"), "Nav-Ps",
                   ifelse(site %in% c("Basari", "Sarries"), "Nav-Aa",
                   ifelse(site %in% c("Fago", "Oza"), "Hue-Aa", "Mad-Pp")))))))

# 2.- Reading climate info ####

climate_data <- read.csv("02_clean_data/02_00_climate_full_data.csv") # Data 1950-2022 :)
spei_data <- read.csv("02_clean_data/02_00_spei_series.csv") %>% # Data 1950 - 2023 :)
  dplyr::select(-c(X, date, month)) %>% unique()

# 3.- Summarising climate info  ####
## 3.1.- Climate data ####

site_plot <- plots %>% dplyr::select(plot_id, pair_id)

climate_data <- left_join(climate_data, site_plot, by = "plot_id")
climate_means <- climate_data %>% 
  dplyr::select(c(pair_id, year, MAP, T_max, T_min)) %>% 
  mutate(T_mean = (T_max + T_min) / 2) %>% 
  group_by(pair_id) %>% 
  summarise(MAP_mean = mean(MAP, na.rm = T),
            MAP_sd = sd(MAP, na.rm = T),
            MAP_max = max(MAP, na.rm = T),
            MAP_min = min(MAP, na.rm = T),
            T_max_mean = mean(T_max, na.rm = T),
            T_max_sd = sd(T_max, na.rm = T),
            T_max_max = max(T_max, na.rm = T),
            T_max_min = min(T_max, na.rm = T),
            T_min_mean  = mean(T_min, na.rm = T),
            T_min_sd = sd(T_min, na.rm = T),
            T_min_max = max(T_min, na.rm = T),
            T_min_min = min(T_min, na.rm = T),
            T_mean_mean = mean(T_mean, na.rm = T),
            T_mean_sd = sd(T_mean, na.rm = T),
            T_mean_max = max(T_mean, na.rm = T),
            T_mean_min = min(T_mean, na.rm = T)
            )

## 3.2.- SPEI data ####

spei_data <- left_join(spei_data, site_plot, by = "plot_id")
spei_means <- spei_data %>% 
  dplyr::select(c(pair_id, year, spei12, spei18, spei24)) %>% 
  group_by(pair_id) %>% 
  summarise(spei12_mean = mean(spei12, na.rm = TRUE),
            spei12_sd = sd(spei12, na.rm = TRUE),
            spei12_max = max(spei12, na.rm = TRUE),
            spei12_min = min(spei12, na.rm = TRUE),
            spei18_mean = mean(spei18, na.rm = TRUE),
            spei24_mean  = mean(spei24, na.rm = TRUE))

# 4.- Summarising elevation data ####

elev_means <- plots %>% 
  group_by(pair_id) %>% 
  summarise(elev_mean = mean(elevation, na.rm = T),
            elev_sd = sd(elevation, na.rm = T),
            elev_max = max(elevation, na.rm = T),
            elev_min = min(elevation, na.rm = T))

# 5.- Joining the three tables ####

site_means <- full_join(elev_means, climate_means, by = "pair_id")
site_means <- full_join(site_means, spei_means, by = "pair_id")

# 6.- Saving ####

write.csv(site_means, "02_clean_data/02_03_site_means.csv")
write.xlsx(site_means, "02_clean_data/02_03_site_means.xlsx")
