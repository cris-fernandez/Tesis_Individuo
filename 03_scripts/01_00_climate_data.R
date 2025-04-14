rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading db ####

plots <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/02_clean_data/02_01_clean_plot.csv", 
                   header = T, sep = ",")

# Since I am just interested on the coordinates, I will select the needed columns

plots <- plots %>% select(c(plot_id, spot_status, geo_GPScm_latitude, geo_GPScm_longitude))

# 2.- Coodinates matrix ####

# Easyclimate accepts the usage of coordinates matrixes in order to download 
# climatic data from several points at the same time

coords <- matrix(c(plots$geo_GPScm_longitude, plots$geo_GPScm_latitude), ncol = 2, byrow = F)

# 3.- Precipitation ####

## 3.1.- Data downloading ####

precipitation <- get_daily_climate(coords = coords,
                                   period = 1950:2022,
                                   climatic_var = "Prcp") 

## 3.2.- Mean annual precipitation ####

precipitation$date <- as.Date(precipitation$date, format =  "%Y-%m-%d")
precipitation$year <- year(precipitation$date)

mean_precipitation <- precipitation %>% 
  group_by(year, lat, lon) %>% 
  summarise(MAP = sum(Prcp))

## 3.3.- Joining Prcp and plot data ####

# First, I make sure the column names in the coordinates df match the names in 
# Prcp data

coord_plots <- plots %>% 
  select(-spot_status) %>% 
  rename(lat = geo_GPScm_latitude,
         lon = geo_GPScm_longitude)

mean_precipitation <- full_join(coord_plots, mean_precipitation, by = c("lat", "lon"))

## 3.4.- Raw precipitation exportation ####

# write.csv(mean_precipitation, "00_Precipitation_raw.csv")

## 3.5.- Mean annual precipitation per plot ####

mean_ppt <- mean_precipitation %>% 
  mutate(site = substr(plot_id, start = 1, stop = 3)) %>% 
  select(c(site, year, MAP)) %>% 
  group_by(site) %>% 
  summarise(avg_MAP = mean(MAP))


## 3.6.- Average MAP per plot ####

# Since most of our data are short-term measurements, we are unable to create a 
# time series. Thus, we will condensate precipitation data into an average value
# per plot

average_precipitation <- precipitation_coords %>% 
  group_by(plot_id) %>% 
  summarise(avg_map = mean(MAP))

# 4.- Tmax ####

## 4.1.- Data downloading ####

maxtemp <- get_daily_climate(coords = coords,
                             period = 1950:2022,
                             climatic_var = "Tmax")  

## 4.2.- Mean Tmax ####

maxtemp$date <- as.Date(maxtemp$date, format =  "%Y-%m-%d")
maxtemp$year <- year(maxtemp$date)

mean_tmax <- maxtemp %>% 
  group_by(year, lat, lon) %>% 
  summarise(T_max = mean(Tmax))

## 4.3.- Joining Tmax and plot coordinates ####

mean_tmax <- full_join(coord_plots, mean_tmax, by = c("lat", "lon"))

# 5.- Tmin ####

## 5.1.- Data downloading ####

mintemp <- get_daily_climate(coords = coords,
                             period = 1950:2022,
                             climatic_var = "Tmin")  

## 5.2.- Mean Tmin ####

mintemp$date <- as.Date(mintemp$date, format =  "%Y-%m-%d")
mintemp$year <- year(mintemp$date)

mean_tmin <- mintemp %>% 
  group_by(year, lat, lon) %>% 
  summarise(T_min = mean(Tmin))

## 5.3.- Joining Tmin and plot coordinates ####

mean_tmin <- full_join(coord_plots, mean_tmin, by = c("lat", "lon"))

# 6.- Joining Prcp and Temp data ####

climate_data <- full_join(mean_precipitation, mean_tmax, by = c("lat", "lon", "year", "plot_id"))
climate_data <- full_join(climate_data, mean_tmin, by = c("lat", "lon", "year", "plot_id"))

# 7.- Means per site ####

climate_data_site <- climate_data %>% 
  mutate(site = substr(plot_id, start = 1, stop = 3)) %>% 
  group_by(site) %>% 
  summarise(Tmin = mean(T_min),
            Tmax = mean(T_max),
            Prcp = mean(MAP))

# 8.- Time series per site ####

climate_data_series <- climate_data %>% 
  mutate(site = substr(plot_id, start = 1, stop = 3)) %>% 
  group_by(site, year) %>% 
  summarise(Tmin = mean(T_min),
            Tmax = mean(T_max),
            Prcp = mean(MAP))

# 9.- Exportation ####

write.csv(climate_data_site, "02_clean_data/02_00_climate_means.csv")
write.csv(climate_data_series, "02_clean_data/02_00_climate_series.csv")
