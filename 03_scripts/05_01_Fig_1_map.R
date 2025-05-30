rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "ggplot2",
        "FactoMineR", "factoextra", "png", "maps", "sf", "rnaturalearth",
        "rnaturalearthdata", "mapSpain") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

# install.packages("rnaturalearthdata")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Spanish provinces ####

provinces_sf <- esp_get_prov()
plot(st_geometry(provinces_sf))
st_crs(provinces_sf)

provinces_sf <- provinces_sf %>% st_set_crs(4326) %>% 
  filter(!nuts2.name == "Canarias") %>% # They appear on the map otherwise
  mutate(grupo = ifelse(ine.prov.name %in% c("Guadalajara", "Madrid", "Teruel", "Huesca", "Navarra"),
                        "in", "out"))

# 2.- Neighboring countries ####

world <- ne_countries(scale = "large", returnclass = "sf")

# Since the provinces and the country shapes do not have the same level of 
# detail, the international borders of Spain look a bit off, so we will not 
# include it into our neighbours plot

neighbours <- world %>%
  filter(admin %in% c("Portugal", "France", "Andorra",
                      "Morocco", "Algeria", "Italy", "Switzerland",
                      "Tunisia", "Libya", "Germany")) %>%
  st_transform(crs = 4326)

neighbours <- st_union(neighbours) %>% 
  st_sf()

# But, for cropping the distribution maps, we need to include Spain

countries <- world %>%
  filter(admin %in% c("Spain", "Portugal", "France", "Andorra",
                      "Morocco", "Algeria", "Italy", "Switzerland",
                      "Tunisia", "Libya", "Germany")) %>%
  st_transform(crs = 4326)

# 3.- Reading distribution maps ####

abialba <- read_sf(dsn = "01_raw_data/Abies alba/shapefiles/Abies_alba_plg.shp")
pinsylv <- read_sf(dsn = "01_raw_data/Pinus sylvestris/shapefiles/Pinus_sylvestris_plg.shp")
pinpine <- read_sf(dsn = "01_raw_data/Pinus pinea/shapefiles/Pinus_pinea_plg.shp")
st_crs(pinpine)

# 4.- Cropping distribution areas ####

pinpine_crop <- st_intersection(pinpine, st_union(countries))
pinsylv_crop <- st_intersection(pinsylv, st_union(countries))
abialba_crop <- st_intersection(abialba, st_union(countries))

# 5.- Plotting

ggplot() +
  geom_sf(data = neighbours, fill = "gray95", color = "gray30") +  # Siluetas
  geom_sf(data = provinces_sf, fill = NA, color = "gray30", linewidth = 0.4) +  
  geom_sf(data = abialba_crop, aes(fill = "Abies alba"), col = NA, alpha = 0.4) +
  geom_sf(data = pinsylv_crop, aes(fill = "Pinus sylvestris"), col = NA, alpha = 0.4) +
  geom_sf(data = pinpine_crop, aes(fill = "Pinus pinea"), col = NA, alpha = 0.4) +
  scale_fill_manual(name = "",
                    values = c("Abies alba" = "blue",
                               "Pinus sylvestris" = "green",
                               "Pinus pinea" = "orange")) +
  theme_minimal() +
  theme_minimal() +
  labs(title = "") + 
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-12, 06), ylim = c(35, 45.5), expand = FALSE) +
  theme(panel.grid = element_line(color = "gray90"))



