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

# 5.- Sites coordinates ####

sites <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_01_result_plot.csv", 
                         header = T, sep = ",") %>% 
  dplyr::select(c(plot_id, site, geo_lat, geo_lon, sp_id)) %>% 
  filter(grepl("01", plot_id)) %>% 
  mutate(sp_id = ifelse(sp_id == "Pinpine", "Pinuspinea",
                        ifelse(sp_id == "Abialba", "Abiesalba", 
                               "Pinussylvestris")))


# 6.- Plotting ####

distrib_map <- ggplot() +
  geom_sf(data = neighbours, fill = "gray96", col = "gray30") +  # Siluetas
  geom_sf(data = provinces_sf, aes(fill = grupo), col = NA, linewidth = 0.6) +  
  geom_sf(data = abialba_crop, aes(fill = "Abies alba"), col = NA, alpha = 0.65) +
  geom_sf(data = pinsylv_crop, aes(fill = "Pinus sylvestris"), col = NA, alpha = 0.65) +
  geom_sf(data = pinpine_crop, aes(fill = "Pinus pinea"), col = NA, alpha = 0.65) +
  geom_sf(data = provinces_sf, fill = NA, color = "gray30", linewidth = 0.5) +  
  geom_point(data = sites, aes(x = geo_lon, y = geo_lat, fill = sp_id),
             col = "black", shape = 22, size = 5, stroke = 1.75) + 
  scale_fill_manual(name = "",
                    values = c("Abies alba" = "#746fb2",
                               "Pinus sylvestris" = "#1b9e77",
                               "Pinus pinea" = "#db5f02",
                               "Abiesalba" = "#746fb2",
                               "Pinussylvestris" = "#1b9e77",
                               "Pinuspinea" = "#db5f02",
                               "in" = "gray70",
                               "out" = "gray88"),
                    breaks = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea")) +
  theme_minimal() +
  theme_minimal() +
  labs(title = "") + 
  xlab("") + 
  ylab("") + 
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-12, 06), ylim = c(35, 45.5), expand = FALSE) +
  theme(panel.grid = element_line(color = "gray90"),
        legend.text = element_text(face = 'italic', size = 22))

# 7.- Exporting ####

tiff("04_figures/05_01_distrib_map.tiff", units = "mm", width = 250, height = 250,
     res = 800, compression = "lzw")
distrib_map
dev.off()


