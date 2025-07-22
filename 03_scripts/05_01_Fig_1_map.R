rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "ggplot2",
        "FactoMineR", "factoextra", "png", "maps", "sf", "rnaturalearth",
        "rnaturalearthdata", "mapSpain", "ggspatial", "cowplot") #list of packages
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
  filter(!nuts2.name == "Canarias")

provinces_ibf <- provinces_sf %>% 
  filter(ine.prov.name %in% c("Guadalajara", "Madrid", "Teruel", "Huesca", "Navarra"))

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


# 6.- Plotting map A ####

distrib_map <- ggplot() +
  geom_sf(data = neighbours, fill = "gray96", col = NA) + 
  geom_sf(data = provinces_sf, fill = "#ffffff", col = "#d5d5d5", linewidth = 0.6) +  
  geom_sf(data = provinces_ibf, fill = "#d3d3d3", col = "black", linewidth = 0.6) +  
  geom_sf(data = abialba_crop, aes(fill = "Abies alba"), col = NA, alpha = 0.65) +
  geom_sf(data = pinsylv_crop, aes(fill = "Pinus sylvestris"), col = NA, alpha = 0.65) +
  geom_sf(data = pinpine_crop, aes(fill = "Pinus pinea"), col = NA, alpha = 0.65) +
  geom_sf(data = countries, fill = NA, col = "black", linewidth = 0.6) +
  scale_fill_manual(name = "",
                    values = c("Abies alba" = "#785EF0",
                               "Pinus sylvestris" = "#FFB000",
                               "Pinus pinea" = "#990000"),
                    breaks = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea")) +
  theme_minimal() +
  labs(title = "") + 
  xlab("") + 
  ylab("") + 
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-12, 05), ylim = c(35, 45.5), expand = FALSE) +
  theme(panel.grid = element_line(color = "gray90"),
        legend.text = element_text(face = 'italic', size = 22),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(colour = "black", 
                                    fill = NA, 
                                    linewidth = 1),
        panel.background = element_rect(fill = "#ffffff")) + 
  ggspatial::annotation_scale(location = "br",
                              bar_cols = c("black", "white"),
                              text_family = "sans")

# 7.- Plotting map B ####

focus_map <- ggplot() +
  geom_sf(data = neighbours, fill = "gray96", col = NA) +
  geom_sf(data = provinces_sf, fill = "#ffffff", col = "#d5d5d5", linewidth = 0.6) +  
  geom_sf(data = provinces_ibf, fill = "#d3d3d3", col = "black", linewidth = 0.6) +  
  geom_sf(data = abialba_crop, aes(fill = "Abies alba"), col = NA, alpha = 0.65) +
  geom_sf(data = pinsylv_crop, aes(fill = "Pinus sylvestris"), col = NA, alpha = 0.65) +
  geom_sf(data = pinpine_crop, aes(fill = "Pinus pinea"), col = NA, alpha = 0.65) +
  geom_sf(data = countries, fill = NA, col = "black", linewidth = 0.6) +
  geom_sf_text(data = provinces_ibf, aes(label = ine.prov.name), size = 4, 
               family = "sans", col = "#565656") +
  geom_point(data = sites, aes(x = geo_lon, y = geo_lat, fill = sp_id),
             col = "black", shape = 22, size = 5, stroke = 1.35) +
  scale_fill_manual(name = "",
                    values = c("Abies alba" = "#785EF0",
                               "Pinus sylvestris" = "#FFB000",
                               "Pinus pinea" = "#990000",
                               "Abiesalba" = "#785EF0",
                               "Pinussylvestris" = "#FFB000",
                               "Pinuspinea" = "#990000"),
                    breaks = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea")) +
  theme_minimal() +
  labs(title = "") + 
  xlab("") + 
  ylab("") + 
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-5.5, 1), ylim = c(39.5, 43.5), expand = FALSE) +
  theme(panel.grid = element_line(color = "gray90"),
        legend.text = element_text(face = 'italic', size = 22),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) + 
  ggspatial::annotation_scale(location = "br",
                              bar_cols = c("black", "white"),
                              text_family = "sans") + 
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.9, "mm"), pad_y = unit(0.9, "mm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "#565656",
      text_family = "sans"))

# 8.- Assembling both maps ####

final_map <- ggdraw() +
  draw_plot(focus_map) + 
  draw_plot(distrib_map, x = 0.09, y = 0.56, 
            width = 0.3, height = 0.3) + 
  draw_plot_label(label = "B", x = 0.09, y = 0.92, size = 30)

# 9.- General distribution map ####

# First I need to generate a shapefile with all countries:

mundo <- world %>% 
  st_transform(crs = 4326)

# Checking if geometries are all valid:

invalid_pinpine <- st_is_valid(pinpine)
invalid_pinsylv <- st_is_valid(pinsylv)
invalid_abialba <- st_is_valid(abialba)
invalid_mundo <- st_is_valid(mundo) # Observation no. 168 is not valid: Egypt

mundo <- world %>% 
  filter(!admin == "Egypt")

# Cropping distribution areas

pinpine_world <- st_intersection(pinpine, st_union(mundo))
pinsylv_world <- st_intersection(pinsylv, st_union(mundo))
abialba_world <- st_intersection(abialba, st_union(mundo))

# 10.- Plotting map A ####

distrib_world <- ggplot() +
  geom_sf(data = mundo, fill = "#d3d3d3", alpha = 0.6, col = "black", linewidth = 0.6) +  
  geom_sf(data = abialba_world, aes(fill = "Abies alba"), col = NA, alpha = 0.65) +
  geom_sf(data = pinsylv_world, aes(fill = "Pinus sylvestris"), col = NA, alpha = 0.65) +
  geom_sf(data = pinpine_world, aes(fill = "Pinus pinea"), col = NA, alpha = 0.65) +
  geom_sf(data = countries, fill = NA, col = "black", linewidth = 0.6) +
  scale_fill_manual(name = "",
                    values = c("Abies alba" = "#785EF0",
                               "Pinus sylvestris" = "#FFB000",
                               "Pinus pinea" = "#990000"),
                    breaks = c("Abies alba",
                               "Pinus sylvestris",
                               "Pinus pinea")) +
  theme_minimal() +
  labs(title = "") + 
  xlab("") + 
  ylab("") + 
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(-15, 65), ylim = c(35, 75), expand = FALSE) +
  theme(panel.grid = element_line(color = "gray90"),
        legend.text = element_text(face = 'italic', size = 22),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        legend.position = "none",
        panel.border = element_rect(colour = "black", 
                                    fill = NA, 
                                    linewidth = 1),
        panel.background = element_rect(fill = "#ffffff")) + 
  ggspatial::annotation_scale(location = "br",
                              bar_cols = c("black", "white"),
                              text_family = "sans") + 
  draw_plot_label(label = "A", x = 0.09, y = 0.8, size = 20) + 
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.1, "mm"), pad_y = unit(0.4, "mm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "white"),
      line_col = "#565656",
      text_family = "sans"))

distrib_world <- ggdraw() + 
  draw_plot(distrib_world) + 
  draw_plot_label(label = "A", x = 0.09, y = 0.92, size = 30)

# 9.- Exporting ####

tiff("04_figures/05_01_fig1_panels.tiff", units = "mm", width = 250, height = 450,
     res = 600, compression = "lzw")
distrib_world/plot_spacer()/final_map + plot_layout(heights = c(5, 0, 5))
dev.off()
