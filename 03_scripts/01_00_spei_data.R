rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "raster", "ncdf4") #list of packages
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

plots <- plots %>% dplyr::select(c(plot_id, spot_status, geo_GPScm_latitude, geo_GPScm_longitude))

# 2.- Coodinates matrix ####

# Obtaining the coordinates so we can select the grids corresponding to our points

coords <- matrix(c(plots$geo_GPScm_longitude, plots$geo_GPScm_latitude), ncol = 2, byrow = F)

# 3.- Reading spei raw data ####

## 3.1.- Extracting spei data ####

# SPEI data is downloaded from CSIC database. It comes in a ".nc" file, which 
# needs a specific function from the library 'ncdf4' to be read:

spei12 <- nc_open("01_raw_data/spei12.nc")

# Now, this file has data structured in layers. We are only interested in 
# the layer containing SPEI values for now...

spei12_array <- ncvar_get(spei12, "spei")

# Data written to a netCDF file is cached in memory, for better performance. 
# This data is only written out to disk when the file is closed. 
# Thus, we need to close it always once we are done with it:

nc_close(spei12)

dim(spei12_array) # The dimensions are lon x lat x time, being time an spei 
# value for a given point at a given ¿month?

## 3.2.- Extracting the coordinates of the grid from spei data ####

# First, we need to know where in the .nc file (in what index number) are my
# coordinates

spei12 <- nc_open("01_raw_data/spei12.nc")

lon <- ncvar_get(spei12, "lon")  # vector de longitudes
lat <- ncvar_get(spei12, "lat")  # vector de latitudes

nc_close(spei12)

# Checking the dimensions: 

length(lon)
length(lat)

# The dimensions match the dimensions of the spei12_array :)

## 3.3.- Extracting the grids where coords are ####

# Function to locate the nearest layer to my point 

nearest_index <- function(value, vector) {
  which.min(abs(vector - value))
}

coord_indices <- apply(coords, 1, function(pt) {
  c(lon_i = nearest_index(pt[1], lon),
    lat_i = nearest_index(pt[2], lat))
})

# Transpose so every row is a coord. point

coord_indices <- t(coord_indices)

## 3.4.- Extracting spei series for every point ####

spei12_series <- apply(coord_indices, 1, function(ix) {
  spei12_array[ix[1], ix[2], ]  # Extracts the time series
})

# In the resulting matrix, each column is a time series for every point (rows)
dim(spei12_series)

spei12_df <- as.data.frame(spei12_series)

## 3.5.- Naming the rows and columns ####

# Rows will be named after the date, so we need to extract the dates from the 
# nc file:

spei12 <- nc_open("01_raw_data/spei12.nc")
time12_raw <- ncvar_get(spei12, "time")
nc_close(spei12)

# Convert dates to date, and then to character for the rownames:

time12_dates <- as.Date(time12_raw, origin = "1900-01-01")

rownames(spei12_df) <- as.character(time12_dates)

# As for the colnames, they will be the same as the plot names in 'plots'

colnames(spei12_df) <- plots$plot_id

## 3.6.- Rearranging the data so it can be exported ####

spei12_df$date <- rownames(spei12_df)

spei12_df <- spei12_df %>% 
  pivot_longer(-date, names_to = "plot_id", values_to = "spei12") %>% 
  mutate(year = substring(date, 1, 4),
         year = as.numeric(year),
         month = substring(date, 6, 7),
         month = as.numeric(month)) %>% 
  filter(year > 1949)

# 4.- Reading spei raw data ####

## 4.1.- Extracting spei data ####

# SPEI data is downloaded from CSIC database. It comes in a ".nc" file, which 
# needs a specific function from the library 'ncdf4' to be read:

spei18 <- nc_open("01_raw_data/spei18.nc")

# Now, this file has data structured in layers. We are only interested in 
# the layer containing SPEI values for now...

spei18_array <- ncvar_get(spei18, "spei")

# Data written to a netCDF file is cached in memory, for better performance. 
# This data is only written out to disk when the file is closed. 
# Thus, we need to close it always once we are done with it:

nc_close(spei18)

dim(spei18_array) # The dimensions are lon x lat x time, being time an spei 
# value for a given point at a given ¿month?

## 4.2.- Extracting the coordinates of the grid from spei data ####

# First, we need to know where in the .nc file (in what index number) are my
# coordinates

spei18 <- nc_open("01_raw_data/spei18.nc")

lon <- ncvar_get(spei18, "lon")  # vector de longitudes
lat <- ncvar_get(spei18, "lat")  # vector de latitudes

nc_close(spei18)

# Checking the dimensions: 

length(lon)
length(lat)

# The dimensions match the dimensions of the spei18_array :)

## 4.3.- Extracting the grids where coords are ####

# Function to locate the nearest layer to my point 

nearest_index <- function(value, vector) {
  which.min(abs(vector - value))
}

coord_indices <- apply(coords, 1, function(pt) {
  c(lon_i = nearest_index(pt[1], lon),
    lat_i = nearest_index(pt[2], lat))
})

# Transpose so every row is a coord. point

coord_indices <- t(coord_indices)

## 4.4.- Extracting spei series for every point ####

spei18_series <- apply(coord_indices, 1, function(ix) {
  spei18_array[ix[1], ix[2], ]  # Extracts the time series
})

# In the resulting matrix, each column is a time series for every point (rows)
dim(spei18_series)

spei18_df <- as.data.frame(spei18_series)

## 4.5.- Naming the rows and columns ####

# Rows will be named after the date, so we need to extract the dates from the 
# nc file:

spei18 <- nc_open("01_raw_data/spei18.nc")
time18_raw <- ncvar_get(spei18, "time")
nc_close(spei18)

# Convert dates to date, and then to character for the rownames:

time18_dates <- as.Date(time18_raw, origin = "1900-01-01")

rownames(spei18_df) <- as.character(time18_dates)

# As for the colnames, they will be the same as the plot names in 'plots'

colnames(spei18_df) <- plots$plot_id

## 4.6.- Rearranging the data so it can be exported ####

spei18_df$date <- rownames(spei18_df)

spei18_df <- spei18_df %>% 
  pivot_longer(-date, names_to = "plot_id", values_to = "spei18") %>% 
  mutate(year = substring(date, 1, 4),
         year = as.numeric(year),
         month = substring(date, 6, 7),
         month = as.numeric(month)) %>% 
  filter(year > 1949)

# 5.- Reading spei raw data ####

## 5.1.- Extracting spei data ####

# SPEI data is downloaded from CSIC database. It comes in a ".nc" file, which 
# needs a specific function from the library 'ncdf4' to be read:

spei24 <- nc_open("01_raw_data/spei24.nc")

# Now, this file has data structured in layers. We are only interested in 
# the layer containing SPEI values for now...

spei24_array <- ncvar_get(spei24, "spei")

# Data written to a netCDF file is cached in memory, for better performance. 
# This data is only written out to disk when the file is closed. 
# Thus, we need to close it always once we are done with it:

nc_close(spei24)

dim(spei24_array) # The dimensions are lon x lat x time, being time an spei 
# value for a given point at a given ¿month?

## 5.2.- Extracting the coordinates of the grid from spei data ####

# First, we need to know where in the .nc file (in what index number) are my
# coordinates

spei24 <- nc_open("01_raw_data/spei24.nc")

lon <- ncvar_get(spei24, "lon")  # vector de longitudes
lat <- ncvar_get(spei24, "lat")  # vector de latitudes

nc_close(spei24)

# Checking the dimensions: 

length(lon)
length(lat)

# The dimensions match the dimensions of the spei24_array :)

## 5.3.- Extracting the grids where coords are ####

# Function to locate the nearest layer to my point 

nearest_index <- function(value, vector) {
  which.min(abs(vector - value))
}

coord_indices <- apply(coords, 1, function(pt) {
  c(lon_i = nearest_index(pt[1], lon),
    lat_i = nearest_index(pt[2], lat))
})

# Transpose so every row is a coord. point

coord_indices <- t(coord_indices)

## 5.4.- Extracting spei series for every point ####

spei24_series <- apply(coord_indices, 1, function(ix) {
  spei24_array[ix[1], ix[2], ]  # Extracts the time series
})

# In the resulting matrix, each column is a time series for every point (rows)
dim(spei24_series)

spei24_df <- as.data.frame(spei24_series)

## 5.5.- Naming the rows and columns ####

# Rows will be named after the date, so we need to extract the dates from the 
# nc file:

spei24 <- nc_open("01_raw_data/spei24.nc")
time24_raw <- ncvar_get(spei24, "time")
nc_close(spei24)

# Convert dates to date, and then to character for the rownames:

time24_dates <- as.Date(time24_raw, origin = "1900-01-01")

rownames(spei24_df) <- as.character(time24_dates)

# As for the colnames, they will be the same as the plot names in 'plots'

colnames(spei24_df) <- plots$plot_id

## 5.6.- Rearranging the data so it can be exported ####

spei24_df$date <- rownames(spei24_df)

spei24_df <- spei24_df %>% 
  pivot_longer(-date, names_to = "plot_id", values_to = "spei24") %>% 
  mutate(year = substring(date, 1, 4),
         year = as.numeric(year),
         month = substring(date, 6, 7),
         month = as.numeric(month)) %>% 
  filter(year > 1949)

# 6.- Merging ####

spei_data <- full_join(spei12_df, spei18_df, by = c("date", "plot_id", "year", "month"))
spei_data <- full_join(spei_data, spei24_df, by = c("date", "plot_id", "year", "month"))

# 7.- Exporting ####

write.csv(spei_data, "02_clean_data/02_00_dendro_series.csv")