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

#### 1.- Raw database cleaning ####

raw_shrubreg <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/01_raw_data/01_04_raw_shrubreg.csv") %>% dplyr::select(-X)

# Substitution of "Other" ("Otra") observations in "sp" column by its specified 
# species name, in "sp_other" column:

raw_shrubreg <- raw_shrubreg %>% 
  mutate(sp = ifelse(sp == 'Otra', 
                     sp_other, sp))
raw_shrubreg <- raw_shrubreg %>% 
  mutate(sp = ifelse(sp == 'Otras', 
                     sp_other, sp))

# Cleaning plot_code IDs, as there may be spaces:

sort(unique(raw_shrubreg$plot_id))
raw_shrubreg$plot_id <- gsub(" ", "", raw_shrubreg$plot_id)
unique(raw_shrubreg$plot_id) # Solved

# Replacing empty observations by NAs in h_shrubreg: 

raw_shrubreg$h_shrubreg[raw_shrubreg$h_shrubreg == ""] <- NA
which(is.na(raw_shrubreg$h_shrubreg)) # Just for checking. 
# We are missing the height of a Rosmarinus officinalis observation in NAV10

# Correction of a height data:

raw_shrubreg$h_shrubreg[raw_shrubreg$h_shrubreg == 4000] <- 400

#### 2.- Tidying extra observations in 'notes' ####

# First, I create a unique ID, and then I subset all observations 
# with a patchy distribution. To do so, I just need to select all rows containing
# a 'x' in 'notes'.

raw_shrubreg$ID <- 1:nrow(raw_shrubreg)

raw_subset <- raw_shrubreg[grep("x", raw_shrubreg$notes), ]

# However, columns with replicas and with notes about J. oxicedrus are not needed
raw_subset <- raw_subset[-grep("copiado|oxicedrus", raw_subset$notes), ]

# Now, I eliminate all rows from raw_shrubreg already present in raw_subset:
raw_shrubreg <- anti_join(raw_shrubreg, raw_subset, by = "ID")

expect_equal(nrow(raw_shrubreg) + nrow(raw_subset), 9864) # Total amount of observations

raw_subset2 <- separate(raw_subset, notes, 
                        c("h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8", "h9", 
                          "h10", "h11", "h12", "h13", "h14")) %>%  
  dplyr::select(-c(h_shrubreg, cover))

raw_subset2$h2 <- replace(raw_subset2$h2, raw_subset2$h2 == "Muerto", NA)
raw_subset2$h3 <- replace(raw_subset2$h3, raw_subset2$h3 == "per", NA)
raw_subset2$h4 <- replace(raw_subset2$h4, raw_subset2$h4 == "rebeotando", NA)

raw_subset2 <- raw_subset2 %>% pivot_longer(cols = 8:21, values_to = "h_cov",
                                            values_drop_na = T)

# Although using columns and row positions is rarely recommended, this time
# it is used because data has very little variation, difficulting its 
# localization through other means:

raw_subset2[185,15] <- "20x15"
raw_subset2[186,15] <- "" 

raw_subset2 <- raw_subset2[!(raw_subset2$h_cov == ""), ]
# 10 "missing" heights. But they are not really "lost" data, R just automatically 
# added empty slots if the original observations ended in "/"

raw_subset2 <- raw_subset2[!(raw_subset2$sp == "Digitalis purpurea"), ] # Not a
# shrub species, it's herbaceous.

raw_subset2 <- raw_subset2 %>% 
  separate(h_cov, c('h_shrubreg', 'cover'), sep="x") %>% dplyr::select(-name)
raw_subset2$notes <- ""
raw_subset2$h_shrubreg <- as.numeric(raw_subset2$h_shrubreg)
raw_subset2$cover <- as.numeric(raw_subset2$cover)

raw_shrubreg <- bind_rows(raw_shrubreg, raw_subset2) %>% dplyr::select(-ID) # I don´t 
# need that ID column

#### 3.- Unique ID generation ####

# Creation of a unique ID for each observation, based on the plot code and the
# observation number of each shrub

raw_shrubreg <- raw_shrubreg %>% group_by(plot_id) %>% 
  mutate(obs_id = row_number())
raw_shrubreg$obs_id <- sprintf("%03d",raw_shrubreg$obs_id)

raw_shrubreg$shrubreg_id <- paste(raw_shrubreg$plot_id, raw_shrubreg$obs_id, 
                                  sep = "-") # Sep changed from "_" to "-"

#### 4.- Species name depuration ####

unique(raw_shrubreg$sp) %>% sort()
raw_shrubreg <- raw_shrubreg[!(raw_shrubreg$sp == "Digitalis purpurea"), ]
# Digitalis is not shrub

raw_shrubreg$sp <- gsub("Acer pseudoplatanus ", 
                        "Acer pseudoplatanus", raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Arctostaphylos uva-ursi |Uva-ursi", 
                        "Arctostaphylos uva-ursi", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Berberis sp.", 
                        "Berberis sp", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("^Buxus$|Buxuss sempervirens", 
                        "Buxus sempervirens", 
                        raw_shrubreg$sp, ignore.case = TRUE, useBytes = T)
raw_shrubreg$sp <- gsub("Córnea sanguinea|^Cornus$", 
                        "Cornus sanguinea", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Cistus laurifolious|Cistus  laurifolius", 
                        "Cistus laurifolius", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Citisus sp|Cytisus sp|Cytisus  scoparius|Cytisus scooarius", 
                        "Cytisus scoparius", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Clemaris vitalba|^Clematis$", 
                        "Clematis vitalba", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("^Corilus$|Corilus avellana|Corilus avellano|Corilus avvellana|
                        Corylis avellana|Corilys avellana", 
                        "Corylus avellana", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Coronillus emerus", 
                        "Coronilla emerus", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("^Crataegus sp$|Crataegus sp.", 
                        "Crataegus monogyna", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Euonimus europeus", 
                        "Euonymus europaeus", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Genista sp", 
                        "Genista scorpius", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("^Hedera$|Hederas", 
                        "Hedera helix", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Helicrysum stoechas", 
                        "Helichrysum stoechas", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Lavándola|Lavándola sp|^Lavandula$|Lavandula sp", 
                        "Lavandula stoechas", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Lavandula latifolia|Espliego", 
                        "Lavandula stoechas", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Lies aquifolium|Oles aquifolium", 
                        "Ilex aquifolium", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Loco era periclinemun|Lonicera periclynemun
                        |^Lonicera spp.$|Lonicera spp.|Lonicera periclynemun|
                        Lonicera periclymenum ", 
                        "Lonicera periclymenum", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg <- raw_shrubreg %>% 
  mutate(sp = ifelse(sp == 'L', 
                     "Lonicera periclymenum", sp))
raw_shrubreg$sp <- gsub("Lonicera cf xylosteum|Lonicera A", 
                        "Lonicera xylosteum", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Ncina", 
                        "Quercus ilex", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Quercus sp.", 
                        "Quercus faginea", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Quer?is subpyrenaica", 
                        "Quercus subpyrenaica", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Robus|^Ru$|Rubios ulmifolius|^Rubus$|^Rubus sp$|
                      ^Rubus sp.$|Rubus ulmifolius.|Rubus spp.", 
                        "Rubus ulmifolius", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Rosa sp.|^Rosa rubus\\??$", 
                        "Rosa sp", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Salvia lavadulifolia", 
                        "Salvia lavandulifolia", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Satureja sp", 
                        "Satureja intricata", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Sorbos", 
                        "Sorbus", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Tilia platiphyllos", 
                        "Tilia platyphyllos", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Thymus sp", 
                        "Thymus vulgaris", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Rosa sp.", 
                        "Rosa sp", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Rubus sp.", 
                        "Rubus ulmifolius", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Lonicera periclymenum ", 
                        "Lonicera periclymenum", 
                        raw_shrubreg$sp, useBytes = T)
raw_shrubreg$sp <- gsub("Quer\xe9is subpyrenaica|Queréis subpyrenaica", 
                        "Quercus subpyrenaica", 
                        raw_shrubreg$sp, useBytes = T)

raw_shrubreg <- raw_shrubreg[!(raw_shrubreg$sp == ""), ] # Missing sp in obs. from Navas
unique(raw_shrubreg$sp) %>% sort()

#### 5.- Removal of unnecessary columns ####

# Collapsing cover and patch cover into the same column, for the sake of 
# simplicity.

raw_shrubreg <- raw_shrubreg %>% mutate(cover = ifelse(is.na(cover) == T, 
                                                       ifelse(is.na(patch_cover) == F, patch_cover, cover), 
                                                       cover))

clean_shrubreg <- raw_shrubreg %>% dplyr::select(c(plot_id, transect, 
                                                   sp, h_shrubreg, cover,
                                                   site, prov, 
                                                   spot_status = damage, 
                                                   shrubreg_id))

#### 6.- Other IDs generation ####
# Filling incomplete columns: 

clean_shrubreg <- clean_shrubreg %>% 
  mutate(spot_status = ifelse(spot_status == "",
                              ifelse(str_detect(plot_id, c("SAR|OZA|URZ")), "coldspot", "hotspot"), 
                              spot_status))

clean_shrubreg <- clean_shrubreg %>% 
  mutate(spot_status = ifelse(is.na(spot_status) == T,
                              ifelse(str_detect(plot_id, c("SAR|OZA|URZ")), "coldspot", "hotspot"), 
                              spot_status))

clean_shrubreg <- clean_shrubreg %>% 
  mutate(region = ifelse(str_detect(plot_id, c("BAS|SAR|URZ|RON")), "Navarra", 
                         ifelse(str_detect(plot_id, c("OZA|FAG")), "Huesca",
                                ifelse(str_detect(plot_id, c("GUA|PEL|NAV")), "Madrid",
                                       ifelse(str_detect(plot_id, c("COR|CED")), "Teruel",
                                              ifelse(str_detect(plot_id, c("TRA|ALU|ADO")), "Guadalajara", prov))))))

clean_shrubreg$spot_status <- gsub("Hotspot", "hotspot", clean_shrubreg$spot_status)
clean_shrubreg$spot_status <- gsub("Coldspot", "coldspot", clean_shrubreg$spot_status)

# Adding new IDs, according to the unified codes proposed by Paloma:

clean_shrubreg <- clean_shrubreg %>% 
  mutate(main_sp = ifelse(str_detect(plot_id, c("BAS|SAR|FAG|OZA")), "Abialb", 
                          ifelse(str_detect(plot_id, c("GUA|COR|CED|RON|URZ|TRA|ADO|ALU")), "Pinsylv",
                                 "Pinpine")))

#### 7.- Adding missing variables: ####

#### 7.1.- sp_id and sp_type #####

sp <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/02_clean_data/02_00_clean_sp_key.csv", header = T, sep = ",") %>% 
  dplyr::select(-X) %>% rename(sp = sp_name)

clean_shrubreg <- full_join(clean_shrubreg, sp, by = "sp")

#### 7.2.- spot_status #####

clean_shrubreg <- clean_shrubreg %>% 
  mutate(spot_status = ifelse(str_detect(plot_id, c("SAR|OZA|URZ|CED|ALU|GUA01|GUA10|GUA11|GUA12|GUA13|GUA14|GUA15|NAV03|NAV05|NAV07|NAV08|NAV09")), 
                              "coldspot", "hotspot"))

#### 8.- Variable renaming and rearranging ####

clean_shrubreg <- clean_shrubreg %>% rename(sp_name = sp,
                                            shrubreg_cover = cover,
                                            shrubreg_height = h_shrubreg)

clean_shrubreg <- clean_shrubreg[, c("shrubreg_id", "plot_id", "sp_id",
                                     "spot_status", "region", "site", "main_sp",
                                     "transect", "sp_type", "sp_name", 
                                     "shrubreg_height", "shrubreg_cover")]

#### 9.- Variable type checking ####

lapply(clean_shrubreg, class)

clean_shrubreg$sp_type <- gsub("shrub", "sh", clean_shrubreg$sp_type)
clean_shrubreg$sp_type <- gsub("tree", "reg", clean_shrubreg$sp_type)


#### 10.- Tests ####

expect_equal(length(which(is.na(clean_shrubreg$sp_name))), 0) #No empty sp obs.
expect_equal(length(which(is.na(clean_shrubreg$transect))), 0) # No empty transects
expect_equal(length(which(is.na(clean_shrubreg$plot_id))), 0) # No empty plots
expect_equal(length(which(is.na(clean_shrubreg$shrubreg_id))), 0) # No empty IDs
expect_equal(length(which(is.na(clean_shrubreg$shrubreg_height))), 1) # Just one missing height!!!!!!

# Filtering Pinus sylvestris

ps_shrubreg <- clean_shrubreg %>% filter(main_sp == "Pinsylv") %>% 
  mutate(pair_id = paste0(region, "-", spot_status)) %>% ungroup()

ps_sh <- ps_shrubreg %>% filter(sp_type == "sh") %>% dplyr::select(-c(shrubreg_id, sp_type, plot_id, transect, main_sp, site))
ps_reg <- ps_shrubreg %>% filter(sp_type == "reg") %>% dplyr::select(-c(shrubreg_id, sp_type, plot_id, transect, main_sp, site))


ps_sh_count <- ps_sh %>%
  count(sp_id, spot_status, region, sp_name, pair_id,
        name = "n_obs")

ps_reg_count <- ps_reg %>%
  count(sp_id, spot_status, region, sp_name, pair_id,
        name = "n_obs")

write.csv(ps_sh_count, "matorral_marcos.csv")
write.csv(ps_reg_count, "regenerado_marcos.csv")
