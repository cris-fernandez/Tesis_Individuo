rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate", "pointRes", "dplR") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd("C:/Users/recup/Universidad de Alcala/Ruiz Benito Paloma - raw_IB-ForRes")

# 1.- Reading .rwl data ####

# Locating the folder route with the chronologies: 

siteFiles <- paste(getwd(),"/2- Raw_brutos_organizados/LONG/RWL/", sep = "")

# Listing all RWL files so they can be read authomatically:

ListFiles <- paste(siteFiles, list.files(siteFiles, pattern = ".rwl"), sep = "")

RWLfiles <- lapply(ListFiles,read.rwl)

# This takes last characters from the 7th to the 4th last positions of the string
# (basically the site codes)

Sites <- substr(ListFiles, nchar(ListFiles)-7, nchar(ListFiles)-4) 

# 2.- Site names ####

MetaInfo <- data.frame(Sites)
MetaInfo$site <- c("Adobes","Alustante","Basari","Cedrillas","Corbalan",
                   "Fago","Guadarrama","Navas","Pelayos","Roncal","Sarries",
                   "SelvadeOza","Traid","Urzainki")
MetaInfo$SP <- c("Ps","Ps","Aa","Ps","Ps","Aa","Ps",
                 "Pp","Pp","Ps","Aa","Aa","Ps","Ps")
MetaInfo

# 3.- IDS generation ####

# In general: 3-4 characters for the site, 3-2 for the species
# One for the radii. NB each tree has one or two radii
# This identifies within each rwl. file what is a tree
# and what is a radii

## 3.1.- Reading IDS files ####

# Warnings should be ignored
EX.ids1 <- read.ids(RWLfiles[[1]], stc = c(4, 3, 1))
EX.ids2 <- read.ids(RWLfiles[[2]], stc = c(4, 3, 1))
EX.ids3 <- read.ids(RWLfiles[[3]], stc = c(2, 3, 2))
EX.ids4 <- read.ids(RWLfiles[[4]], stc = c(4, 2, 1))
EX.ids5 <- read.ids(RWLfiles[[5]], stc = c(4, 2, 1))
EX.ids6 <- read.ids(RWLfiles[[6]], stc = c(3, 3, 1))
EX.ids7 <- read.ids(RWLfiles[[7]], stc = c(4, 3, 1))
EX.ids8 <- read.ids(RWLfiles[[8]], stc = c(4, 3, 1))
EX.ids9 <- read.ids(RWLfiles[[9]], stc = c(4, 3, 1))
EX.ids10 <- read.ids(RWLfiles[[10]], stc = c(3, 3, 1))
EX.ids11 <- read.ids(RWLfiles[[11]], stc = c(3, 3, 1))
EX.ids12 <- read.ids(RWLfiles[[12]], stc = c(4, 3, 1))
EX.ids13 <- read.ids(RWLfiles[[13]], stc = c(4, 2, 1))
EX.ids14 <- read.ids(RWLfiles[[14]], stc = c(3, 3, 1))

# 4.- Averaging of radii ####

# Some trees have 2 radii and others 1, this way each tree has a single ring-width series

RWLtrees <- list()

for (i in 1:14) {
  RWLtrees[[i]] <- treeMean(RWLfiles[[i]], get(paste0("EX.ids", i)), na.rm = T)
  print(i)
}

#have a look at the names and check that they are correct
for(i in c(1:14)){
  
  print(Sites[i])
  print(names(RWLtrees[[i]]))
}
# These numbers should be the code of the tree

# 5.- Detrending ####

# We will use a 32 years spline

# To prevent some warnings from appearing, we have to remove some targets,
# as they cannot be detrended with spline due to the existance of NAs in the 
# middle of the chronology. I would like to ask IPE about this...

RWLtrees[[2]] <- RWLtrees[[2]][, colnames(RWLtrees[[2]]) != "117"]
RWLtrees[[6]] <- RWLtrees[[6]][, colnames(RWLtrees[[6]]) != "494"]
RWLtrees[[10]] <- RWLtrees[[10]][, colnames(RWLtrees[[10]]) != "366"]
RWLtrees[[12]] <- RWLtrees[[12]][, colnames(RWLtrees[[12]]) != "385"]

# Now the detrending:

RWItrans <- list()

for(i in 1:length(RWLtrees))
{
  print(i)
  RWItrans[[i]] <- detrend(RWLtrees[[i]], method = "Spline", nyrs = 32)
}

# 6.- BAI calculation ####

BAItrans <- list()

for(i in 1:length(RWLtrees))
{
  BAItrans[[i]] <- bai.out(RWLtrees[[i]])
  print(i)
}

# 7.- Pivot longer ####

for (i in 1:length(BAItrans)) {
  BAItrans[[i]] <- BAItrans[[i]] %>% 
    mutate(year = rownames(BAItrans[[i]])) %>% 
    pivot_longer(cols = -year,
                 names_to = "tree_number",
                 values_to = "bai")
  print(i)
}

BAItrans[[1]]
BAI_df <- list_rbind(BAItrans)

# 8.- Tidying tree ids ####

# First, I need to ensure all numbers have 3 digits, as the tree IDs follow 
# said structure:

BAI_df$tree_number <- str_pad(BAI_df$tree_number, 3, pad = "0")

# Now, I add "T" to the ID, so it matches plot, target and tree data.

BAI_df$tree_number <- paste0("T", BAI_df$tree_number, sep = "")

# 9.- Merging plot and dendro data ####

setwd("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/")

plot <- read.csv("02_clean_data/02_01_clean_plot.csv") %>% 
  dplyr::select(c(region, site, plot_id, spot_status))
target <- read.csv("02_clean_data/02_03_clean_target.csv") %>% 
  dplyr::select(c(plot_id, tree_number, sp_id,
                  height, dbh))

BAI_df2 <- full_join(target, BAI_df, by = "tree_number")

setdiff(BAI_df$tree_number, BAI_df2$tree_number)
setdiff(BAI_df2$tree_number, BAI_df$tree_number) # No dendro data from these 30 trees

BAI_df3 <- full_join(plot, BAI_df2, by = "plot_id")

# 10.- Exporting ####

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()
write.csv(BAI_df3, "02_clean_data/02_02_dendro_series.csv")
