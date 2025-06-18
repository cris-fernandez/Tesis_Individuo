rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra") #list of packages
new_pck <- pck[!(pck %in% installed.packages()[,"Package"])] #new packages (not installed ones)
if(length(new_pck)) install.packages(new_pck) #install new packages
lapply(pck, library, character.only=T) #load all packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd('..')
getwd()

# 1.- Reading plot data ####

clean_plot <- read.csv("C:/Users/recup/Universidad de Alcala/IBFORRES/git_local_ibforres/Database_IBFORRES/05_outputs/03_01_result_plot.csv", 
                       header = T, sep = ",") %>% 
  mutate(site = substr(plot_id, 1, 3))

# 2.- Selecting variables ####

clean_plot <- clean_plot %>% 
  dplyr::select(c(Ca_ppm_mean, P_ppm_sd, K_ppm_mean, pH_H2O_sd, clay_perc_sd,
                  Ca_ppm_sd, N_perc_sd, loam_perc_sd, sand_perc_sd, P_ppm_mean,
                  CaCO3_perc_mean, N_perc_mean, pH_H2O_mean, clay_perc_mean,
                  sand_perc_mean, loam_perc_mean)) %>% 
  select(sort(names(.)))

colnames(clean_plot) <- c("Ca content (mean)", "Ca content (sd)", 
                          "CacCO3 content (mean)", 
                          "Clay proportion (mean)", "Clay proportion (sd)",
                          "K content (mean)", 
                          "Loam proportion (mean)", "Loam proportion (sd)",
                          "N content (mean)", "N content (sd)",
                          "P content (mean)", "P content (sd)",
                          "Soil pH (mean)", "Soil pH (sd)",
                          "Sand proportion (mean)", "Sand proportion (sd)")

# 3.- Correlogram ####
# First I need to remove na values from the correlogram 

clean_plot2 <- na.omit(clean_plot)

# Now I make the correlogram and reorder the variables in alphabetical order

correlogram <- cor(clean_plot2)
orden <- sort(colnames(correlogram)) %>% rev()
correlogram <- correlogram[orden, orden]

# P-value matrix creation, also by alphabetical order

p_matrix <- cor_pmat(clean_plot2)
p_matrix <- p_matrix[orden, orden]

correlogram <- ggcorrplot(correlogram, 
                          type = "lower",
                          lab = TRUE,
                          method = "circle", 
                          p.mat = p_matrix, 
                          insig = "blank")

# 7.- Plotting ####

tiff("04_figures/04_03_correlogram_soil.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
correlogram
dev.off()

