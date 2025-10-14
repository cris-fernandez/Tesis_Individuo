rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggtext") #list of packages
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

# 2.- Removing 2023 data ####
# So I can have in the same column 2022 and 2023 values

clean_target <- clean_target %>% 
  dplyr::select(-contains("_23"))

# 3.- Additional IDs ####

clean_target$pair_id <- ifelse(grepl("NAV|PEL", clean_target$plot_id) == T, "Mad-Pinpine",
                               ifelse(grepl("GUA", clean_target$plot_id) == T, "Mad-Pinsylv",
                                      ifelse(grepl("ADO|TRA|ALU", clean_target$plot_id) == T, "Gua-Pinsylv",
                                             ifelse(grepl("COR|CED", clean_target$plot_id) == T, "Ter-Pinsylv",
                                                    ifelse(grepl("RON|URZ", clean_target$plot_id) == T, "Nav-Pinsylv",
                                                           ifelse(grepl("BAS|SAR", clean_target$plot_id) == T, "Nav-Abialba",
                                                                  ifelse(grepl("FAG|OZA", clean_target$plot_id) == T, "Hue-Abialba",
                                                                         "z")))))))

clean_target$vigor_id <- ifelse(clean_target$spot_status == "coldspot",
                                "cold_healthy",
                                ifelse(clean_target$prec < 30,
                                       "hot_healthy", "hot_damaged")) %>% 
  as.factor()

# 4.- Data corrections #####

clean_target$total_chl_fw_22 <- ifelse(clean_target$total_chl_fw_22 > 3000, NA,
                                       clean_target$total_chl_fw_22)
clean_target$xc_fw_22 <- ifelse(clean_target$xc_fw_22 > 2000, NA,
                                ifelse(clean_target$total_chl_fw_22 < 0, 
                                       NA, clean_target$xc_fw_22))
clean_target$chl_xc_22 <- ifelse(clean_target$chl_xc_22 < 0, NA,
                                 clean_target$chl_xc_22)
clean_target$chla_chlb_22 <- ifelse(clean_target$chla_chlb_22 < 0, NA,
                                    clean_target$chla_chlb_22)

clean_target$sp_id <- ifelse(clean_target$tree_number == "missing_1" | 
                               clean_target$tree_number == "missing_2",
                             "Pinsylv", clean_target$sp_id)

clean_target <- clean_target %>% 
  mutate(sp_id = factor(sp_id))

clean_target$sp_id <- fct_relevel(clean_target$sp_id, "Abialba", "Pinsylv", "Pinpine")

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

# 5.- Selecting variables ####

clean_target <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(height, dbh, percent_c, percent_n, cn_ratio, sla_22, age, hegyi_index, 
                  wc_22, total_chl_fw_22, chla_chlb_22, xc_fw_22, chl_xc_22, 
                  leaf_d13c, leaf_d15n, leaf_d18o_corrected, mean_1980, mean_05, 
                  Rt12, Rt17, Rt22, Rs12, Rs17, mean_def_obs))

colnames(clean_target) <- c("Height", "d.b.h.", "C content", "N content", 
                            "Leaf C:N", "SLA", "Age", "Hegyi Index",  "LWC", 
                            "Chl. content", "Chl. a / b", "Car. content", 
                            "Chl. / Car.", "Leaf δ13C", "Leaf δ15N", "Leaf δ18O", 
                            "BAI 1980", "BAI 05", "Rt 2012", "Rt 2017", "Rt 2022",
                            "Rs 2012", "Rs 2017", "Defoliation")


# 6.- Correlogram ####
# First I need to remove na values from the correlogram 

clean_target2 <- na.omit(clean_target)

# Correlogram 

correlogram <- cor(clean_target2)

# P-value matrix creation

p_matrix <- cor_pmat(clean_target2)

correlogram <- ggcorrplot(correlogram, 
                          type = "lower",
                          lab = TRUE,
                          method = "circle", 
                          p.mat = p_matrix, 
                          insig = "blank",
                          hc.order = FALSE)

# 7.- Assigning color per variable type ####

var_types <- tibble(var_name = colnames(clean_target2),
                    var_type = 
                      case_when(var_name %in% 
                                  c("Height", "d.b.h.", "C content", "N content", 
                                    "Leaf C:N", "SLA", "Age", "Hegyi Index") ~ "Morpho",
                                var_name %in%
                                  c("LWC", "Chl. content", "Chl. a / b", "Car. content", 
                                    "Chl. / Car.", "Leaf δ13C", "Leaf δ15N", 
                                    "Leaf δ18O") ~ "Physio",
                                var_name %in% 
                                  c("BAI 1980", "BAI 05", "Rt 2012", "Rt 2017", 
                                    "Rt 2022", "Rs 2012", "Rs 2017", 
                                    "Defoliation") ~ "Whole"))

var_palette <- c("Morpho" = "#440154FF",
                 "Physio" = "#39568CFF",
                 "Whole" = "#1F968BFF")

coloured_labs <- var_types %>%
  mutate(label_col = paste0("<span style='color:", var_palette[var_type], "'>", var_name, "</span>")) %>%
  pull(label_col)

coloured_labs_y <- coloured_labs[1:23]
coloured_labs_x <- coloured_labs[2:24]

# Adding it to the graph: 
correlogram2 <- correlogram +
  scale_x_discrete(labels = coloured_labs_x) + 
  scale_y_discrete(labels = coloured_labs_y) + 
  theme(axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1, size = 12),
        axis.text.y = ggtext::element_markdown(size = 12))

# 8.- Plotting ####

tiff("04_figures/30_01_correlogram.tiff", units = "mm", 
     width = 300, height = 300,
     res = 700, compression = "lzw")
correlogram2
dev.off()

# How many significant correlations? 

sum(p_matrix < 0.05) / 2 #199
