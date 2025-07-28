rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis") #list of packages
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

# Adding T290 defoliation info:

clean_target <- clean_target %>% 
  mutate(mean_def_obs = ifelse(tree_number == "T290", 15, mean_def_obs))

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
                                ifelse(clean_target$mean_def_obs < 25,
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

clean_target2 <- clean_target %>% 
  mutate(sp_id = "all")

clean_target <- rbind(clean_target, clean_target2)
clean_target$sp_id <- as.factor(clean_target$sp_id)
clean_target <- clean_target %>%
  mutate(sp_id = fct_relevel(sp_id, "all", "Abialba", "Pinsylv", "Pinpine"),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

# 5.- Wilcoxon test ####

# It is necessary to perform a Wilcoxon test to see differences between 
# groups (species + spot status). Since most of the data might not be normally 
# distributed, Wilcoxon test is recommended. The Bonferroni correction could 
# be made afterwards, but 'pairwise.wilcox.test' already has an argument that
# allows us to choose the correction method, so it can be done straightforward :)

# Variable selection. We will create two dataframes, one with "vulnerability" 
# variables, and another one with "response" variables

vars_df <- clean_target %>% 
  rename(mean_bai = mean) %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  dplyr::select(c(sp_id, vigor_id, tree_number, height, dbh, 
                  age, hegyi_index, mean_bai, mean_1980, mean_20, mean_15,
                  mean_10, mean_05, Rt12, Rt17, Rt22, Rs12, Rs17, wc_22, sla_22,
                  total_chl_fw_22, chlor_a_fw_22, chlor_b_fw_22, chla_chlb_22, 
                  xc_fw_22, chl_xc_22, percent_c, percent_n, cn_ratio,
                  leaf_d13c, leaf_d15n, leaf_d18o,
                  mean_def_obs)) # Had to remove wood isotopes bcs not enough variables

# Filtering per species:

all_target <- vars_df %>% filter(sp_id == "all")
aa_target <- vars_df %>% filter(sp_id == "Abialba")
ps_target <- vars_df %>% filter(sp_id == "Pinsylv")
pp_target <- vars_df %>% filter(sp_id == "Pinpine")

# All:
allvars_wilcox <- list()

for (i in 1:(ncol(all_target)-3)) { # Because vigor_id and tree_number are not numeric
  allvars_wilcox[[i]] <- pairwise.wilcox.test(all_target[, i+3], all_target$vigor_id,
                                           p.adjust.method = "bonferroni")
  print(i)
}

# Abies alba:
aavars_wilcox <- list()

for (i in 1:(ncol(aa_target)-3)) { # Because vigor_id and tree_number are not numeric
  aavars_wilcox[[i]] <- pairwise.wilcox.test(aa_target[, i+3], aa_target$vigor_id,
                                              p.adjust.method = "bonferroni")
  print(i)
}

# Pinus sylvestris:
psvars_wilcox <- list()

for (i in 1:(ncol(ps_target)-3)) { # Because vigor_id and tree_number are not numeric
  psvars_wilcox[[i]] <- pairwise.wilcox.test(ps_target[, i+3], ps_target$vigor_id,
                                             p.adjust.method = "bonferroni")
  print(i)
}

# Pinus pinea:
ppvars_wilcox <- list()

for (i in 1:(ncol(pp_target)-3)) { # Because vigor_id and tree_number are not numeric
  ppvars_wilcox[[i]] <- pairwise.wilcox.test(pp_target[, i+3], pp_target$vigor_id,
                                             p.adjust.method = "bonferroni")
  print(i)
}

# 6.- Grouping results in a single df ####

# We need to create vectors with the name of the variable analyzed, which 
# is each name of the original vuln_ and resp_df

names_vars <- names(all_target)[3:ncol(all_target)] # They should all remain constant

# Function to convert p-values matrix to dataframe
convert_pw_wilcox <- function(result_list, var_names, tipo) {
  out <- list()
  
  for (i in seq_along(result_list)) {
    pvals <- result_list[[i]]$p.value
    if (is.null(pvals)) next  # Skip if NA
    
    df_long <- as.data.frame(as.table(pvals)) %>%
      rename(Group1 = Var1, Group2 = Var2, P_value_adjusted = Freq) %>%
      mutate(Variable = names_vars[i], Tipo = tipo)
    
    out[[i]] <- df_long
  }
  
  bind_rows(out)
}


all_df_long <- convert_pw_wilcox(allvars_wilcox, names_vars, "var") %>% na.omit()
aa_df_long <- convert_pw_wilcox(aavars_wilcox, names_vars, "var") %>% na.omit()
ps_df_long <- convert_pw_wilcox(psvars_wilcox, names_vars, "var") %>% na.omit()
pp_df_long <- convert_pw_wilcox(ppvars_wilcox, names_vars, "var") %>% na.omit()


# 7.- Df arrangement ####
# Column telling whether it is significant or not!
all_df_long$significant <- ifelse(all_df_long$P_value_adjusted < 0.05, 1, 0)
aa_df_long$significant <- ifelse(aa_df_long$P_value_adjusted < 0.05, 1, 0)
ps_df_long$significant <- ifelse(ps_df_long$P_value_adjusted < 0.05, 1, 0)
pp_df_long$significant <- ifelse(pp_df_long$P_value_adjusted < 0.05, 1, 0)


# 8.- Exporting ####
# write.csv(status_pairs, "02_clean_data/12_02_pvals_bonferroni_sp.csv")