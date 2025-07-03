rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "lme4", "sjPlot", "effects") #list of packages
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

clean_target$category <- paste0(clean_target$sp_id, "_", clean_target$spot_status)

# 5.- Wilcoxon test ####

# It is necessary to perform a Wilcoxon test to see differences between 
# groups (species + spot status). Since most of the data might not be normally 
# distributed, Wilcoxon test is recommended. The Bonferroni correction could 
# be made afterwards, but 'pairwise.wilcox.test' already has an argument that
# allows us to choose the correction method, so it can be done straightforward :)

# Variable selection. We will create two dataframes, one with "vulnerability" 
# variables, and another one with "response" variables

vuln_df <- clean_target %>% 
  rename(mean_bai = mean) %>% 
  dplyr::select(c(category, tree_number, height, dbh, 
                  age, hegyi_index, mean_bai, mean_1980, mean_20, mean_15,
                  mean_10, mean_05, Rt12, Rt17, Rt22, Rs12, Rs17))

resp_df <- clean_target %>% 
  mutate(cn_ratio = percent_c / percent_n) %>% 
  dplyr::select(c(category, tree_number, wc_22, ewt_22, sla_22,
                  total_chl_fw_22, chlor_a_fw_22, chlor_b_fw_22, chla_chlb_22, 
                  xc_fw_22, chl_xc_22, percent_c, percent_n, cn_ratio,
                  leaf_d13c, leaf_d15n, leaf_d18o,
                  wood_d13c_17, wood_d13c_22))

vuln_wilcox <- list()

for (i in 1:(ncol(vuln_df)-2)) { # Because category and tree_number are not numeric
 vuln_wilcox[[i]] <- pairwise.wilcox.test(vuln_df[, i+2], vuln_df$category,
                                          p.adjust.method = "bonferroni")
 print(i)
}

# Not enough values (too many NAs) for wood isotopes, hence wood variables will 
# be removed

resp_df <- resp_df %>% 
  dplyr::select(-c(wood_d13c_17, wood_d13c_22))

resp_wilcox <- list()

for (i in 1:(ncol(resp_df)-2)) { # Because category and tree_number are not numeric
  resp_wilcox[[i]] <- pairwise.wilcox.test(resp_df[, i+2], resp_df$category,
                                           p.adjust.method = "bonferroni")
  print(i)
} # All good :)

# 6.- Grouping results in a single df ####

# We need to create vectors with the name of the variable analyzed, which 
# is each name of the original vuln_ and resp_df

vuln_vars <- names(vuln_df)[3:ncol(vuln_df)]
resp_vars <- names(resp_df)[3:ncol(resp_df)]

# Function to convert p-values matrix to dataframe
convert_pw_wilcox <- function(result_list, var_names, tipo) {
  out <- list()
  
  for (i in seq_along(result_list)) {
    pvals <- result_list[[i]]$p.value
    if (is.null(pvals)) next  # Skip if NA
    
    df_long <- as.data.frame(as.table(pvals)) %>%
      rename(Group1 = Var1, Group2 = Var2, P_value_adjusted = Freq) %>%
      mutate(Variable = var_names[i], Tipo = tipo)
    
    out[[i]] <- df_long
  }
  
  bind_rows(out)
}


vuln_df_long <- convert_pw_wilcox(vuln_wilcox, vuln_vars, "vuln")
resp_df_long <- convert_pw_wilcox(resp_wilcox, resp_vars, "resp")

# 7.- Exporting ####

write.csv(vuln_df_long, "02_clean_data/06_01_pvals_bonferroni_vuln.csv")
write.csv(resp_df_long, "02_clean_data/06_01_pvals_bonferroni_resp.csv")

pairwise.wilcox.test(resp_df$total_chl_fw_22, resp_df$category,
                     p.adjust.method = "bonferroni")

# ewe <- pairwise.wilcox.test(vuln_df$Rs12, resp_df$category,
#                      p.adjust.method = "bonferroni")
# # multcompView::multcompLetters(ewe$p.value)
# 
# 
# pmat <- ewe$p.value
# 
# # Crear matriz completa simétrica
# full_pmat <- pmat
# full_pmat[upper.tri(full_pmat)] <- t(pmat)[upper.tri(pmat)]
# full_pmat <- t(full_pmat)
# 
# # Poner diagonal a 1 (p-valor de comparar grupo consigo mismo)
# diag(full_pmat) <- 1

# Ahora sí usar multcompLetters()
letters <- multcompView::multcompLetters(full_pmat)
print(letters)
