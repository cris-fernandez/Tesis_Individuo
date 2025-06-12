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
                         header = T, sep = ",") %>% select(-X) %>% 
  mutate(site = substr(plot_id, 1, 3))

# 2.- Removing 2023 data ####
# So I can have in the same column 2022 and 2023 values

clean_target <- clean_target %>% 
  select(-contains("_23"))

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
# distributed, Wilcoxon test is recommended

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
                  chl_fw_22, chlor_a_fw_22, chlor_b_fw_22, chla_chlb_22, 
                  xc_fw_22, chl_xc_22, percent_c, percent_n, cn_ratio,
                  leaf_d13c, leaf_d15n, leaf_d18o,
                  wood_d13c_17, wood_d13c_22))

pairwise.wilcox.test(density_df$mainsp_density, density_df$spot_status)

# Inicializamos lista para guardar resultados

phys_vars <- names(vuln_df)[sapply(vuln_df, is.numeric)]

# Inicializamos lista para guardar resultados
results <- list()

# Loop por especie y variable
for (sp in unique(vuln_df$sp_id)) {
  df_sp <- subset(vuln_df, sp_id == sp)
  
  for (var in phys_vars) {
    # Extraer valores por grupo
    vals_hot <- df_sp[df_sp$spot_status == "hotspot", var, drop = TRUE]
    vals_cold <- df_sp[df_sp$spot_status == "coldspot", var, drop = TRUE]
    
    # Solo si hay datos suficientes
    if (length(vals_hot) > 2 & length(vals_cold) > 2) {
      test <- wilcox.test(vals_hot, vals_cold)
      results[[length(results) + 1]] <- data.frame(
        sp_id = sp,
        variable = var,
        p_value = test$p.value
      )
    }
  }
}

# Unir todos los resultados
results_df <- do.call(rbind, results)

# Corrección de Bonferroni
results_df$p_adj <- p.adjust(results_df$p_value, method = "bonferroni")
results_df$significant <- results_df$p_adj < 0.05

# Mostrar solo diferencias significativas
sig_results <- subset(results_df, significant == TRUE)
print(sig_results[order(sig_results$p_adj), ])
