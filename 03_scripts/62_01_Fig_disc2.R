rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "ggplot2", "ggbiplot", "ggfortify", "MASS", 
        "viridis", "lme4", "lmerTest", "emmeans", "mgcv", "broom.mixed", "xlsx") #list of packages
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
  mutate(sp_id = factor(sp_id),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

clean_target$cn <- clean_target$percent_c / clean_target$percent_n

clean_target <- clean_target %>% filter(mean_def_obs < 100)

clean_target$site <- as.factor(clean_target$site)

# Transforming spot status into a factor so it can be modellised:

clean_target$spot_status <- as.factor(clean_target$spot_status)

# Filtering by species: 

aa_target <- clean_target %>% 
  filter(sp_id == "Abialba") %>% 
  mutate(d13c_centered = 1 + (leaf_d13c - min(leaf_d13c, na.rm = TRUE)) / 
           (max(leaf_d13c, na.rm = TRUE) - min(leaf_d13c, na.rm = TRUE)),
         d15n_centered = 1 + (leaf_d15n - min(leaf_d15n, na.rm = TRUE)) / 
           (max(leaf_d15n, na.rm = TRUE) - min(leaf_d15n, na.rm = TRUE)),
         d18o_centered = 1 + (leaf_d18o_corrected - min(leaf_d18o_corrected, na.rm = TRUE)) / 
           (max(leaf_d18o_corrected, na.rm = TRUE) - min(leaf_d18o_corrected, na.rm = TRUE)))

ps_target <- clean_target %>% 
  filter(sp_id == "Pinsylv") %>% 
  mutate(d13c_centered = 1 + (leaf_d13c - min(leaf_d13c, na.rm = TRUE)) / 
           (max(leaf_d13c, na.rm = TRUE) - min(leaf_d13c, na.rm = TRUE)),
         d15n_centered = 1 + (leaf_d15n - min(leaf_d15n, na.rm = TRUE)) / 
           (max(leaf_d15n, na.rm = TRUE) - min(leaf_d15n, na.rm = TRUE)),
         d18o_centered = 1 + (leaf_d18o_corrected - min(leaf_d18o_corrected, na.rm = TRUE)) / 
           (max(leaf_d18o_corrected, na.rm = TRUE) - min(leaf_d18o_corrected, na.rm = TRUE)))

pp_target <- clean_target %>% 
  filter(sp_id == "Pinpine") %>% 
  mutate(d13c_centered = 1 + (leaf_d13c - min(leaf_d13c, na.rm = TRUE)) / 
           (max(leaf_d13c, na.rm = TRUE) - min(leaf_d13c, na.rm = TRUE)),
         d15n_centered = 1 + (leaf_d15n - min(leaf_d15n, na.rm = TRUE)) / 
           (max(leaf_d15n, na.rm = TRUE) - min(leaf_d15n, na.rm = TRUE)),
         d18o_centered = 1 + (leaf_d18o_corrected - min(leaf_d18o_corrected, na.rm = TRUE)) / 
           (max(leaf_d18o_corrected, na.rm = TRUE) - min(leaf_d18o_corrected, na.rm = TRUE)))

# 5.- Variable selection ####

var_list <- c("height", "dbh", "hegyi_index", "wc_22", "percent_c", "percent_n",
              "cn", "sla_22", "age", 
              "chlor_a_fw_22", "chlor_b_fw_22", "total_chl_fw_22", "xc_fw_22", 
              "chla_chlb_22", "chl_xc_22", "d13c_centered", "d15n_centered", 
              "d18o_centered",
              "mean_1980", "mean_05", "Rt12", "Rt17", "Rt22", "Rs12", "Rs17")

# 8.- Abies alba ####

model_list_aa <- list()
confint_aa <- data.frame()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    '~ spot_status + s(site, bs = "re")'))
  model_list_aa[[i]] <- mgcv::gam(model_formula, data = aa_target, 
                               method= "ML")
  ci <- summary(emmeans(model_list_aa[[i]], ~ spot_status))
  ci$variable <- var_list[i]
  
  confint_aa <- bind_rows(confint_aa, ci)
  print(i)
}

# 9.- Pinus sylvestris ####

model_list_ps <- list()
confint_ps <- data.frame()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    '~ spot_status + s(site, bs = "re")'))
  model_list_ps[[i]] <- mgcv::gam(model_formula, data = ps_target, 
                                  method= "ML")
  ci <- summary(emmeans(model_list_ps[[i]], ~ spot_status))
  ci$variable <- var_list[i]
  
  confint_ps <- bind_rows(confint_ps, ci)
  print(i)
}

# 10.- Pinus pinea ####

model_list_pp <- list()
confint_pp <- data.frame()

for (i in 1:length(var_list)) {
  model_formula <- as.formula(paste(var_list[i], 
                                    '~ spot_status + s(site, bs = "re")'))
  model_list_pp[[i]] <- mgcv::gam(model_formula, data = pp_target, 
                                  method= "ML")
  ci <- summary(emmeans(model_list_pp[[i]], ~ spot_status))
  ci$variable <- var_list[i]
  
  confint_pp <- bind_rows(confint_pp, ci)
  print(i)
}

# 11.- Abies alba figure ####

new_order <- c("height", "dbh", "percent_c", "percent_n",
               "cn", "sla_22", "age", "hegyi_index", 
               "wc_22", "total_chl_fw_22", "chlor_a_fw_22", "chlor_b_fw_22",  
               "chla_chlb_22", "xc_fw_22", "chl_xc_22", "d13c_centered", "d15n_centered", 
              "d18o_centered", "mean_1980", "mean_05", "Rt12", "Rt17", "Rt22", "Rs12", "Rs17")

confint_aa$variable <- factor(confint_aa$variable, levels = new_order)
confint_aa$significant <- ifelse(confint_aa$variable %in% c("Rt12", "Rt22"), "yes", "no")

fig_aa <- ggplot(confint_aa) + 
  geom_point(aes(x = variable, y = emmean, colour = spot_status,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = variable, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_x_discrete(labels = c("height" = "Height",
                              "dbh" = "DBH",
                              "percent_c" = "%C",
                              "percent_n" = "%N",
                              "cn" = "C:N",
                              "sla_22" = "SLA",
                              "age" = "Age",
                              "hegyi_index" = "Hegyi",
                              "wc_22" = "LWC",
                              "total_chl_fw_22" = "Chl.",
                              "chlor_a_fw_22" = "Chl a",
                              "chlor_b_fw_22" = "Chl b",
                              "chla_chlb_22" = "Chl a:b",
                              "xc_fw_22" = "Carot.",
                              "chl_xc_22" = "Chl:car.",
                              "d13c_centered" = "δ13C",
                              "d15n_centered" = "δ15N",
                              "d18o_centered" = "δ18O",
                              "mean_1980" = "BAI80",
                              "mean_05" = "BAI05",
                              "Rt12" = "Rt12",
                              "Rt17" = "Rt17",
                              "Rt22" = "Rt22",
                              "Rs12" = "Rs12",
                              "Rs17" = "Rs17")) + 
  xlab("") + 
  ylab("A. alba") + 
  scale_y_log10() +   
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 20))

# 12.- Pinus sylvestris figure ####

confint_ps$variable <- factor(confint_ps$variable, levels = new_order)
confint_ps$significant <- ifelse(confint_ps$variable %in% 
                                   c("age", "hegyi_index", "total_chl_fw_22", 
                                     "chlor_a_fw_22", "chlor_b_fw_22", 
                                     "chla_chlb_22", "xc_fw_22", "d15n_centered", 
                                     "mean_1980", "mean_05"), "yes", "no")

fig_ps <- ggplot(confint_ps) + 
  geom_point(aes(x = variable, y = emmean, colour = spot_status,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = variable, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_x_discrete(labels = c("height" = "Height",
                              "dbh" = "DBH",
                              "percent_c" = "%C",
                              "percent_n" = "%N",
                              "cn" = "C:N",
                              "sla_22" = "SLA",
                              "age" = "Age",
                              "hegyi_index" = "Hegyi",
                              "wc_22" = "LWC",
                              "total_chl_fw_22" = "Chl.",
                              "chlor_a_fw_22" = "Chl a",
                              "chlor_b_fw_22" = "Chl b",
                              "chla_chlb_22" = "Chl a:b",
                              "xc_fw_22" = "Carot.",
                              "chl_xc_22" = "Chl:car.",
                              "d13c_centered" = "δ13C",
                              "d15n_centered" = "δ15N",
                              "d18o_centered" = "δ18O",
                              "mean_1980" = "BAI80",
                              "mean_05" = "BAI05",
                              "Rt12" = "Rt12",
                              "Rt17" = "Rt17",
                              "Rt22" = "Rt22",
                              "Rs12" = "Rs12",
                              "Rs17" = "Rs17")) + 
  xlab("") + 
  ylab("P. sylvestris") + 
  scale_y_log10() +   
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 20))

# 13.- Pinus pinea figure ####

confint_pp$variable <- factor(confint_pp$variable, levels = new_order)
confint_pp$significant <- "no"

fig_pp <- ggplot(confint_pp) + 
  geom_point(aes(x = variable, y = emmean, colour = spot_status,
                 alpha = significant), size = 3, 
             position = position_dodge(width = 0.3)) + 
  geom_errorbar(aes(x = variable, ymin = lower.CL, ymax = upper.CL, 
                    colour = spot_status, alpha = significant),
                linewidth = 1.5, width = 0,
                position = position_dodge(width = 0.3)) +
  scale_color_manual(breaks = c("coldspot", "hotspot"),
                     values = c("coldspot" = "#2274A5",
                                "hotspot" = "#D71515"),
                     labels = c("Non-declining",
                                "Declining"),
                     name = "") + 
  scale_x_discrete(labels = c("height" = "Height",
                              "dbh" = "DBH",
                              "percent_c" = "%C",
                              "percent_n" = "%N",
                              "cn" = "C:N",
                              "sla_22" = "SLA",
                              "age" = "Age",
                              "hegyi_index" = "Hegyi",
                              "wc_22" = "LWC",
                              "total_chl_fw_22" = "Chl.",
                              "chlor_a_fw_22" = "Chl a",
                              "chlor_b_fw_22" = "Chl b",
                              "chla_chlb_22" = "Chl a:b",
                              "xc_fw_22" = "Carot.",
                              "chl_xc_22" = "Chl:car.",
                              "d13c_centered" = "δ13C",
                              "d15n_centered" = "δ15N",
                              "d18o_centered" = "δ18O",
                              "mean_1980" = "BAI80",
                              "mean_05" = "BAI05",
                              "Rt12" = "Rt12",
                              "Rt17" = "Rt17",
                              "Rt22" = "Rt22",
                              "Rs12" = "Rs12",
                              "Rs17" = "Rs17")) + 
  xlab("") + 
  ylab("P. pinea") + 
  scale_y_log10() +   
  theme_classic() + 
  theme(axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 30),
        legend.text = element_text(size = 20))

# 14.- Plotting ####

tiff("04_figures/62_01_MegaFig_d2.tiff", units = "mm", width = 640, height = 320,
     res = 400, compression = "lzw")
fig_aa / fig_ps / fig_pp + plot_layout(guides = "collect")
dev.off()