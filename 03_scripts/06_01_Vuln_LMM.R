rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats", "GGally", "MuMIn", "corrr", "ggcorrplot","ggfortify", 
        "FactoMineR", "factoextra", "lme4", "sjPlot", "effects", "car") #list of packages
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

# 5.- Reading SPEI data ####

spei <- read.csv("02_clean_data/02_00_spei_series.csv") %>% 
  select(-X) %>% filter(month == 7)

spei12 <- spei %>% 
  select(c(plot_id, year, spei12)) %>% 
  group_by(plot_id) %>% 
  summarise(mean_spei12 = mean(spei12, na.rm = T))

clean_target <- full_join(clean_target, spei12, by = "plot_id")

clean_target <- clean_target %>% 
  filter(!is.na(sp_id)) %>% 
  filter(sp_id == "Pinsylv")

# 6.- Reading Prcp data ####

climate <- read.csv("02_clean_data/02_00_climate_full_data.csv") %>% 
  dplyr::select(-X) %>% 
  filter(year > 1979) %>% 
  group_by(plot_id) %>% 
  summarise(Prcp = mean(MAP, na.rm = T),
            Tmax = mean(T_max, na.rm = T),
            Tmin = mean(T_min, na.rm = T))

clean_target <- full_join(clean_target, climate, by = "plot_id")

# 7.- Selecting variables ####

pca_target <- clean_target %>% 
  dplyr::select(c(tree_number, site, plot_id, mean_def_obs, height, age, hegyi_index, 
                  mean_1980, Rt12, Rt17, Rt22, Rs12, Rs17)) %>% 
  select(sort(names(.))) %>% na.omit()

# 7.- PCA of vulnerability ####

## 7.1.- Standardization ####

norm_target <- pca_target %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         age_ST = (age - mean(age, na.rm = T)) / sd(age, na.rm = T),
         hegyi_index_ST = (hegyi_index - mean(hegyi_index, na.rm = T)) / sd(hegyi_index, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T),
         Rt12_ST = (Rt12 - mean(Rt12, na.rm = T)) / sd(Rt12, na.rm = T),
         Rs12_ST = (Rs12 - mean(Rs12, na.rm = T)) / sd(Rs12, na.rm = T),
         Rt17_ST = (Rt17 - mean(Rt17, na.rm = T)) / sd(Rt17, na.rm = T),
         Rs17_ST = (Rs17 - mean(Rs17, na.rm = T)) / sd(Rs17, na.rm = T),
         Rt22_ST = (Rt22 - mean(Rt22, na.rm = T)) / sd(Rt22, na.rm = T))

norm_target <- norm_target %>% 
  select(c(contains("_ST"), mean_def_obs, plot_id, tree_number, site)) %>% 
  na.omit()

## 7.2.- Correlogram ####

norm_target2 <- norm_target %>% 
  dplyr::select(-c(mean_def_obs, plot_id, tree_number, site))

correlogram <- cor(norm_target2)
ggcorrplot(correlogram)

# 8.- PCA analysis ####

pca_results <- prcomp(norm_target2, scale. = FALSE)
summary(pca_results)

# The first three components over 60% of the data variance

components <- pca_results$x[, 1:3]

# 9.- Joining PCA results ####

model_df <- cbind(norm_target, components) %>% 
  dplyr::select(c(tree_number, PC1, PC2, PC3))

clean_target <- full_join(clean_target, model_df, by = "tree_number") %>% 
  dplyr::select(c(plot_id, tree_number, site, mean_def_obs, PC1, PC2, PC3, mean_spei12,
                  Prcp, Tmax, Tmin, height, age, hegyi_index, 
                  mean_1980, Rt12, Rt17, Rt22, Rs12, Rs17)) %>% 
  na.omit() %>%
  mutate(height_ST = (height - mean(height, na.rm = T)) / sd(height, na.rm = T),
         age_ST = (age - mean(age, na.rm = T)) / sd(age, na.rm = T),
         hegyi_index_ST = (hegyi_index - mean(hegyi_index, na.rm = T)) / sd(hegyi_index, na.rm = T),
         bai_1980_ST = (mean_1980 - mean(mean_1980, na.rm = T)) / sd(mean_1980, na.rm = T),
         Rt12_ST = (Rt12 - mean(Rt12, na.rm = T)) / sd(Rt12, na.rm = T),
         Rs12_ST = (Rs12 - mean(Rs12, na.rm = T)) / sd(Rs12, na.rm = T),
         Rt17_ST = (Rt17 - mean(Rt17, na.rm = T)) / sd(Rt17, na.rm = T),
         Rs17_ST = (Rs17 - mean(Rs17, na.rm = T)) / sd(Rs17, na.rm = T),
         Rt22_ST = (Rt22 - mean(Rt22, na.rm = T)) / sd(Rt22, na.rm = T),
         Prcp_ST = (Prcp - mean(Prcp, na.rm = T)) / sd(Prcp, na.rm = T),
         Tmax_ST = (Rt22 - mean(Tmax, na.rm = T)) / sd(Tmax, na.rm = T),
         Tmin_ST = (Prcp - mean(Tmin, na.rm = T)) / sd(Tmin, na.rm = T),
         SPEI12_ST = (mean_spei12 - mean(mean_spei12, na.rm = T)) / sd(mean_spei12, na.rm = T))

str(clean_target)

# 10.- Model ####

# modelo_vuln <- lmer(mean_def_obs ~ PC1*mean_spei12 + PC2* mean_spei12 + PC3*mean_spei12 + (1 | site/plot_id), 
#                    data = clean_target)
# modelo_vuln <- lmer(mean_def_obs ~ PC1 + PC2 + PC3 + mean_spei12 + (1 | site), 
#                     data = clean_target) Sigue aumentando el REML

# modelo_vuln <- lmer(mean_def_obs ~ PC1*Prcp_ST + 
#                       PC2* Prcp_ST + PC3*Prcp_ST + (1 | site/plot_id), 
#                     data = clean_target) Mejor pero pf

# modelo_vuln <- lmer(mean_def_obs ~ PC1 + PC2 + PC3 + Prcp_ST +
#        height_ST + age_ST + hegyi_index_ST + bai_1980_ST + 
#        Rt12_ST + Rs12_ST + (1 | site), data = clean_target)
#                     
# I dont think it makes statistical sense to include PCs and the variables that 
# make them up...

modelo_vuln <- lmer(mean_def_obs ~ SPEI12_ST + Tmax_ST + Prcp_ST +
                    height_ST + age_ST + hegyi_index_ST + bai_1980_ST + 
                    Rt12_ST + Rs12_ST + Rt17_ST + Rs17_ST + Rt22_ST + 
                      (1 | site), data = clean_target)

summary(modelo_vuln)

plot(modelo_vuln) 

# There are some assymmetries that should not be there ideally...

# 11.- VIF ####

# Variance inflation factor, it helps us detect multicolinearity problems 
# among the fixed effects (not the random variables)

lm_vuln <- lm(mean_def_obs ~ SPEI12_ST + Tmax_ST + Prcp_ST +
                      height_ST + age_ST + hegyi_index_ST + bai_1980_ST + 
                      Rt12_ST + Rs12_ST + Rt17_ST + Rs17_ST + Rt22_ST, 
              data = clean_target)
vif(lm_vuln)
alias(lm_vuln) # Rt22 is giving problems


lm_vuln <- lm(mean_def_obs ~ SPEI12_ST + Tmax_ST + Prcp_ST +
                height_ST + age_ST + hegyi_index_ST + bai_1980_ST + 
                Rt12_ST + Rs12_ST + Rt17_ST + Rs17_ST,
              data = clean_target)
vif(lm_vuln)

# No major colninarity problems, as everything stays below 3 (some prefer 
# to use 10 as the value of reference, so even better in that case!)

# 12.- Dredge ####

# Dredge compares models by their REML value, which gives us an idea of 
# how well their fit is. Smaller REML values imply a better fit, and therefore
# a more precise model

modelo_vuln <- lmer(mean_def_obs ~ SPEI12_ST + Tmax_ST + Prcp_ST +
                      height_ST + age_ST + hegyi_index_ST + bai_1980_ST + 
                      Rt12_ST + Rs12_ST + Rt17_ST + Rs17_ST + # Now without Rt22 
                      (1 | site), data = clean_target)

options(na.action = "na.fail") # Crucial for dredge 

dredge_vuln <- dredge(modelo_vuln) 

# Lets see all models with delta < 2
dredge_vuln <- subset(dredge_vuln, delta < 2)

# Model 1916 is the most parsimonious one, with 2 variables less! 

modelo_vuln <- lmer(mean_def_obs ~ SPEI12_ST + Tmax_ST + Prcp_ST +
                      height_ST + age_ST +  bai_1980_ST + 
                      Rs12_ST + Rt17_ST + Rs17_ST + 
                      (1 | site/plot_id), data = clean_target)
summary(modelo_vuln)

# 13.- 

anova(modelo_vuln)
plot_model(modelo_vuln, type = "pred", terms = c("mean_spei12"))

plot(modelo_vuln) # No pattern, I guess
qqnorm(resid(modelo_vuln))
qqline(resid(modelo_vuln))
performance::check_model(modelo_vuln)

##### 6.1.2.-  Effect plotting #####

ef_all <- Effect(c("mean_spei12"), modelo_vuln)
ef_all <- ggplot(as.data.frame(ef_all),
                 aes(mean_spei12, fit, colour = "red", 
                     fill = "red")) +
  geom_line() +
  ## colour = NA suppresses edges of the ribbon
  geom_ribbon(colour = NA, alpha = 0.1,
              aes(ymin = lower, ymax = upper)) +
  ## add rug plot based on original data
  geom_rug(data = ef_all$data,aes(y=NULL), sides = "b") +
  ylab("mean_def_obs") + xlab("") + labs(tag = "(b)") +
  theme_classic() +
  theme(legend.position = "none")

ef_all
