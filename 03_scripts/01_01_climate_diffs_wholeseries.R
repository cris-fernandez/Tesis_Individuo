rm(list=ls()) #Clearing Gl environment

pck<- c("tidyverse", "dplyr", "patchwork", "grid", "easyclimate",
        "ggprism", "forcats") #list of packages
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
# clean_target <- read.csv("C:/Users/crist/Documents/Database_IBFORRES/05_outputs/03_03_result_target.csv",
#                          header = T, sep = ",") %>% dplyr::select(-X) %>%
#   mutate(site = substr(plot_id, 1, 3)) # PC office

clean_target$pair_id <- ifelse(grepl("NAV|PEL", clean_target$plot_id) == T, "Mad-Pinpine",
                               ifelse(grepl("GUA", clean_target$plot_id) == T, "Mad-Pinsylv",
                                      ifelse(grepl("ADO|TRA|ALU", clean_target$plot_id) == T, "Gua-Pinsylv",
                                             ifelse(grepl("COR|CED", clean_target$plot_id) == T, "Ter-Pinsylv",
                                                    ifelse(grepl("RON|URZ", clean_target$plot_id) == T, "Nav-Pinsylv",
                                                           ifelse(grepl("BAS|SAR", clean_target$plot_id) == T, "Nav-Abialba",
                                                                  ifelse(grepl("FAG|OZA", clean_target$plot_id) == T, "Hue-Abialba",
                                                                         "z")))))))


# 2.- Climate ####

climate_data_series <- read.csv("02_clean_data/02_00_climate_series.csv") %>% 
  dplyr::select(-X) %>% 
  mutate(Tmean = (Tmax + Tmin) / 2)

# 3.- Average series ####
# Preparing an "average series" for declining vs. non-declining stands 
# per site 

clean_target <- clean_target %>% 
  dplyr::select(c(plot_id, pair_id, spot_status)) %>% 
  mutate(site_code = paste0(pair_id, "-", spot_status))

plot_series <- full_join(clean_target, climate_data_series, by = "plot_id")

avg_series <- plot_series %>% 
  group_by(year, site_code) %>% 
  summarise(Tmean = mean(Tmean, na.rm = T),
            Prcp = mean(Prcp, na.rm = T))

# Separating them into spot_status and pair_id again:
avg_series <- avg_series %>%
  mutate(spot_status = ifelse(grepl("Hotspot$", site_code),
                              "Hotspot", "Coldspot"),
         pair_id = sub("-(Coldspot|Hotspot)$", "", site_code)) 

avg_temp <- avg_series %>% 
  dplyr::select(-c(Prcp, site_code)) %>% 
  pivot_wider(names_from = spot_status,
              values_from = Tmean) %>%
  mutate(temp_diff = Hotspot - Coldspot)

avg_temp %>%
  group_by(pair_id) %>%
  summarise(p_value = wilcox.test(Coldspot, Hotspot, paired = TRUE)$p.value) %>% 
  mutate(p_bonferroni = p.adjust(p_value, method = "bonferroni"))


# Nav-Abialba
navaa <- avg_temp %>% filter(pair_id == "Nav-Pinsylv")
wilcox.test(navaa$Coldspot, navaa$Hotspot, paired = T)

# Hue-Abialba

avg_temp %>%
  group_by(pair_id) %>%
  summarise(
    mean_diff = mean(Hotspot - Coldspot, na.rm = TRUE),
    sd_diff = sd(Hotspot - Coldspot, na.rm = TRUE),
    min_diff = min(Hotspot - Coldspot, na.rm = TRUE),
    max_diff = max(Hotspot - Coldspot, na.rm = TRUE),
    .groups = "drop"
  )

# 
# wilcox_clay <- soils_sites %>% 
#   group_by(pair_id) %>% 
#   summarise(p = wilcox.test(clay_perc ~ spot_status)$p.value) %>% 
#   mutate(p_bonf = p.adjust(p, method = "bonferroni"))
# 
# hist(soils_sites$sand_perc)
# wilcox_sand <- soils_sites %>% 
#   group_by(pair_id) %>% 
#   summarise(p = wilcox.test(sand_perc ~ spot_status)$p.value) %>% 
#   mutate(p_bonf = p.adjust(p, method = "bonferroni"))




library(tidyverse)
library(nlme)

gls_results <- avg_temp %>%
  ungroup() %>%
  group_by(pair_id) %>%
  group_modify(~ {
    
    dat <- .x %>%
      filter(is.finite(temp_diff)) %>%
      arrange(year)
    
    model <- gls(
      temp_diff ~ 1,
      data = dat,
      correlation = corAR1(form = ~ year),
      method = "REML"
    )
    
    coefs <- summary(model)$tTable
    
    tibble(
      mean_diff = coefs[1, "Value"],
      SE = coefs[1, "Std.Error"],
      t_value = coefs[1, "t-value"],
      p_value = coefs[1, "p-value"],
      AR1 = coef(model$modelStruct$corStruct, unconstrained = FALSE)
    )
  }) %>%
  ungroup() %>%
  mutate(
    p_holm = p.adjust(p_value, method = "holm"),
    p_bonferroni = p.adjust(p_value, method = "bonferroni")
  )

gls_results



# .............


library(tidyverse)
library(nlme)

equivalence_margin <- 0.5  # °C; replace with a scientifically justified value

equiv_results <- avg_temp %>%
  ungroup() %>%
  group_by(pair_id) %>%
  group_modify(~ {
    
    dat <- .x %>%
      filter(is.finite(temp_diff)) %>%
      arrange(year)
    
    model <- gls(
      temp_diff ~ 1,
      data = dat,
      correlation = corAR1(form = ~ year),
      method = "REML"
    )
    
    est <- unname(coef(model)[1])
    se  <- sqrt(vcov(model)[1, 1])
    df  <- df.residual(model)
    
    # 90% confidence interval for the TOST procedure
    crit <- qt(0.95, df = df)
    lower <- est - crit * se
    upper <- est + crit * se
    
    tibble(
      mean_diff = est,
      CI90_lower = lower,
      CI90_upper = upper,
      equivalent = lower > -equivalence_margin &
        upper < equivalence_margin
    )
  }) %>%
  ungroup()

equiv_results

# <####################


# model_full <- gls(temperature ~ stand_type * pair_id * year_c,
#   data = temp_long,
#   correlation = corAR1(form = ~ year | pair_id/stand_type),
#   method = "ML"
# )
# 
# summary(model_full)

navaa <- navaa %>% 
  dplyr::select(-temp_diff) %>% 
  pivot_longer(cols = Coldspot:Hotspot,
               names_to = "spot_status",
               values_to = "Tmean")

lm(Tmean ~ year * spot_status, data = navaa)
# ########################

library(nlme)

# Model without spot_status
m0 <- gls(
  Tmean ~ year,
  data = navaa,
  correlation = corAR1(form = ~ year | spot_status),
  method = "ML"
)

# Model including spot_status and its interaction with year
m1 <- gls(
  Tmean ~ year * spot_status,
  data = navaa,
  correlation = corAR1(form = ~ year | spot_status),
  method = "ML"
)

# Compare models
anova(m0, m1)