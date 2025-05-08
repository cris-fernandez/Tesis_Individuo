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
                         header = T, sep = ",") %>% select(-X)

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
                                ifelse(clean_target$mean_def_obs < 30,
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

clean_target <- clean_target %>% filter(!sp_id == "Pinpine")

clean_target$sp_id <- fct_relevel(clean_target$sp_id, "Abialba", "Pinsylv")

## 4.1.- Def ~ height ####

clean_target2 <- clean_target %>%
  filter(!is.na(height)) %>% 
  group_by(sp_id, spot_status) %>%
  mutate(def_cat = cut(mean_def_obs,
                       breaks = quantile(mean_def_obs,
                                         probs = c(0, 1/3, 2/3, 1), 
                                         na.rm = T),
                       include.lowest = T,
                       labels = c("1st T", "2nd T", "3rd T"))) %>%
  ungroup() %>% 
  filter(!is.na(mean_def_obs))

def_height <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = height, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "A") +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  ylab("Tree height (m)") +
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.2.- Def ~ dbh ####

def_dbh <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = dbh, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "B") +
  ylab("Tree d.b.h. (cm)") +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.3.- Def ~ Hegyi ####

def_hegyi <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = hegyi_index, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "C") +
  ylab("Hegyi index") +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  ylim(0, 75) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.4.- Def ~ C ####

def_c <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = percent_c, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "D") +
  ylab(expression(paste("Leaf C content (%)"))) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.5.- Def ~ N ####

def_n <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = percent_n, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "E") +
  ylab(expression(paste("Leaf N content (%)"))) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.6.- Def ~ dC13 ####

def_d13c <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = d13c, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "F") +
  ylab(bquote("Leaves δ"~C^13~"(‰)")) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.7.- Def ~ dN15 ####

def_d15n <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = d15n, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "G") +
  ylab(bquote("Leaves δ"~N^15~"(‰)")) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.8.- Def ~ dO18 ####

def_d18o <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = d18o, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "H") +
  ylab(bquote("Leaves δ"~O^18~"(‰)")) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.9.- Def ~ water content ####

def_wc <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = wc_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "I") +
  ylab(expression(paste("Leaf water content (%)"))) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.10.- Def ~ total chl ####

def_chl_fw <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = total_chl_fw_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "J") +
  ylab(expression(paste("Leaf chlorophyll content (μg g"^"-1", ")"))) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.11.- Def ~ carotenoids ####

def_xc_fw <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = xc_fw_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "K") +
  ylab(expression(paste("Leaf carotenoids content (μg g"^"-1", ")"))) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() + 
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.12.- Def ~ chla/b ####

def_chl_ab <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = chla_chlb_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "L") +
  ylab(expression(paste("Chlorophyll a/b ratio"))) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "none",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.13.- Def ~ chl/xc ####

def_chl_xc <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = chl_xc_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "M") +
  ylab(expression(paste("Chlorophylls/carotenoids ratio"))) +
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.14.- Def ~ SLA ####

def_sla <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = sla_22, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "N") +
  ylab(expression(paste("Tree average SLA (cm² g"^"-1", ")"))) + 
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.15.- Def ~ BAI05 ####

def_bai05 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = mean_05, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "O") +
  ylab(expression(paste("Tree average growth 05 (mm² year"^"-1", ")"))) + 
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

## 4.16.- Def ~ BAI10 ####

def_bai10 <- ggplot(clean_target2) + 
  geom_boxplot(aes(x = def_cat, y = mean_10, fill = sp_id, alpha = spot_status)) + 
  scale_fill_manual(breaks = c("Abialba", "Pinsylv"),
                    values = c("Abialba" = "#746fb2",
                               "Pinsylv" = "#1b9e77"),
                    labels = c("A. alba",
                               "P. sylvestris"),
                    name = "") +
  scale_alpha_manual(breaks = c("hotspot", "coldspot"),
                     values = c("hotspot" = 0.5,
                                "coldspot" = 1),
                     name = "") + 
  guides(alpha = "none") +
  labs(tag = "P") +
  ylab(expression(paste("Tree average growth 10 (mm² year"^"-1", ")"))) + 
  xlab(expression(paste("Tree average defoliation (%)"))) + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 9, vjust = - .85),
        axis.title.x = element_text(size = 15, vjust = 1.15),
        legend.text = element_text(size = 8),
        plot.tag = element_text(size = 22)) 

# 5.- Leaf traits plotting ####

tiff("04_figures/04_02_leaf_binned_defoliation.tiff", units = "mm", width = 450, height = 400,
     res = 800, compression = "lzw")
def_height + def_dbh + def_hegyi + def_c + 
  def_n + def_d13c + def_d15n + def_d18o +  
  def_wc + def_chl_fw + def_xc_fw + def_chl_ab +  
  def_chl_xc + def_sla + def_bai05 + def_bai10 +
  plot_layout(guides = 'collect', ncol = 4)
dev.off()