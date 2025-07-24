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
  mutate(sp_id = factor(sp_id),
         vigor_id = fct_relevel(vigor_id, "cold_healthy", "hot_healthy", "hot_damaged"))

clean_target <- clean_target[!is.na(clean_target$sp_id), ]

# 5.- Coldspot quantiles ####

all_quantile <- quantile(clean_target[clean_target$spot_status == "coldspot", ]$mean_def_obs, 
                         0.9, na.rm = T)
abialba_quantile <- quantile(clean_target[clean_target$spot_status == "coldspot" & clean_target$sp_id == "Abialba", ]$mean_def_obs, 
                         0.9, na.rm = T)
pinsylv_quantile <- quantile(clean_target[clean_target$spot_status == "coldspot" & clean_target$sp_id == "Pinsylv", ]$mean_def_obs, 
                             0.9, na.rm = T)
pinpine_quantile <- quantile(clean_target[clean_target$spot_status == "coldspot" & clean_target$sp_id == "Pinpine", ]$mean_def_obs, 
                             0.9, na.rm = T)

mean(c(all_quantile, abialba_quantile, pinsylv_quantile, pinpine_quantile)) # 24.375

# 6.- Density plot all ####

density_all <- ggplot(data = clean_target) +
  geom_density(aes(x = mean_def_obs, fill = spot_status), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks=c("hotspot", "coldspot"),
                    values = c("hotspot" = "#D71515",
                               "coldspot" = "#2274A5"),
                    labels = c("Declining sites",
                               "Non-declining sites"),
                    name = "") +
  geom_vline(xintercept = all_quantile, color = "navy", linewidth = 3, 
             linetype = "dashed") + 
  geom_text(x = all_quantile, y = 0.05, label = "90%", size = 10) +
  xlab("") + 
  ylab("Density") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0, 0.085) + 
  ggtitle("All") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

# 7.- Density plot Abies alba ####

density_abialba <- ggplot(data = clean_target[clean_target$sp_id == "Abialba", ]) +
  geom_density(aes(x = mean_def_obs, fill = spot_status), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks=c("hotspot", "coldspot"),
                    values = c("hotspot" = "#D71515",
                               "coldspot" = "#2274A5"),
                    labels = c("Declining sites",
                               "Non-declining sites"),
                    name = "") +
  geom_vline(xintercept = abialba_quantile, color = "navy", linewidth = 3, 
             linetype = "dashed") + 
  xlab("") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ylim(0,100) + 
  ylim(0, 0.085) + 
  ggtitle("Abies alba") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30, face = "italic"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

# 8.- Density plot Pinus sylvestris ####

density_pinsylv <- ggplot(data = clean_target[clean_target$sp_id == "Pinsylv", ]) +
  geom_density(aes(x = mean_def_obs, fill = spot_status), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks=c("hotspot", "coldspot"),
                    values = c("hotspot" = "#D71515",
                               "coldspot" = "#2274A5"),
                    labels = c("Declining sites",
                               "Non-declining sites"),
                    name = "") +
  geom_vline(xintercept = pinsylv_quantile, color = "navy", linewidth = 3, 
             linetype = "dashed") + 
  xlab("Defoliation (%)") + 
  ylab("Density") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  ylim(0, 0.085) + 
  ggtitle("Pinus sylvestris") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30, face = "italic"),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

# 9.- Density plot Pinus pinea ####

density_pinpine <- ggplot(data = clean_target[clean_target$sp_id == "Pinpine", ]) +
  geom_density(aes(x = mean_def_obs, fill = spot_status), col = NA, alpha = 0.6) + 
  scale_fill_manual(breaks=c("hotspot", "coldspot"),
                    values = c("hotspot" = "#D71515",
                               "coldspot" = "#2274A5"),
                    labels = c("Declining sites",
                               "Non-declining sites"),
                    name = "") +
  geom_vline(xintercept = pinpine_quantile, color = "navy", linewidth = 3, 
             linetype = "dashed") + 
  xlab("Defoliation (%)") + 
  ylab("") + 
  scale_x_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) +
  ylim(0, 0.085) + 
  ggtitle("Pinus pinea") + 
  theme_classic() +
  theme(legend.position = "right",
        legend.key.size = unit(1, "cm"),
        plot.title = element_text(size = 30, face = "italic"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 25),
        legend.text = element_text(size = 25),
        plot.margin = margin(t = 0,  # Top margin
                             r = 0.25,  # Right margin
                             b = 1.2,  # Bottom margin
                             l = 0.1,  # Left margin
                             unit = "cm"))

# 10.- Plotting ####

tiff("04_figures/16_01_Defoliation_spot_status.tiff", units = "mm", width = 500, height = 300,
     res = 400, compression = "lzw")
density_all + density_abialba + guide_area() + density_pinsylv + density_pinpine + 
  plot_layout(guides = 'collect', ncol = 3)
dev.off()
# TreeInfo <- clean_target
# FGC <- TreeInfo
# FGC$defoliation <- apply(FGC[,c(12:13)],1,mean,na.rm=T)
# DDsite <- subset(FGC,spot_status%in%"hotspot")
# NDsite <- subset(FGC,spot_status%in%"coldspot")
# dim(DDsite)
# dim(NDsite)
# summary(DDsite$defoliation)
# summary(NDsite$defoliation)
# 
# par(mfrow=c(2,2),oma=c(2,2,0,0),mar=c(2,2,1,1))
# plot(density(DDsite$defoliation,na.rm =
#                TRUE),type="n",ylim=c(0,0.08),xlim=c(0,100),axes=F,xlab=" ",ylab="
# ",main=" ")
# polygon(density(DDsite$defoliation,na.rm =
#                   TRUE),col=rgb(139,62,47,maxColorValue = 255,alpha=150),border=NA)
# polygon(density(NDsite$defoliation,na.rm =
#                   TRUE),col=rgb(83,134,139,maxColorValue = 255,alpha=150),border=NA)
# segments(x0=quantile(NDsite$defoliation,0.9,na.rm=T),x1=quantile(NDsite$defoliation,0.9,na.rm=T),
#          y0=0,y1=0.06,lty=2,col="orange",lwd=2)
# text(x=quantile(NDsite$defoliation,0.9,na.rm=T),y=0.065,"90%",col=rgb(83,134,139,maxColorValue =
#                                                                         255))
# axis(1,at=seq(0,100,10),labels=T,tck=-0.03)
# axis(2,at=seq(0,0.08,0.02),labels=T,tck=-0.03,las=2)
# title("a) All species",adj=0,font.main=1)
# text(y=0.06,x=50,adj=0,lab=paste("trees in hot-spot N =
# ",nrow(DDsite),sep=" ") ,col=rgb(139,62,47,maxColorValue = 255))
# text(y=0.05,x=50,adj=0,lab=paste("trees in coldspot N =
# ",nrow(NDsite),sep=" ") ,col=rgb(83,134,139,maxColorValue = 255))
# mtext("Density",2,2.8)
# #
# plot(density(subset(DDsite,sp_id%in%"Abialba")$defoliation,na.rm =
#                TRUE),type="n",ylim=c(0,0.08),xlim=c(0,100),axes=F,xlab=" ",ylab="
# ",main=" ")
# polygon(density(subset(DDsite,sp_id%in%"Abialba")$defoliation,na.rm =
#                   TRUE),col=rgb(139,62,47,maxColorValue = 255,alpha=150),border=NA)
# polygon(density(subset(NDsite,sp_id%in%"Abialba")$defoliation,na.rm =
#                   TRUE),col=rgb(83,134,139,maxColorValue = 255,alpha=150),border=NA)
# segments(x0=quantile(subset(NDsite,sp_id%in%"Abialba")$defoliation,0.9,na.rm=T),x1=quantile(subset(NDsite,sp_id%in%"Abialba")$defoliation,0.9,na.rm=T),
#          y0=0,y1=0.06,lty=2,col="orange",lwd=2)
# text(x=quantile(subset(NDsite,sp_id%in%"Abialba")$defoliation,0.9,na.rm=T),y=0.065,"90%",col=rgb(83,134,139,maxColorValue =
#                                                                                                     255))
# axis(1,at=seq(0,100,10),labels=T,tck=-0.03)
# axis(2,at=seq(0,0.08,0.02),labels=T,tck=-0.03,las=2)
# title("b) A. alba",adj=0,font.main=1)
# #
# plot(density(subset(DDsite,sp_id%in%"Pinsylv")$defoliation,na.rm =
#                TRUE),type="n",ylim=c(0,0.08),xlim=c(0,100),axes=F,xlab=" ",ylab="
# ",main=" ")
# polygon(density(subset(DDsite,sp_id%in%"Pinsylv")$defoliation,na.rm =
#                   TRUE),col=rgb(139,62,47,maxColorValue = 255,alpha=150),border=NA)
# polygon(density(subset(NDsite,sp_id%in%"Pinsylv")$defoliation,na.rm =
#                   TRUE),col=rgb(83,134,139,maxColorValue = 255,alpha=150),border=NA)
# segments(x0=quantile(subset(NDsite,sp_id%in%"Pinsylv")$defoliation,0.9,na.rm=T),x1=quantile(subset(NDsite,sp_id%in%"Pinsylv")$defoliation,0.9,na.rm=T),
#          y0=0,y1=0.06,lty=2,col="orange",lwd=2)
# text(x=quantile(subset(NDsite,sp_id%in%"Pinsylv")$defoliation,0.9,na.rm=T),y=0.065,"90%",col=rgb(83,134,139,maxColorValue =
#                                                                                                     255))
# axis(1,at=seq(0,100,10),labels=T,tck=-0.03)
# axis(2,at=seq(0,0.08,0.02),labels=T,tck=-0.03,las=2)
# title("c) P. sylvestris",adj=0,font.main=1)
# mtext("Defoliation (%)",1,2.5)
# mtext("Density",2,2.8)
# #
# plot(density(subset(DDsite,sp_id%in%"Pinpine")$defoliation,na.rm =
#                TRUE),type="n",ylim=c(0,0.08),xlim=c(0,100),axes=F,xlab=" ",ylab="
# ",main=" ")
# polygon(density(subset(DDsite,sp_id%in%"Pinpine")$defoliation,na.rm =
#                   TRUE),col=rgb(139,62,47,maxColorValue = 255,alpha=150),border=NA)
# polygon(density(subset(NDsite,sp_id%in%"Pinpine")$defoliation,na.rm =
#                   TRUE),col=rgb(83,134,139,maxColorValue = 255,alpha=150),border=NA)
# segments(x0=quantile(subset(NDsite,sp_id%in%"Pinpine")$defoliation,0.9,na.rm=T),x1=quantile(subset(NDsite,sp_id%in%"Pinpine")$defoliation,0.9,na.rm=T),
#          y0=0,y1=0.06,lty=2,col="orange",lwd=2)
# text(x=quantile(subset(NDsite,sp_id%in%"Pinpine")$defoliation,0.9,na.rm=T),y=0.065,"90%",col=rgb(83,134,139,maxColorValue =
#                                                                                                     255))
# axis(1,at=seq(0,100,10),labels=T,tck=-0.03)
# axis(2,at=seq(0,0.08,0.02),labels=T,tck=-0.03,las=2)
# title("d) P. pinea",adj=0,font.main=1)
# mtext("Defoliation (%)",1,2.5)
