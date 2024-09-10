
# This is the analysis script for the following paper: 
# "Wind shapes the growth strategies of trees in a tropical forest". Jackson et al (2024). Ecology Letters
# Written by Toby Jackson (tobydjackson@gmail.com) in March 2024. Please contact me if you have questions.

# This creates the graphs and runs the models for the paper
# It uses the output file from step 3 "BCI_step3_output_dtrees_risk_with_exposure_17July24.csv". 
# This file is available on my github "https://github.com/TobyDJackson/WindAndTrees_BarroColoradoIsland/tree/main/summary_data/"

# This code was written with R 4.3.2 using magrittr 2.0.3; dplyr 1.1.3; stats 4.3.2; lubridate 1.9.3; 
# ggplot2 1.1.3; dplR 1.7.6; zoo 1.8-12; tidyverse 2.0.0; readxl 1.4.3; MetBrewer 0.2.0; patchwork 1.2.0; 
# Metrics 0.1.4; broom 1.0.6; terra 1.7-55; sf 1.0-14; curl 5.1.0; ggpubr 0.6.0

library(tidyverse); library(magrittr)
library(readxl); library(MetBrewer)
library(patchwork); library(ggpubr)
library(Metrics); library(broom)

local_path <- "C:/Users/av23907/Dropbox/9_Data/TreeMotion/BCI/"

# Import summary data #####

dt_analysis = readr::read_csv(paste0(local_path,"summary_data/BCI_step3_output_dtrees_risk_with_exposure_17July24.csv")) 

# Transform variables
# The main response variable is 'risk_at_37' which describes the risk of the tree snapping at 37 km/hr wind speed measured at the Lutz tower.
dt_analysis %<>% mutate(rel_risk = 100*(risk_at_37/max(risk_at_37)))
dt_analysis %<>% mutate(log_risk = log10(risk_at_37))
dt_analysis %<>% mutate(rel_log_risk = 100*(log_risk/max(log_risk)))
dt_analysis %<>% mutate(log_slenderness = log10(slenderness))


# Set factor levels for figures
dt_analysis$genus         %<>% factor(levels = rev(c("Jacaranda","Anacardium","Virola","Dypterix","Handroanthus" )))
dt_analysis$genus_wd      %<>% factor(levels = rev(c("Jacaranda (0.35)","Anacardium (0.41)","Virola (0.44)","Dypterix (0.85)","Handroanthus (0.92)" )))

dt_analysis$wood_density_class = "Low"
dt_analysis$wood_density_class[dt_analysis$wood_density>0.6] = "High"
dt_analysis$wood_density_class %<>% factor(levels = (c("High","Low")))


# Split the data by exposure calculation type. Circular = c50, triangular = t50. These were set in step3.
dt_analysis_c50 = dt_analysis  %>% dplyr::filter(buffer_type =="circular",buffer_width==50)
dt_analysis_t50 = dt_analysis  %>% dplyr::filter(buffer_type =="triangular",buffer_width==50)

# Summarize number of trees by genus
summary(as.factor(dt_analysis_t50$genus))

# Fig 3 - Risk vs height & slenderness ########

dt_analysis_t50$genus_wd  %<>% factor(levels = (c("Jacaranda (0.35)","Dypterix (0.85)","Anacardium (0.41)","Handroanthus (0.92)", "Virola (0.44)")))

g1 = ggplot(dt_analysis_t50 , aes(tree_height_m, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +  
  geom_point(size=0.5,alpha=1)+theme_bw()+
  ylab("Relative wind \nmortality risk (%)")+scale_y_log10()+
  xlab("Tree height (m)")

g2 = ggplot(dt_analysis_t50 , aes(tree_dbh_m, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +  
  geom_point(size=0.5,alpha=1)+theme_bw()+
  ylab("Relative wind \nmortality risk (%)")+scale_y_log10()+
  xlab("Trunk diameter (m)")

g3 = ggplot(dt_analysis_t50 , aes(slenderness, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +  
  geom_point(size=0.5,alpha=1)+theme_bw()+
  ylab("Relative wind \nmortality risk (%)")+scale_y_log10()+scale_x_log10()+
  xlab("Slenderness")

g4 = ggplot(dt_analysis_t50 , aes(tree_dbh_m, tree_height_m,color=genus_wd))+
  geom_smooth(formula = y ~ log(x),se=F,method="lm",linewidth=0.5)+
  geom_point(size=0.5,alpha=1)+theme_bw()+  
  ylab("Tree height (m)")+xlab("Trunk diameter (m)")

gc =  (g1+g2)/(g3+g4) +plot_layout(guides = "collect") & 
  scale_color_manual(values=met.brewer("Archambault", 7)[rev(c(5,2,6,1,7))])&
  guides(colour = guide_legend(override.aes = list(size=5),nrow=2))&
  theme(legend.position = "bottom",legend.title = element_blank(),  panel.grid.minor = element_blank()) &
  plot_annotation(tag_level="A")

gc

ggsave(plot = gc, filename = paste0(local_path,"figures/",
      "Fig3_wind_risk_vs_size_per_species.pdf"),dpi = 600, height = 6, width = 6)

ggsave(plot = gc, filename = paste0(local_path,"figures/",
                  "Fig3_wind_risk_vs_size_per_species.tiff"),height = 6, width = 6)


# Fig 4 - models classed by wood density ####


g1 = ggplot(data = dt_analysis_t50, aes(slenderness, rel_risk,color=wood_density_class))+
  geom_point(size=0.5,alpha=1)+
  geom_smooth(method="lm", linewidth=0.5)+
  theme_bw()+
  ylab("Relative wind \nmortality risk (%)")+scale_y_log10()+scale_x_log10()+
  xlab("Slenderness")

g2 = ggplot(data = dt_analysis_t50, aes(exposure_dsm_field, rel_risk,color=wood_density_class))+
  geom_vline(xintercept=0,color="black")+
  geom_point(size=0.5,alpha=1)+xlim(c(-15,30))+
  geom_smooth(method="lm", linewidth=0.5)+
  theme_bw()+
  ylab("Relative wind \nmortality risk (%)")+scale_y_log10()+
  xlab("Northerly wind exposure \n(m above canopy)")

g3 = ggplot(data = dt_analysis_c50, aes(exposure_dsm_field, rel_risk,color=wood_density_class))+
  geom_vline(xintercept=0,color="black")+
  geom_point(size=0.5,alpha=1)+xlim(c(-15,30))+
  geom_smooth(method="lm", linewidth=0.5)+
  theme_bw()+
  ylab("Relative wind \nmortality risk (%)")+scale_y_log10()+
  xlab("Overall wind exposure \n(m above canopy)")

g4 = ggplot(data = dt_analysis_t50, aes(exposure_dsm_field, slenderness, color=wood_density_class))+
  geom_vline(xintercept=0,color="black")+
  geom_point(size=0.5,alpha=1)+xlim(c(-15,30))+
  geom_smooth(method="lm", linewidth=0.5)+
  theme_bw()+
  xlab("Northerly wind exposure \n(m above canopy)")+scale_y_log10()+
  ylab("Slenderness")


gc = (g1+g2)/(g3+g4)+plot_layout(guides = "collect") &
  scale_color_manual(name = "Wood density", values=met.brewer("Archambault", 7)[(c(2,6))])&
  guides(colour = guide_legend(override.aes = list(size=5), reverse=T))&
  theme(legend.position = "bottom",  panel.grid.minor = element_blank()) &
  plot_annotation(tag_level="A")

gc

ggsave(plot = gc, filename = paste0(local_path,"figures/",
                          "Fig4_wind_risk_vs_slenderness_and_exposure.pdf"),dpi=600,height = 6, width = 6)


ggsave(plot = gc, filename = paste0(local_path,"figures/",
                      "Fig4_wind_risk_vs_slenderness_and_exposure.tiff"),dpi=600,height = 6, width = 6)

# Graphical abstract ###########

dt_analysis_t50$wood_density_class %<>% factor(levels = (c("Low","High")))


g1 = ggplot(data = dt_analysis_t50, aes(exposure_dsm_field, rel_risk,color=wood_density_class))+
  geom_point(size=0.5,alpha=1)+xlim(c(-15,30))+theme_classic() +
  #geom_smooth(method="lm", linewidth=0.5)+
  ylab("Relative wind \nmortality risk (%)")+
  scale_y_log10()+
  xlab("Northerly wind exposure \n(m above canopy)")

g2 = ggplot(data = dt_analysis_t50, aes(slenderness, rel_risk,color=wood_density_class))+
    geom_point(size=0.5,alpha=1)+theme_classic() +
  #geom_smooth(method="lm", linewidth=0.5)+
  ylab("")+scale_y_log10()+scale_x_log10()+
  theme(axis.text.y = element_blank(), axis.ticks.y =element_blank() )+
  xlab("Tree height / diameter")



gc = (g1+g2)+plot_layout(guides = "collect") & 
  scale_color_manual(name = "Wood \ndensity", values=met.brewer("Archambault", 7)[(c(6,2))])&
  guides(colour = guide_legend(override.aes = list(size=5), reverse=F))&
  theme(legend.position = "right",  panel.grid.minor = element_blank(), panel.grid.major = element_blank())

gc

ggsave(plot = gc, filename = paste0(local_path,"figures/",
                                    "Graphical_abstract.png"),dpi=600,height = 2.5, width = 6)

# Table 1 - models #####


# Scale variables for modelling
dt_analysis_t50 %<>% mutate(log_slenderness_scaled = scale(log_slenderness), 
                       slenderness_scaled = scale(slenderness),
                       exposure_dsm_field_scaled = scale(exposure_dsm_field), 
                       wood_density_scaled = scale(wood_density))

# Make a little function to display model results quickly
model_summary = function(dt1,lm1){
  lm1_tidy = cbind(broom::tidy(lm1),stats::confint(lm1,level = 0.95))
  lm1_tidy$rmse = Metrics::rmse(actual=dt1$rel_log_risk, predicted = predict(lm1,newdata=dt1))
  a = cbind(lm1_tidy$term,round(lm1_tidy[,c(2,6,7,8)]))
  return(a)
}

# __ combined model #####
lm_all = lm(rel_log_risk ~ log_slenderness_scaled+exposure_dsm_field_scaled+wood_density_class, data=dt_analysis_t50); summary(lm_all)
(model_summary(dt1 = dt_analysis_t50,lm1 = lm_all))
#plot(lm_all)
car::vif(lm_all)
clipr::write_clip((model_summary(dt_analysis_c50,lm_all)))

# __ low wood density model #####
dt_low  = dt_analysis_t50 %>% dplyr::filter(wood_density<0.6)
lm_low =  lm(rel_log_risk ~ log_slenderness_scaled+exposure_dsm_field_scaled, data=dt_low);  summary(lm_low)
(model_summary(dt_low,lm_low))

# __ high wood density model #####
dt_high = dt_analysis_t50 %>% dplyr::filter(wood_density>0.6)
lm_high = lm(rel_log_risk ~ log_slenderness_scaled+exposure_dsm_field_scaled, data=dt_high); summary(lm_high)
(model_summary(dt_high,lm_high))



# Supplementary S4 ####


dt_analysis_t50$genus_wd  %<>% factor(levels = (c("Jacaranda (0.35)","Dypterix (0.85)","Anacardium (0.41)","Handroanthus (0.92)", "Virola (0.44)")))


g1 = ggplot(dt_analysis_t50 , aes(tree_height_m, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +  geom_point(size=0.1)+stat_cor(label.y.npc="bottom") +
  theme_bw()+
  ylab("Relative wind \nmortality risk (%)")+scale_y_log10()+
  xlab("Tree height (m)")

g2 = ggplot(dt_analysis_t50 , aes(tree_dbh_m, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +  geom_point(size=0.1)+stat_cor(label.y.npc="bottom") +
  theme_bw()+
  ylab("")+scale_y_log10()+
  xlab("Trunk diameter (m)")

g3 = ggplot(dt_analysis_t50 , aes(slenderness, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +  geom_point(size=0.1)+stat_cor(label.y.npc="bottom") +
  theme_bw()+
  ylab("")+scale_y_log10()+scale_x_log10()+
  xlab("Slenderness")


gc =  (g1+g2+g3) +plot_layout(guides = "collect") & 
  scale_color_manual(values=met.brewer("Archambault", 7)[rev(c(5,2,6,1,7))])&
  guides(colour = guide_legend(override.aes = list(size=5),nrow=2))&
  theme(legend.position = "bottom",legend.title = element_blank(),  panel.grid.minor = element_blank()) &
  plot_annotation(tag_level="A")

gc


ggsave(plot = gc, filename = paste0(local_path,"figures/",
                                    "S4_risk_vs_tree_height_diameter_slenderness.png"),dpi=600,height = 5, width = 8)


# Supplementary S5 -  Slenderness vs H & D ########


dt_analysis_t50$genus_wd  %<>% factor(levels = (c("Jacaranda (0.35)","Dypterix (0.85)","Anacardium (0.41)","Handroanthus (0.92)", "Virola (0.44)")))

g1 = ggplot(dt_analysis_t50 , aes(tree_height_m, slenderness,color=genus_wd))+
  geom_smooth(se=F,method="lm",,linewidth=0.5) +
  geom_point(size=0.5,alpha=1)+theme_bw()+
  ylab("Slenderness")+scale_y_log10()+
  xlab("Tree height (m)")

g2 = ggplot(dt_analysis_t50 , aes(tree_dbh_m, slenderness,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +
  geom_point(size=0.5,alpha=1)+theme_bw()+
  ylab("Slenderness")+scale_y_log10()+
  xlab("Trunk diameter (m)")


gc =  (g1+g2)+plot_layout(guides = "collect") & 
  scale_color_manual(values=met.brewer("Archambault", 7)[rev(c(5,2,6,1,7))])&
  guides(colour = guide_legend(override.aes = list(size=5),nrow=2))&
  theme(legend.position = "bottom",legend.title = element_blank(),  panel.grid.minor = element_blank()) &
  plot_annotation(tag_level="A")

gc

ggsave(plot = gc, filename = paste0(local_path,"figures/",
                                    "S5_slenderness_vs_size.png"),height = 4, width = 6)

