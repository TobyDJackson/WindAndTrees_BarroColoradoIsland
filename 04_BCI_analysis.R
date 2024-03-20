# This script makes the figures and runs the models  for the Panama wind-strain paper
# Written by Toby Jackson (tobydjackson@gmail.com) in March 2024.

library(tidyverse); library(magrittr)
library(readxl); library(MetBrewer)
library(patchwork); library(ggpmisc)
library(ggpubr)

local_path <- "C:/Users/av23907/Dropbox/9_Data/TreeMotion/BCI/"

# Import data #####
dtrees = readr::read_csv(paste0(local_path,"dtrees_with_exposure_240320.csv")) %>% dplyr::filter(is.na(wood_density)==0)

hist(dtrees$snapping_risk)
dtrees %<>% mutate(rel_risk = 100*(risk_at_37/max(risk_at_37)))
dtrees %<>% mutate(log_risk = log10(risk_at_37))
dtrees %<>% mutate(rel_log_risk = 100*(log_risk/max(log_risk)))

dtrees %<>% mutate(log_slenderness = log10(slenderness))
hist(dtrees$rel_risk)

dtrees$wood_density_class = "Low"
dtrees$wood_density_class[dtrees$wood_density>0.6] = "High"

dtrees$genus         %<>% factor(levels = rev(c("Jacaranda","Anacardium","Virola","Dypterix","Handroanthus" )))
dtrees$genus_wd      %<>% factor(levels = rev(c("Jacaranda (0.35)","Anacardium (0.41)","Virola (0.44)","Dypterix (0.85)","Handroanthus (0.92)" )))
dtrees_c50 = dtrees  %>% dplyr::filter(buffer_type =="circular",buffer_width==50)
dtrees_t50 = dtrees  %>% dplyr::filter(buffer_type =="triangular",buffer_width==50)

#dtrees_t50 %>% group_by(genus) %>% summary(tree_height_m)
summary(as.factor(dtrees_t50$genus))


# Fig 3 - Risk vs height & slenderness ########


dtrees_t50$genus_wd  %<>% factor(levels = (c("Jacaranda (0.35)","Dypterix (0.85)","Anacardium (0.41)","Handroanthus (0.92)", "Virola (0.44)")))

g1 = ggplot(dtrees_t50 , aes(tree_height_m, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",,linewidth=0.5) +  stat_cor() +
  geom_point(size=0.5,alpha=1)+theme_bw()+
  ylab("Wind mortality risk")+scale_y_log10()+
  xlab("Tree height (m)")

g2 = ggplot(dtrees_t50 , aes(tree_dbh_m, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +  stat_cor() +
  geom_point(size=0.5,alpha=1)+theme_bw()+
  ylab("Wind mortality risk")+scale_y_log10()+
  xlab("Trunk diameter (m)")

g3 = ggplot(dtrees_t50 , aes(slenderness, rel_risk,color=genus_wd))+
  geom_smooth(se=F,method="lm",linewidth=0.5) +  stat_cor() +
  geom_point(size=0.5,alpha=1)+theme_bw()+
  ylab("Wind mortality risk")+scale_y_log10()+scale_x_log10()+
  xlab("Slenderness")

g4 = ggplot(dtrees_t50 , aes(tree_dbh_m, tree_height_m,color=genus_wd))+
  geom_smooth(formula = y ~ log(x),se=F,method="lm",linewidth=0.5)+
  geom_point(size=0.5,alpha=1)+theme_bw()+  
  ylab("Tree height (m)")+xlab("Trunk diameter (m)")

gc =  (g1+g2)/(g3+g4) +plot_layout(guides = "collect") & 
  scale_color_manual(values=met.brewer("Archambault", 7)[rev(c(5,2,6,1,7))])&
  guides(colour = guide_legend(override.aes = list(size=5),nrow=2))&
  theme(legend.position = "bottom",legend.title = element_blank(),  panel.grid.minor = element_blank()) &
  plot_annotation(tag_level="A")

gc

ggsave(plot = gc, filename = paste0(dropbox,"Rscripts/BioMechR/Biomech_figures/",
                                    "Fig3_risk_vs_size_supplementary.png"),height = 8, width = 6)



# Fig 4 - models classed by wood density ####

g1 = ggplot(data = dtrees_t50, aes(slenderness, rel_risk,color=wood_density_class))+
  geom_point(size=0.5,alpha=1)+
  geom_smooth(method="lm", linewidth=0.5)+
  theme_bw()+
  ylab("Wind mortality risk")+scale_y_log10()+scale_x_log10()+
  xlab("Slenderness")

g2 = ggplot(data = dtrees_t50, aes(exposure_dsm_field, rel_risk,color=wood_density_class))+
  geom_vline(xintercept=0,color="black")+
  geom_point(size=0.5,alpha=1)+xlim(c(-15,30))+
  geom_smooth(method="lm", linewidth=0.5)+
  theme_bw()+
  ylab("Wind mortality risk")+scale_y_log10()+
  xlab("Northerly wind exposure \n(m above canopy)")

g3 = ggplot(data = dtrees_c50, aes(exposure_dsm_field, rel_risk,color=wood_density_class))+
  geom_vline(xintercept=0,color="black")+
  geom_point(size=0.5,alpha=1)+xlim(c(-15,30))+
  geom_smooth(method="lm", linewidth=0.5)+
  theme_bw()+
  ylab("Wind mortality risk")+scale_y_log10()+
  xlab("Symmetric wind exposure \n(m above canopy)")

g4 = ggplot(data = dtrees_t50, aes(exposure_dsm_field, slenderness, color=wood_density_class))+
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
ggsave(plot = gc, filename = paste0(dropbox,"Rscripts/BioMechR/Biomech_figures/",
                                    "Fig4.png"),height = 6, width = 6)

# Table 1 - models #####

dtrees_t50 %<>% mutate(log_slenderness_scaled = scale(log_slenderness), 
                       slenderness_scaled = scale(slenderness),
                       exposure_dsm_field_scaled = scale(exposure_dsm_field), 
                       wood_density_scaled = scale(wood_density))

#plot(lm1)
model_summary = function(dt1,lm1){
  lm1_tidy = cbind(broom::tidy(lm1),stats::confint(lm1,level = 0.95))
  lm1_tidy$rmse = Metrics::rmse(actual=dt1$rel_log_risk, predicted = predict(lm1,newdata=dt1))
  a = cbind(lm1_tidy$term,round(lm1_tidy[,c(2,6,7,8)],2))
  return(a)
}


lm_all = lm(rel_log_risk ~ log_slenderness_scaled+exposure_dsm_field_scaled+wood_density_class, data=dtrees_t50); summary(lm_all)
(model_summary(dt1 = dtrees_t50,lm1 = lm_all))
plot(lm_all)
car::vif(lm_all)
clipr::write_clip((model_summary(dtrees_c50,lm_all)))


dt_low  = dtrees_t50 %>% dplyr::filter(wood_density<0.6)
lm_low =  lm(rel_log_risk ~ log_slenderness_scaled+exposure_dsm_field_scaled, data=dt_low);  summary(lm_low)
(model_summary(dt_low,lm_low))

dt_high = dtrees_t50 %>% dplyr::filter(wood_density>0.6)
lm_high = lm(rel_log_risk ~ log_slenderness_scaled+exposure_dsm_field_scaled, data=dt_high); summary(lm_high)
(model_summary(dt_high,lm_high))

