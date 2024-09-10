
# This is step 2 of the processing for the following paper: 
# "Wind shapes the growth strategies of trees in a tropical forest". Jackson et al (2024). Ecology Letters
# Written by Toby Jackson (tobydjackson@gmail.com) in March 2024. Please contact me if you have questions.

# This script uses the hourly maxima from step 1 and fits wind-strain curves to get a single 'risk' estimate per tree. 

# The wind data are available from https://smithsonian.figshare.com/articles/dataset/Barro_Colorado_Island_Lutz_tower_48m_Wind_Speed/10042427
# All other summary data are available on my github "https://github.com/TobyDJackson/WindAndTrees_BarroColoradoIsland/tree/main/summary_data/"


# This code was written with R 4.3.2 using magrittr 2.0.3; dplyr 1.1.3; stats 4.3.2; lubridate 1.9.3; 
# ggplot2 1.1.3; dplR 1.7.6; zoo 1.8-12; tidyverse 2.0.0; readxl 1.4.3; MetBrewer 0.2.0; patchwork 1.2.0; 
# Metrics 0.1.4; broom 1.0.6; terra 1.7-55; sf 1.0-14; curl 5.1.0

library(tidyverse)
library(magrittr)
library(MetBrewer)
library(lubridate)

# Change this path 
local_path <- "C:/Users/av23907/Dropbox/9_Data/TreeMotion/BCI/"

# Strain data #########
df_hourly_max_strain <- read_csv2(paste0(local_path,"summary_data/BCI_step1_output_bending_strain_hourly_maxima_17July24.csv"))  %>% distinct()
df_hourly_max_strain %<>% mutate(datetime = lubridate::as_datetime(datetime,tz = "America/Panama"))
meta_data <- read_csv(paste0(local_path, "summary_data/BCI_tree_metadata.csv"))

df_hourly_max_strain %<>% left_join(meta_data, by="tree_id") %>% tidyr::drop_na()


# Wind data LUTZ #######

wind_lutz = read_csv(paste0(local_path,"summary_data/bci_lutz48m_wsmx_elect.csv"))
wind_lutz %<>% mutate(datetime = lubridate::as_datetime(datetime,format="%m/%d/%Y %H:%M:%S",tz = "UTC"))
wind_lutz %<>% dplyr::filter(datetime>min(df_hourly_max_strain$datetime),datetime<max(df_hourly_max_strain$datetime))

wind_lutz %<>% mutate(datetime = lubridate::ceiling_date(datetime,unit="hour"))
wind_lutz_hourly = wind_lutz %>% group_by(datetime) %>% summarize(wsmx = max(wsmx,na.rm=T))

df_hourly_max_strain_with_wind = left_join(df_hourly_max_strain,wind_lutz_hourly,by="datetime")
#ggplot(bci_1hr_with_lutz,aes(gust_speed_era5_ms,wsmx))+geom_point()

hist(df_hourly_max_strain_with_wind$wsmx)

# Re-name variables to simplify loop.
df_hourly_max_strain_with_wind %<>% dplyr::filter(completeness > 0.5) %>%
  # renaming variables
  dplyr::rename(wind=wsmx, strain = bending_strain_max_highpass) %>%
  dplyr::mutate(wind_squared = wind^2, wind_logged = log(wind), 
                snapping_risk = 100*strain/breaking_strain,
                snapping_risk_logged = log(snapping_risk),
                strain_logged = log(strain), 
                slenderness = 1000*tree_height_m/tree_dbh_mm) %>%
  dplyr::select(tree_id,wind,wind_squared,wind_logged,
                strain,strain_logged, snapping_risk, snapping_risk_logged,
                tree_height_m,species,tree_dbh_mm, slenderness,wood_density) %>%
  dplyr::filter(is.finite(wind_logged)) %>% dplyr::filter(is.finite(strain_logged)) %>% 
  tidyr::drop_na() %>% dplyr::arrange(desc(wood_density), slenderness) %>% 
  dplyr::filter(species != "Other")  %>% tidyr::drop_na() #%>% dplyr::filter(strain<15e-5)
  




# Loop over trees ###########
trees = unique(df_hourly_max_strain_with_wind$tree_id)
plots_out = list(); info_out = list()
for(i in 1:95){
  print(paste(i, nrow(trees)))
  this_tree <- trees[i]
  #this_tree = "s136"
  df_this_tree =  dplyr::filter(df_hourly_max_strain_with_wind, tree_id == this_tree)
  #df_this_tree %<>% dplyr::mutate()
  
  # Fit models
  lm_squared                = MASS::rlm(snapping_risk ~ wind_squared, data=df_this_tree)
  lm_squared_through_origin = MASS::rlm(snapping_risk ~ 0+wind_squared, data=df_this_tree, weights=wind_squared) # THIS IS THE BEST
  lm_squared_weighted       = MASS::rlm(snapping_risk ~ wind_squared, data=df_this_tree,weights = wind_squared)
  lm_logged                 = MASS::rlm(snapping_risk_logged ~ wind_logged, data=df_this_tree)
  
  info_squared_through_origin <- tibble(risk_at_37 = as.numeric(predict.lm(lm_squared_through_origin, data.frame(wind_squared=37^2))),  
                                        fit_type = "squared through origin")
  

  temp=info_squared_through_origin
  temp$tree_id = this_tree
  temp$num_hours_data = nrow(df_this_tree)
  info_out[[i]] = temp
  
  # Create figure for supplementary
  if (1==2){
    # Make this long format
    df_squared = df_this_tree
    df_squared$risk =  predict.lm(lm_squared)
    df_squared$fit_type = "squared"
    df_squared_through_origin = df_this_tree
    df_squared_through_origin$risk =  predict.lm(lm_squared_through_origin)
    df_squared_through_origin$fit_type = "squared through origin"
    df_squared_weighted = df_this_tree
    df_squared_weighted$risk =  predict.lm(lm_squared_weighted)
    df_squared_weighted$fit_type = "squared weighted"
    df_logged = df_this_tree
    df_logged$risk_logged =  predict.lm(lm_logged)
    df_logged$risk =  exp(df_logged$risk_logged)
    df_logged$fit_type = "power law"
    df_predictions = dplyr::bind_rows(df_squared,df_squared_through_origin,df_squared_weighted,df_logged)
    
    if (df_this_tree$species[1] == "Anacardium"){color_in = met.brewer("Klimt", 6)[c(4)]}
    if (df_this_tree$species[1] == "Dypterix")  {color_in = met.brewer("Klimt", 6)[c(2)]}
    if (df_this_tree$species[1] == "Guyacan")   {color_in = met.brewer("Klimt", 6)[c(3)]}
    if (df_this_tree$species[1] == "Jacaranda") {color_in = met.brewer("Klimt", 6)[c(6)]}
    if (df_this_tree$species[1] == "Virola")    {color_in = met.brewer("Klimt", 6)[c(1)]}
    
      
    p1=ggplot() +
      geom_point(data = df_this_tree,   aes(x=wind,y=snapping_risk),size = 0.01,alpha=0.5,color="grey") +
      geom_line( data = df_predictions, aes(x=wind,y=risk,color=fit_type)) +
      labs(x="", y="") +      theme_bw()+
      theme ( axis.text.y = element_blank () , axis.ticks.y = element_blank ())+
      theme ( axis.text.x = element_blank () , axis.ticks.x = element_blank ())+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

     plots_out[[i]] = p1
    
    
  }
}

# Save output data frame ####
dtrees_risk = dplyr::bind_rows(info_out)

# Add meta data again
dtrees_risk = dplyr::right_join(meta_data,dtrees_risk, by = "tree_id")


# Remove sensors not on tree trunks (just for fun during experiment!)
dtrees_risk %<>% dplyr::filter(species !="Attalea", species != "Ceiba")
dtrees_risk %<>% dplyr::mutate(tree_dbh_m = tree_dbh_mm/1000, 
                          slenderness = tree_height_m/tree_dbh_m)

readr::write_csv(dtrees_risk, paste0(local_path,"summary_data/BCI_step2_output_dtrees_risk_17July24.csv"))


# __ Fitting figure for supplementary materials  ########
a = patchwork::wrap_plots(plots_out, nrow = 12,ncol=8)
b =  a + patchwork::plot_layout(guides = "collect") & theme(legend.position = "top",plot.margin=margin(0.5,0.5,0.5,0.5))

ggsave(filename=paste0(local_path, "big_plot_RISK_with_fits_highpass.png"), plot=b, 
       height=10, width=7)