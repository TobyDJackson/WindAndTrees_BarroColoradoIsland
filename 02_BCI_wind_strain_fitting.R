
# This fits wind-strain curves for trees in Barro Colorado Island, Panama
# Written by Toby Jackson (tobydjackson@gmail.com) in March 2024.



library(tidyverse)
library(magrittr)
library(MetBrewer)
library(lubridate)

# Change this path 
local_path <- "C:/Users/av23907/Dropbox/9_Data/TreeMotion/BCI/"


# Strain data #########
# Hourly bending strain summary data (available here:  https://doi.org/10.5285/f03806fa-3596-4119-90c5-70254f39cfc0)
bci_1hr_df <- read_csv(paste0(local_path,"BCI_data_deposit/BCI_strain_hourly_maxima.csv"))  %>% distinct()
bci_1hr_df %<>% mutate(datetime = lubridate::as_datetime(datetime,tz = "America/Panama"))


meta_data <- read_csv(paste0(local_path, "BCI_data_deposit/BCI_strain_metadata.csv")) %>% 
  dplyr::select(!sensor_id) %>% dplyr::distinct() %>%
  dplyr::mutate(tree_id = as.character(paste0("s",tree_id)))

bci_1hr_df %<>% left_join(meta_data, by="tree_id") %>% tidyr::drop_na()

wood_properties <- readxl::read_excel(paste0(local_path,"WoodDensity.xlsx"))
bci_1hr_df %<>% dplyr::left_join(wood_properties, by = "species") 

number_of_measurements = nrow(bci_1hr_df)*2*60*60*4/1e9

min(bci_1hr_df$datetime)

# Wind data #######

ws_lutz = read_csv(paste0(local_path,"wind_data/bci_elect_48m_ws/bci_lutz48m_wsmx_elect.csv"))
ws_lutz %<>% mutate(datetime = lubridate::as_datetime(datetime,format="%m/%d/%Y %H:%M:%S",tz = "UTC"))
 
wd_lutz = read_csv(paste0(local_path,"wind_data/bci_elect_48m_wd/bci_lutz48m_wdvm_elect.csv"))
wd_lutz %<>% mutate(datetime = lubridate::as_datetime(datetime,format="%m/%d/%Y %H:%M:%S",tz = "UTC"))

wind_lutz = left_join(ws_lutz,wd_lutz,by="datetime")
wind_lutz %<>% dplyr::filter(datetime>min(bci_1hr_df$datetime),datetime<max(bci_1hr_df$datetime))

openair::windRose(mydata=wind_lutz,ws="wsmx",wd="wdvm")
wind_lutz2 = dplyr::filter(wind_lutz,wsmx>10)
wind_lutz3 = bind_rows(dplyr::filter(wind_lutz2,wdvm>310),
                       dplyr::filter(wind_lutz2,wdvm<30)) 

wind_lutz %<>% mutate(datetime = lubridate::ceiling_date(datetime,unit="hour"))
wind_lutz_hourly = wind_lutz %>% group_by(datetime) %>% summarize(wsmx = max(wsmx,na.rm=T))

bci_1hr_with_lutz = left_join(bci_1hr_df,wind_lutz_hourly,by="datetime")
ggplot(bci_1hr_with_lutz,aes(gust_speed_era5_ms,wsmx))+geom_point()




# Re-name variables to simplify loop.
df = bci_1hr_with_lutz %>% dplyr::filter(completeness > 0.5) %>%
  # renaming variables
  dplyr::rename(wind=wsmx, strain = bending_strain_max) %>%
  dplyr::mutate(wind_squared = wind^2, wind_logged = log(wind), 
                snapping_risk = 100*strain/breaking_strain,
                snapping_risk_logged = log(snapping_risk),
                strain_logged = log(strain), 
                slenderness = 1000*tree_height_m/tree_dbh_mm) %>%
  dplyr::select(tree_id,wind,wind_squared,wind_logged,wind_direction = wind_direction_era5,
                strain,strain_logged, snapping_risk, snapping_risk_logged,
                tree_height_m,species,tree_dbh_mm, slenderness,wood_density) %>%
  dplyr::filter(is.finite(wind_logged)) %>% dplyr::filter(is.finite(strain_logged)) %>% 
  tidyr::drop_na() %>% dplyr::arrange(desc(wood_density), slenderness) %>% 
  dplyr::filter(species != "Other")  %>% tidyr::drop_na() %>% dplyr::filter(strain<15e-5)
  


trees = unique(df$tree_id)


# Loop over trees ###
##
plots_out = list(); info_out = list()
for(i in 1:96){
  print(paste(i, nrow(trees)))
  this_tree <- trees[i]
  #this_tree = "s136"
  df_this_tree =  dplyr::filter(df, tree_id == this_tree)
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
  
  # Create plot
  if (1==1){
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
dtrees = dplyr::bind_rows(info_out)

# Add meta data again
dtrees = dplyr::right_join(meta_data,dtrees, by = "tree_id")

# Add wood density values
wood_properties <- readxl::read_excel(paste0(local_path,"/WoodDensity.xlsx"))
dtrees %<>% dplyr::left_join(wood_properties, by = "species") 
dtrees$genus = dtrees$species
dtrees$genus[dtrees$species=="Guyacan"] = "Handroanthus"
dtrees %<>% dplyr::mutate(genus_wd = paste0(genus, " (",wood_density,")"))

# Remove sensors not on tree trunks (just for fun during experiment!)
dtrees %<>% dplyr::filter(species !="Attalea", species != "Ceiba")
dtrees %<>% dplyr::mutate(tree_dbh_m = tree_dbh_mm/1000, 
                          slenderness = tree_height_m/tree_dbh_m)

# Add sink logger info - useful for estimating the location of trees not mapped in the field.
dt_sink = readxl::read_excel("C:/Users/av23907/Dropbox/3_BCI fieldwork/BCI_BioMech_field_setup.xlsx") 
names(dt_sink)
dt_sink %<>% dplyr::mutate(tree_id = paste0("s",BioMech_ID)) %>% 
  dplyr::select(tree_id, Sink_logger)

dtrees2 = dplyr::left_join(dtrees,dt_sink, by = "tree_id")
readr::write_csv(dtrees2, paste0(local_path,"dtrees_240320.csv"))



# Fitting figure for supplementary materials  ########
a = patchwork::wrap_plots(plots_out, nrow = 12,ncol=8)
b =  a + patchwork::plot_layout(guides = "collect") & theme(legend.position = "top",plot.margin=margin(0.5,0.5,0.5,0.5))
ggsave(filename=paste0(local_path, "big_plot_RISK_with_fits3.png"), plot=b, 
       height=10, width=7)

dtrees = dplyr::bind_rows(info_out) %>% dplyr::filter(fit_type == "squared through origin") 
max(dtrees$strain_at_37)

