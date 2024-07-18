
# This is step 3 of the processing for the following paper: 
# "Wind shapes the growth strategies of trees in a tropical forest". Jackson et al (2024). Ecology Letters
# Written by Toby Jackson (tobydjackson@gmail.com) in March 2024. Please contact me if you have questions.

# This script calculates the wind exposure for each tree, using the height of the surrounding canopy from LiDAR data

# The LiDAR data are available here: https://smithsonian.figshare.com/articles/dataset/Digital_Surface_Models_Whole_Island_2018-2023/24021381
# All other summary data are available on my github "https://github.com/TobyDJackson/WindAndTrees_BarroColoradoIsland/tree/main/summary_data/"



library(tidyverse)
library(magrittr)
library(terra)
library(sf)
library(curl)

local_path <- "C:/Users/av23907/Dropbox/9_Data/TreeMotion/BCI/"


# Load data ######

# Load tree risk data from step 2
dtrees_risk = readr::read_csv(paste0(local_path,"summary_data/BCI_step2_output_dtrees_risk_17July24.csv"))

# Load crown polygons 
# Note - automatically downloading corrupted this file, so please download it manually and place in local 'summary_data' folder
crowns = sf::st_read(paste0(local_path,"summary_data/BCI_wind_project_crowns_2022.gpkg"))

# Load LiDAR rasters #########
# DSM represents the canopy surface. DTM represents the terrain. The difference is canopy height (CHM)
dsm = terra::rast(paste0(local_path,"summary_data/DSM_2023.tif"))
dtm = terra::rast(paste0(local_path,"summary_data/LidarDEM_BCI.tif"))

# Crop and resample to same grid
dsm %<>% terra::crop(dtm)
dtm %<>% terra::crop(dsm)
dtm %<>% terra::resample(dsm)

dsm_dtm = terra::rast(list(dsm,dtm))
names(dsm_dtm) = c("dsm", "dtm")


# Get crown area etc #####
crown_values = terra::extract(dsm_dtm,crowns, fun="mean",na.rm=TRUE) #%>% dplyr::select(ID, dsm, dtm)

# Set function to crop triangles #############
make_triangular_buffer_zone = function(this_crown,buffer_width){
  
  crown_in = sf::st_as_sf(this_crown)
  # Get centroid as numeric
  crown_centroid  = sf::st_centroid(crown_in)
  a = crown_centroid$geometry[[1]]
  
  # Create triangular polygon from centroid
  pol = sf::st_polygon(
    list(
      cbind(
        c(a[1],a[1]-buffer_width/2,a[1]+buffer_width/2,a[1]), 
        c(a[2],a[2]+buffer_width,a[2]+buffer_width,a[2]))
    )
  )
  
  # Convert to terra and extract
  pol_terra = terra::vect(pol)
  return(pol_terra)
}


# Loop over each crown and buffer + calculate mean height of surroundings ######
temp=list(); c = 1
buffer_width = 50
for (i in seq(1,nrow(crowns))){
  print(paste(i, ", ",buffer_width))
  this_crown = crowns[i]
  
  # Circular buffer
  this_crown_circular_buf = this_crown %>% terra::buffer(width=buffer_width)
  # Extract mean height within buffer
  this_crown_circular_mean =dsm_dtm %>% terra::extract(this_crown_circular_buf,fun="mean",na.rm=T)
  this_crown_circular_mean$buffer_type = "circular"
  
  #Triangular buffer
  this_crown_triangular_buf = make_triangular_buffer_zone(this_crown,buffer_width)
  this_crown_triangular_mean = terra::extract(dsm_dtm,this_crown_triangular_buf,fun="mean",na.rm=T) 
  this_crown_triangular_mean$buffer_type = "triangular"
  
  # Combine buffer values
  this_crown_means = dplyr::bind_rows(this_crown_circular_mean, this_crown_triangular_mean)
  this_crown_means %<>% dplyr::mutate(ID=i, buffer_width = buffer_width) %>%
    dplyr::rename(surrounding_dsm = dsm, surrounding_dtm = dtm)
  this_crown_means$tree_id = this_crown$tree_id
  temp[[c]]=this_crown_means
  c = c+1
}

surrounding_canopy = dplyr::bind_rows(temp)

# Combine tree canopy and surrounding canopy ####
dt_canopy = dplyr::right_join(crown_values,surrounding_canopy, by = "ID")


# Fill trees with missing crowns #####
# 8 trees don't have mapped crowns, so I have filled the data using the average of the nearby trees (those with same sink logger)
missing_trees = c("s144", "s148", "s242", "s264", "s270", "s296", "s298", "s322")
dtrees_missing = dtrees_risk %>% dplyr::filter(tree_id %in% missing_trees)
dtrees_risk %<>% dplyr::filter(!tree_id %in% missing_trees)

# Join surroundings onto tree data
dtrees_risk_with_canopy = dtrees_risk %>% dplyr::left_join(dt_canopy, by = "tree_id") #%>% tidyr::drop_na()

#ggplot(dtrees_risk_with_canopy, aes(sink_logger, surrounding_dsm))+geom_point()

# Create flag to show which surroundings were added
dtrees_risk_with_canopy$surroundings_added = 0
dtrees_missing$surroundings_added = 1

# Calculate mean surroundings per sink, for each buffer
dtrees_risk_with_canopy %<>% dplyr::group_by(sink_logger, buffer_width, buffer_type) %>% 
  dplyr::mutate(dtm_sink_mean = mean(dtm,na.rm=T),
                surrounding_dtm_sink_mean = mean(surrounding_dtm,na.rm=T), 
                surrounding_dsm_sink_mean = mean(surrounding_dsm,na.rm=T))

# Get distinct values of surroundings
dtrees_risk_with_canopy_short = dtrees_risk_with_canopy %>% dplyr::select(sink_logger, buffer_type, buffer_width, 
                                dtm_sink_mean, surrounding_dtm_sink_mean, surrounding_dsm_sink_mean) %>% dplyr::distinct()

# Join mean surroundings onto missing trees and rename
dtrees_filled = left_join(dtrees_missing,dtrees_risk_with_canopy_short,by="sink_logger",relationship = "many-to-many")
dtrees_filled %<>% dplyr::mutate(surrounding_dtm = surrounding_dtm_sink_mean, 
                                 surrounding_dsm = surrounding_dsm_sink_mean, 
                                 dtm = dtm_sink_mean)

# Join missing and original dtrees back together
dtrees_risk_with_canopy_filled = dplyr::bind_rows(dtrees_risk_with_canopy, dtrees_filled)


# Calculate wind exposure  #####
dtrees_risk_with_canopy_filled %<>% dplyr::mutate(tree_height_m_plus_dtm = tree_height_m+dtm,
                                             exposure_dsm_field = tree_height_m_plus_dtm -surrounding_dsm) 

# Save exposure
readr::write_csv(dtrees_risk_with_canopy_filled,paste0(local_path,"summary_data/BCI_step3_output_dtrees_risk_with_exposure_17July24.csv"))

