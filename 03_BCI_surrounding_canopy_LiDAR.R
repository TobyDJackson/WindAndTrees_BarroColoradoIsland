
# Aim: to calculate mean height of surrounding trees in BCI, so we can calculate wind exposure.

# Code written May 2023
# Updated November 2023 - I tested exact_extract but it wasn't any faster
# The crown delineations are available here: https://zenodo.org/records/7746643#.ZCQdE3bMJhE

library(tidyverse)
library(terra)
library(magrittr)
library(sf)
dropbox = "C:/Users/av23907/Dropbox/"
local_path <- "C:/Users/av23907/Dropbox/9_Data/TreeMotion/BCI/"

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


# Load crown polygons ####

crowns = terra::vect(paste0(local_path,"9_Data/ManualCrowns/BCI_manual_crowns_by_Juliet_and_Jakob/Crown_Delineation_Whole_Island_2022.shp"))
crowns %<>% terra::project("epsg:32617") %>% sf::st_as_sf()
crowns$tree_id = as.double(crowns$SensorID)
crowns$tree_id = paste0("s",crowns$SensorID)

# save final crowns with bending data #####
dcrowns_t50 = dplyr::right_join(crowns,dtrees_t50,by="tree_id")
#sf::st_write(dcrowns_t50, paste0(dropbox,"Rscripts/BioMechR/dcrowns_with_exposure_231129.geojson"))


# 
crowns_no_geom = crowns %>% sf::st_drop_geometry()

meta_data <- read_csv(paste0(dropbox, "9_Data/TreeMotion/BCI/BCI_data_deposit/BCI_strain_metadata.csv")) %>% 
  dplyr::select(!sensor_id) %>% dplyr::distinct()

#meta_data %<>% dplyr::filter(tree_id %in% crowns$tree_id)

crowns_meta = full_join(crowns, meta_data, by="tree_id")
names(crowns_meta)

head(crowns)
head(meta_data)
summary(as.factor(crowns$SensorID))
plot(crowns)

# Load 2023 rasters #########

dsm_2023 = terra::rast(paste0(dropbox,"9_Data/ALS/BCI_ALS/BCI_2023/06. DSM/ASCII/Mosaic/MDS_BARRO_COLORADO_MOSAICO_0.5.asc"))
dtm_2023 = terra::rast(paste0(dropbox,"9_Data/ALS/BCI_ALS/BCI_2023/04. DTM/ASCII/Mosaic/MDT_BARRO_COLORADO_MOSAICO_0.5.asc"))
chm_2023 = dsm_2023 - dtm_2023
terra::writeRaster(chm_2023, paste0(dropbox,"9_Data/ALS/BCI_ALS/BCI_2023/CHM_BARRO_COLORADO_MOSAICO_0.5.tif"))
dsm_dtm_2023 = terra::rast(list(dsm_2023,dtm_2023))
names(dsm_dtm_2023) = c("dsm", "dtm")


# Tree heights, crown area etc #####
crown_values_2023 = terra::extract(dsm_dtm_2023,crowns, fun="mean") #%>% dplyr::select(ID, dsm, dtm)

# Get sensor ID from the crowns shapefile
crowns_sf = sf::st_as_sf(crowns)
crowns_sf$crown_area_m2 = sf::st_area(crowns_sf)
crowns_sf %<>% sf::st_drop_geometry() %>% units::drop_units()
crowns_sf %<>% mutate(sensor_id = as.double(SensorID), 
                      ID = seq(1,nrow(crowns_sf)),
                      crown_radius_m = sqrt(crown_area_m2/3.14))
crowns_sf %<>% select(ID,sensor_id, crown_area_m2, crown_radius_m)

crowns_combined = dplyr::right_join(crowns_sf,crown_values_2023, by = "ID")


# Loop over each crown and buffer + calculate mean height of surroundings ######
temp=list(); c = 1
buffer_width = 50
for (i in seq(1,nrow(crowns))){
  print(paste(i, ", ",buffer_width))
  this_crown = crowns[i]
  tictoc::tic()
  
  # Circular buffer
  this_crown_circular_buf = this_crown %>% terra::buffer(width=buffer_width)
  # Extract mean height within buffer
  this_crown_circular_mean =dsm_dtm_2023 %>% terra::extract(this_crown_circular_buf,fun="mean")
  this_crown_circular_mean$buffer_type = "circular"
  
  #Triangular buffer
  this_crown_triangular_buf = make_triangular_buffer_zone(this_crown,buffer_width)
  this_crown_triangular_mean = terra::extract(dsm_dtm_2023,this_crown_triangular_buf,fun="mean") 
  this_crown_triangular_mean$buffer_type = "triangular"
  
  # Combine buffer values
  this_crown_means = dplyr::bind_rows(this_crown_circular_mean, this_crown_triangular_mean)
  this_crown_means %<>% dplyr::mutate(ID=i, buffer_width = buffer_width) %>%
    dplyr::rename(surrounding_dsm = dsm, surrounding_dtm = dtm)
  temp[[c]]=this_crown_means
  c = c+1
  tictoc::toc()
}

surrounding_canopy = dplyr::bind_rows(temp)


# Combined and save ####
dt_crowns_and_surroundings = dplyr::right_join(crowns_combined,surrounding_canopy, by = "ID")

ggplot(dt_crowns_and_surroundings, aes(dtm,surrounding_dtm,color=buffer_width))+geom_point()

readr::write_csv(dt_crowns_and_surroundings,paste0(dropbox,"1_Papers/Papers - in progress/P2_Panama_BioMech/",
                  "BCI_crowns_and_surroundings_from_2023_LiDAR_231121.csv"))


# Import height of surrounding canopy from LiDAR summary (03_BCI_surrounding_canopy_LiDAR). 
dt_surroundings = readr::read_csv(paste0(local_path,"BCI_crowns_and_surroundings_from_2023_LiDAR_231121.csv"))
dt_surroundings %<>% dplyr::mutate(tree_id = paste0("s",sensor_id)) 


# Fill missing surroundings data by sink logger #####
# 8 trees don't have mapped crowns, so I have filled the data.
dtrees = readr::read_csv(paste0(local_path,"dtrees_240320.csv"))
missing_trees = c("s144", "s148", "s242", "s264", "s270", "s296", "s298", "s322")
dtrees_missing = dtrees %>% dplyr::filter(tree_id %in% missing_trees)
dtrees %<>% dplyr::filter(!tree_id %in% missing_trees)

# Join surroundings onto tree data
dtrees_surroundings = dtrees %>% dplyr::left_join(dt_surroundings, by = "tree_id") #%>% tidyr::drop_na()

ggplot(dtrees_surroundings, aes(Sink_logger, surrounding_dsm))+geom_point()

# Create flag to show which surroundings were added
dtrees_surroundings$surroundings_added = 0
dtrees_missing$surroundings_added = 1

# Calculate mean surroundings per sink, for each buffer
dtrees_surroundings %<>% dplyr::group_by(Sink_logger, buffer_width, buffer_type) %>% 
  dplyr::mutate(dtm_sink_mean = mean(dtm,na.rm=T),
                surrounding_dtm_sink_mean = mean(surrounding_dtm,na.rm=T), 
                surrounding_dsm_sink_mean = mean(surrounding_dsm,na.rm=T))

# Get distinct values of surroundings
dtrees_surroundings_short = dtrees_surroundings %>% dplyr::select(Sink_logger, buffer_type, buffer_width, 
                                dtm_sink_mean, surrounding_dtm_sink_mean, surrounding_dsm_sink_mean) %>% dplyr::distinct()

# Join mean surroundings onto missing trees and rename
dtrees_filled = left_join(dtrees_missing,dtrees_surroundings_short,by="Sink_logger",relationship = "many-to-many")
dtrees_filled %<>% dplyr::mutate(surrounding_dtm = surrounding_dtm_sink_mean, 
                                 surrounding_dsm = surrounding_dsm_sink_mean, 
                                 dtm = dtm_sink_mean)

# Join missing and original dtrees back together
dtrees_joined = dplyr::bind_rows(dtrees_surroundings, dtrees_filled)


# Calculate wind exposure  #####
dtrees_exposure = dtrees_joined %>% dplyr::mutate(tree_height_m_plus_dtm = tree_height_m+dtm,
                                   chm = dsm-dtm, 
                                   surrounding_chm = surrounding_dsm- surrounding_dtm, 
                                   exposure_dsm_field = tree_height_m_plus_dtm -surrounding_dsm, 
                                   exposure_dsm_ratio = tree_height_m_plus_dtm /surrounding_dsm, 
                                   exposure_chm_field = tree_height_m - surrounding_chm) 

# Save exposure
readr::write_csv(dtrees_exposure,paste0(local_path,"dtrees_with_exposure_240324.csv"))

