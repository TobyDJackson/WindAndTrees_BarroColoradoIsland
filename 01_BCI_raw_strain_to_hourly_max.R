
# This script summarizes 4 Hz strain data, collected on tropical trees in Barro Colorado Island, to hourly maxima per tree for analysis
# Written by Toby Jackson (tobydjackson@gmail.com) in March 2024.

# The raw bending strain data are openly available here: https://doi.org/10.5285/f03806fa-3596-4119-90c5-70254f39cfc0
# The output of this script (maximum hourly bending strain) are also available at that link.

# ADJUST YOUR PATH
strain_data_folder_path = paste0("C:/Users/av23907/Dropbox/9_Data/TreeMotion/BCI/BCI_data_deposit/strain_all_trees_hourly/")
strain_files = list.files(strain_data_folder_path,pattern=paste0("strain_data*"),recursive = T,full.names = F,include.dirs = F)

# Filter by completeness
strain_completeness = as.numeric(stringr::str_sub(tools::file_path_sans_ext(strain_files),start=46))
strain_files = strain_files[strain_completeness>20]

# RUN THE FUNCTION
summarise_1hr_bending_strains <- function(strain_data_in){
  
  
  tree_summary_out = list(); 
  c = 1
  for (j in seq(3,217,by=2)){
    #print(j)
    
    (tree_id = colnames(strain_data_in[j]))
    
    # Filter data for each sensor
    dt1 = strain_data_in[,c(1,2,j)]
    colnames(dt1) = c("datetime","milliseconds","raw")
    dt1$sensor = "sensor1"
    
    dt2 = strain_data_in[,c(1,2,j+1)]
    colnames(dt2) = c("datetime","milliseconds","raw")
    dt2$sensor = "sensor2"
    
    # Check there is data in both signals
    num_values_dt1 = sum(!is.na(dt1$raw))
    num_values_dt2 = sum(!is.na(dt2$raw))
    if (num_values_dt1>100 && num_values_dt2>100){
      
      # Correct offset by subtracting linear prediction from raw signal
      dt1 %<>% dplyr::mutate( unit_seq = seq(1,14400)) 
      dt2 %<>% dplyr::mutate(unit_seq = seq(1,14400)) 
      lm1 = lm(raw ~ unit_seq,data=dt1)
      lm2 = lm(raw ~ unit_seq,data=dt2)
      
      dt1 %<>% dplyr::mutate(predicted_linear = predict(lm1,newdata = dt1), 
                             corrected_linear = raw-predicted_linear)
      dt2 %<>% dplyr::mutate(predicted_linear = predict(lm2,newdata = dt2), 
                             corrected_linear = raw-predicted_linear)
      
      # Find resultant strain (sensor) value 
      resultant_linear = sqrt(dt1$corrected_linear^2 + dt2$corrected_linear^2)
      
      
      # Correct offset by subtracting linear prediction from raw signal      # Create 1hr summary data 
      obs_expt <- 60*60*4 # number of data points there should be in 1hr at 4Hz
      my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=TRUE), NA) # deals with problem of default max function outputting -INF if there is no data
      
      j_summary = data.frame(tree_id,
                             bending_strain_max = max(resultant_linear,na.rm=T),
                             bending_strain_q99 = stats::quantile(resultant_linear,probs=0.99,na.rm=TRUE),
                             completeness = sum(!is.na(resultant_linear))/obs_expt)
      
      tree_summary_out[[c]]=j_summary
      
      c=c+1
    } # End of check for NAs
    
  } # End of loop over tree column pairs
  tree_summary_out2  = tree_summary_out %>% dplyr::bind_rows()
  return(tree_summary_out2)
  #print(paste(name_format_out, "summarised"))
} # End of function

# LOOP OVER FILES AND APPLY FUNCTION
temp_list=list()
for(t in seq(1,length(strain_files)))  {
  
  (this_file = strain_files[t])
  this_file_datetime_text =  substr(this_file, 13, 31)
  this_file_datetime = lubridate::as_datetime(this_file_datetime_text,tz = "America/Panama")

  # Load in this bending strain data
  strain_data_in = readr::read_csv(paste0(strain_data_folder_path,strain_files[t]))

  # Run the function to get hourly maxima
  temp = summarise_1hr_bending_strains(strain_data_in)
  temp$datetime = this_file_datetime
  
  # Save to list
  temp_list[[t]] = temp

  print(t)
}

# Collapse list into data frame
hourly_strain_data_out = dplyr::bind_rows(temp)

# SAVE OUTPUT SOMEWHERE SENSIBLE
#write.csv2(dt_deposit,paste0(temp_data_out,"BCI_bending_strain_hourly_maxima.csv"))