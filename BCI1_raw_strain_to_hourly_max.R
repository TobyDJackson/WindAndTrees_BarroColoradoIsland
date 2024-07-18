

# This is step 1 of the processing for the following paper: 
# "Wind shapes the growth strategies of trees in a tropical forest". Jackson et al (2024). Ecology Letters
# Written by Toby Jackson (tobydjackson@gmail.com) in March 2024. Please contact me if you have questions.

# This script summarizes 4 Hz strain data to hourly maxima per tree for further analysis (steps 2-4)

# The raw bending strain data are openly available here: https://doi.org/10.5285/f03806fa-3596-4119-90c5-70254f39cfc0
# You should manually download these files and put them in a local directory
# This code takes ~6 hours to run on a 2024 Macbook Pro
# The output file is saved on my github "BCI_step1_output_bending_strain_hourly_maxima_17July24.csv"

library(magrittr)
library(dplyr)
library(stats)
library(lubridate)
library(ggplot2)
library(dplR)
library(zoo)

local_path <- "C:/Users/av23907/Dropbox/9_Data/TreeMotion/BCI/"

# Load data (users need to set their local path to the directory containing the data above)
strain_data_folder_path = paste0(local_path,"strain_all_trees_hourly/")
strain_files = list.files(strain_data_folder_path,pattern=paste0("strain_data*"),recursive = T,full.names = F,include.dirs = F)

# Filter by completeness
strain_completeness = as.numeric(stringr::str_sub(tools::file_path_sans_ext(strain_files),start=46))
strain_files = strain_files[strain_completeness>1]


# Create function #############
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

    # Check there is sufficient data in both sensors
    num_values_dt1 = sum(!is.na(dt1$raw))
    num_values_dt2 = sum(!is.na(dt2$raw))
    if (num_values_dt1>10000 && num_values_dt2>10000){
      
      # A - Correct offset by subtracting linear prediction from raw signal 
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
      
      # B - Correct offset using a highpass filter
      dt1$padded = dt1$raw %>% zoo::na.fill(fill="extend") # Replace NAs with nearest observation
      dt2$padded = dt2$raw %>% zoo::na.fill(fill="extend") # Replace NAs with nearest observation
      
      dt1$highpass = dt1$padded %>% 
        dplR::pass.filt( W = 2400, type = c("high"), # Apply hight pass filter
                  method = c("Butterworth"),
                  n = 4, Rp = 1)
      
      dt2$highpass = dt2$padded %>% 
        dplR::pass.filt( W = 2400, type = c("high"),
                   method = c("Butterworth"),
                   n = 4, Rp = 1)
      
      dt1 %<>% mutate(corrected_highpass = highpass - mean(highpass,na.rm=T))
      dt2 %<>% mutate(corrected_highpass = highpass - mean(highpass,na.rm=T))
      
      resultant_highpass = sqrt(dt1$corrected_highpass^2 + dt2$corrected_highpass^2)
      
      # Plot the filter results to check 
      #ggplot()+geom_point(data = dt1, aes(datetime,raw))+
      #  geom_point(data = dt1, aes(datetime,predicted_linear),color="red")+
      #  geom_point(data = dt1, aes(datetime,corrected_linear),color="blue")
    #  
    #  ggplot()+geom_point(data = dt1, aes(datetime,raw))+
    #    geom_point(data = dt1, aes(datetime,highpass),color="red")
    #  ggplot()+
    #    geom_point(data = dt1, aes(datetime,corrected_highpass),color="blue")
      

      obs_expt <- 60*60*4 # number of data points there should be in 1hr at 4Hz
      my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=TRUE), NA) # deals with problem of default max function outputting -INF if there is no data
      
      tree_summary_out[[c]]= data.frame(tree_id,
                                        bending_strain_max_linear = max(resultant_linear,na.rm=T),
                                        bending_strain_q99_linear = stats::quantile(resultant_linear,probs=0.99,na.rm=TRUE),
                                        bending_strain_max_highpass = max(resultant_highpass,na.rm=T),
                                        bending_strain_q99_highpass = stats::quantile(resultant_highpass,probs=0.99,na.rm=TRUE),
                                        completeness = sum(!is.na(resultant_linear))/obs_expt)
      
      c=c+1
    } # End of check for NAs
    
  } # End of loop over tree column pairs
  hour_all_trees_summary_out  = tree_summary_out %>% dplyr::bind_rows()
  return( hour_all_trees_summary_out)
} # End of function definition

# Loop over files and apply function ##########
temp_list=list()
for(t in seq(1,length(strain_files)))  {
  print(t)
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

# Save output data #############
df_hourly_max_strain = dplyr::bind_rows(temp_list)
write.csv2(df_hourly_max_strain,paste0(local_path,"summary_data/BCI_step1_output_bending_strain_hourly_maxima_17July24.csv"))

