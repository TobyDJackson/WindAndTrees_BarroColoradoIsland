The repository hosts the R code and summary data for the following paper: 
'Wind shapes the growth strategies of trees in a tropical forest'. Jackson et al 2024. Ecology Letters.
Any questions, feel free to email me

[![DOI](https://zenodo.org/badge/775025715.svg)](https://zenodo.org/doi/10.5281/zenodo.12772776)

You can reproduce the figures and models from the paper using the R script "BCI4_analysis.R". 
You will need the summary file "BCI_step3_output_dtrees_risk_with_exposure_17July24.csv" from the "summary_data" folder.

The full work flow includes of three data processing steps. 

Step 1 summarizes the 4 Hz bending strain data to hourly maxima per tree. 

Step 2 fits these hourly maximum bending strains against wind speed to get a risk estimate per tree. This step reduces the number of trees from 109 to 95, to focus only on those where we have enough data and species replicates.

Step 3 calculates the wind exposure for each tree from the local LiDAR data. 

Step 4 does the analysis, produces the figures and runs the models. 

If you want to reproduce the whole analysis please download the full "summary_data" folder. 
This contains meta data for each tree and the manually segmented crowns for each tree. 

For step 1, please download the raw bending strain data from https://doi.org/10.5285/f03806fa-3596-4119-90c5-70254f39cfc0

For step 2, please download the wind speed data from https://smithsonian.figshare.com/articles/dataset/Barro_Colorado_Island_Lutz_tower_48m_Wind_Speed/10042427

For step 3, please download the LiDAR data from https://smithsonian.figshare.com/articles/dataset/Digital_Surface_Models_Whole_Island_2018-2023/24021381

output data from each step is also saved in the 'summary_data' folder so you can run part of the analysis. 
The first step takes quite a long time (around 6 hours on my 2024 Macbook Pro). 

The variable names, definitions and units for each of the summary data files are given below: 

For the file “BCI_tree_metadata.csv” the variables are:

tree_id – unique identifier for each tree, which also corresponds to the ID of one of the BioMech sensors which was placed on the tree. 

Tree_height_m – field measured tree height in metres. 

Tree_dbh_mm  - diameter of the tree trunk at breast height in millimetres.

Sensors_height – height on the tree trunk at which the sensor was attached in metres. 

Location – general area on Barro Colorado Island where the tree is located.

Sink_logger – identifier for the data logger which stored the data for this tree. 

Wood_density – wood specific gravity for this species (dimensionless).

Elasticity_kpsi – modulus of elasticity for this species (kilo pounds per square inch) .

Genus – the genus name for this species.

Genus_wd – a combined variable with both the genus name and wood density, for use in graphs.



For the file “BCI_step1_output_bending_strain_hourly_maxima_17July24.csv” the variables are:

tree_id – unique identifier for each tree, which also corresponds to the ID of one of the BioMech sensors which was placed on the tree. 

bending_strain_max_linear – the maximum bending strain for this hour of data calculated using a linear model to remove the sensor drift. 

bending_strain_q99_linear – the 99th percentile of bending strain for this hour of data calculated using a linear model to remove the sensor drift. 

bending_strain_max_highpass – the maximum bending strain for this hour of data calculated using a highpass filter to remove the sensor drift. 

bending_strain_q99_highpass – the 99th percentile of bending strain for this hour of data calculated using a highpass filter to remove the sensor drift. 

Completeness – measurements were taken four times per second but were often lost in transmission, or due to battery failure. The completeness gives the proportion of measurements retrieved.

Datetime – The date and time the measurement was taken, in YYYY-MM-DD hh:mm:ss format, in the Panama timezone.



The file “BCI_step2_output_dtrees_risk_17July24.csv” contains all the variables from “BCI_tree_metadata.csv” and also:

risk_at_37 – the predicted wind mortality risk at a wind speed of 37 m/s .

fit_type – the type of fit used to make the prediction above.

num_hours_data – the number of hours of data available for this tree.

tree_dbh_m – the diameter of the trunk of the tree at breast height in metres.

slenderness – the ratio of tree height to diameter (dimensionless).



The file “BCI_step3_output_dtrees_risk_with_exposure_17July24.csv” contains all the variables from  “BCI_step2_output_dtrees_risk_17July24.csv” and also:

ID – the identifier of the polygon representing the tree crown.

Dsm – the mean height of the canopy surface within the tree crown (in metres above sea level).

Dtm - the mean ground elevation within the tree crown (in metres above sea level).

surrounding_dsm - the mean height of the surrounding canopy (in metres above sea level).

surrounding_dtm - the mean ground elevation of the surroundings (in metres above sea level).

buffer_type – the shape used to define the ‘surroundings’, either circular or triangular.

buffer_width  - the size of the surrounding area considered.

surroundings_added – logical describing whether the surrounding canopy area was added as an average of other trees. This was only used where the tree crown was not manually mapped.

dtm_sink_mean – the mean ground elevation of all the trees connected to this sink logger .

surrounding_dtm_sink_mean – the mean ground elevation of the surroundings for all the trees connected to this sink logger.

surrounding_dsm_sink_mean - the mean canopy surface height of the surroundings for all the trees connected to this sink logger.

tree_height_m_plus_dtm – the field measured tree height plus the ground elevation for this tree.

exposure_dsm_field – the wind exposure of this tree, calculated as how much taller the tree is than its surrounds, as defined above.


