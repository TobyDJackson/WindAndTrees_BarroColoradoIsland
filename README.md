The repository hosts the R code and summary data for the following paper: 
'Wind shapes the growth strategies of trees in a tropical forest'. Jackson et al 2024. Ecology Letters.

You can reproduce the figures and models from the paper using the R script "BCI4_analysis.R". 
You will need the summary file "BCI_step3_output_dtrees_risk_with_exposure_17July24.csv" from the "summary_data" folder.

The full work flow consists of three previous steps. 
Step 1 summarizes the 4 Hz bending strain data to hourly maxima per tree. 
Step 2 fits these hourly maximum bending strains against wind speed to get a risk estimate per tree. 
Step 3 calculates the wind exposure for each tree from the local LiDAR data. 
Step 4 does the analysis, produces the figures and runs the models. 

If you want to reproduce the whole analysis please download the full "summary_data" folder. 
This contains meta data for each tree and the manually segmented crowns for each tree. 

For step 1, please download the raw bending strain data from https://doi.org/10.5285/f03806fa-3596-4119-90c5-70254f39cfc0
For step 2, please download the wind speed data from https://smithsonian.figshare.com/articles/dataset/Barro_Colorado_Island_Lutz_tower_48m_Wind_Speed/10042427
For step 3, please download the LiDAR data from https://smithsonian.figshare.com/articles/dataset/Digital_Surface_Models_Whole_Island_2018-2023/24021381

output data from each step is also saved in the 'summary_data' folder so you can run part of the analysis. 
The first step takes quite a long time (around 6 hours on my 2024 Macbook Pro). 

Any questions, feel free to email me
