# DO Data Processing  

This repository contains code that processes NERRS monitoring data for the DO Dashboard (see [`do-dashboard` repo](https://github.com/nerrscdmo/do-dashboard)).  


## Inputs:  

The following files should be in the `data` directory:  

-  `.zip` folder from a CDMO AQS download - should have one `.csv` file per station per year.  
-  `sampling_stations.csv` - copy this out of the zip folder; it's got useful metadata on the stations.  

In a sub-folder, `data/trends from synthesis`, there are several output files from the synthesis effort. The one used in this dashboard is `long-term-trends.RData`. **These trends will need to be re-calculated with updated data when this dashboard is updated.** That will be the tricky part of updating this dashboard.


## Scripts to Run:  


To update the trends, download fresh data (**make sure the final year has been through at least annual qc at reserves**; there were some qaqc issues with recently submitted data during this effort); then work of off the following, from the SWMP synthesis effort, to calculate trends:  

-  [trend analysis Rmd](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/f7cbea75e33a55144f8b892123775bdaff02431a/R/Analyses_for_paper/02_long-term-trends/02_long-term_trend_analyses.Rmd)  (that is a commit from before some changes were made that affect nutrient trend calculations, but wouldn't affect DO trend calculations)  
-  [WQ GAM function](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/f7cbea75e33a55144f8b892123775bdaff02431a/helper_files/functions.R#L415)  
-  [DO-threshold GAM function](https://github.com/Lake-Superior-Reserve/WQ_SWMP_Synthesis/blob/f7cbea75e33a55144f8b892123775bdaff02431a/helper_files/functions.R#L467) - used for DO time below thresholds, because those are proportions and so we use the beta family.  



Once trends have been calculated, run the following scripts, in the `R` directory, in order:  

1.  `01_downloaded_processing.R` - this pulls together data from the `.zip` folder into monthly summaries of DO data (monthly median DO; percent of time below each threshold). Only data points with flags of 0 and 4 are retained and used in these summaries. This script produces a single qaqc'd file per station, containing all data in the downloaded time period for that station, in the `data/QAQCd_monthly` folder.  
2.  `02_combining.R` - combines all stations' monthly summaries into a single data frame. Two versions are created; one containing all information generated in the first script, and another with only the key columns retained. Both files are in the `data/combined` folder.  
3.  `03_summarizing.R` - generates summary tables; combines monthly information with trend information and other metadata; generates `do_dataframes.RData` in the `data` folder.   


## Outputs:  

`do_dataframes.RData` needs to be copied from this processing repo into the `data_wq` folder of the `do-dashboard` project.