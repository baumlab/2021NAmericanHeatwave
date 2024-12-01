# 2021NAmericanHeatwave
This repository is for the synthesis paper 'Widespread ecological responses and cascading effects of the record-breaking 2021 western North American heatwave'

## Reproduction of figures and tables


**Figure 1**


**Figure 2**


*Script:* scripts/main_figures/fig_TPC_conceptual.R

*Input data:* 
10-day mean air temperature leading up to heat dome period:
"fig2_raster_data/pre_heatdome_norm_avg_Tavg.tif" # Historical
"fig2_raster_data/pre_2021heatdome_avg_Tavg.tif" # 2021

Avg surface temperature during period of heat dome; assumed = leaf temperature:
"fig2_raster_data/heatdome_norm_avg_LST_proj.tif" # Historical
"fig2_raster_data/heatdome_2021_avg_LST_proj.tif"# 2021

Avg VPD during period of heat dome:
"fig2_raster_data/heatdome_norm_avg_VPD.tif"# Historical
"fig2_raster_data/heatdome_2021_avg_VPD.tif"# 2021

*Output figure:* fig2.png


**Figure 3 and Extended Data Table 2**

*Script:*

*Input:*
- "data/tidy_data/fig3/meta_analysis/heatdome_meta.csv"
(the construction of this dataset is detailed below under "Meta-analysis")

*Output:*
- figures/fig3.png
- extendeddatatable2.csv


**Figure 4** is a photo montage, so no underlying data

**Figure 5**

*Script:* scripts/main_figures/fig5_GPP_map.R

*Input data:* 
10-day mean air temperature leading up to heat dome period:
"fig2_raster_data/pre_heatdome_norm_avg_Tavg.tif" # Historical
"fig2_raster_data/pre_2021heatdome_avg_Tavg.tif" # 2021

Avg surface temperature during period of heat dome; assumed = leaf temperature:
"fig2_raster_data/heatdome_norm_avg_LST_proj.tif" # Historical
"fig2_raster_data/heatdome_2021_avg_LST_proj.tif"# 2021

Avg VPD during period of heat dome:
"fig2_raster_data/heatdome_norm_avg_VPD.tif"# Historical
"fig2_raster_data/heatdome_2021_avg_VPD.tif"# 2021

*Output figure:* fig5.png


**Figure 6**

*Scripts*, to be run in numbered order, in scripts/streamflow_scripts:

- 01_Hydro_station_metadata
- 02_streamflow
- 03_Collating_2021_streamflow_data
- 04_waterflow_metrics
- 05_extract temperature_points
- 06_all_data_compiled
- 07_streamflow_visualization
 
then run: scripts/main_figures/fig6_wildfire_area_and_streamflow

*Input data:*

- data/streamflow_wildfires/input/Hydro_station_metadata.csv
- data/streamflow_wildfires/input/week_before_htdome_daymet_tmax.tif
- data/streamflow_wildfires/input/tmax_deviation_new5days.tif
- data/streamflow_wildfires/input/tmax_deviation_max_new5days.tif
- data/streamflow_wildfires/input/lst_deviation_new5days.tif
- data/streamflow_wildfires/input/lst_deviation_max_new5days.tif
- data/streamflow_wildfires/input/glacier_snowcover_per_watersheds.csv
- data/streamflow_wildfires/input/snowcover_per_watersheds.csv
- data/streamflow_wildfires/processed_data/wildfire_stats_2000_2020.csv
- data/streamflow_wildfires/processed_data/wildfire_stats_2016_2020.csv
- data/streamflow_wildfires/processed_data/wildfire_stats_2021.csv
- watersheds/WSC_Basinsgdb/ [all files]
- waterflow_2021/ [all files]

*Intermediate data object* (facilitates recreation of regression models):
- data/streamflow_wildfires/processed_data/xvar_streamflow.csv

*Output:*
- regression.tif
- Figure6.tiff


Script: Heatdone_intertidal_veg_analysis.R"
Input data:"./data/intertidal_veg_grouped.csv"
Output:"supplemental_algae.jpg"

