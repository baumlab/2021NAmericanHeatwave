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

*Script:* "scripts/main_figures/fig3meta_analysis.R""

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

## Meta-analysis

### Step 1: The raw data provided by data providers was tidied as follows:

**Aphids**
*Script:* "scripts/processing/figure 3/fig3_aphids-on-berries.R"
*Input:* "/tidy_data/aphids-on-berries.csv"
*Output:* "/tidy_data/meta_analysis/aphids_meta.Rdata"

**Balanus**
*Script:* "scripts/processing/figure 3/fig3_balanus_shade_vs_sun.R"
*Input:* "tidy_data/balanus_mortality_pruth.csv"
*Output:* 
- "tidy_data/meta_analysis/balanus_meta.Rdata"
- "figures/fig3_balanus_pruthbay_by_exposure.jpg"

**Bats**
*Script:* "scripts/processing/figure 3/fig3_condensed_bat-data.R"
*Input:* "data/tidy_data/fig3/bat.2020.2021.short.csv"
*Output:* "./tidy_data/meta_analysis/bat_meta.Rdata"

**Hawks**
*Script:* "scripts/processing/figure 3/fig3_hawks.R"
*Input:* tidy_data/FEHAall2020_2022_distancetravelledovertime_filledin_June25toJuly7.csv"
*Output:* 
- "figures/maggie_proto/hawk_fig.png"
- "tidy_data/meta_analysis/hawks_meta.Rdata"

**Honeybee**
*Script:* "scripts/processing/figure 3/fig3_honeybees.R"
*Input:* "tidy_data/hive-weight-summary-by-period.csv"
*Output:* 
- "figures/maggie_proto/bee_fig.png"
- "tidy_data/meta_analysis/honeybee_meta.RData"

**Interior shrubs**
*Script:* "scripts/processing/figure 3/fig3_interior_shrubs.R"
*Input:* "Heat dome impactsv2.csv"
*Output:* 
- "tidy_data/meta_analysis/interiorshrubMAS_meta.RData"

**Lupines**
*Script:* "scripts/processing/figure 3/fig3_lupines_fruits.R"
*Input:* "tidy_data/LUPLATheatdome_fruits_20_21.csv"
*Output:* 
-"figures/maggie_proto/lupine_fig.png"
- "tidy_data/meta_analysis/lupine_meta.Rdata"

**Mammals**
*Script:* "scripts/processing/figure 3/fig3_mammals.R"
*Input:* "tidy_data/ct_hd_temp_v_detections_fig_data.csv"
*Output:* 
-"figures/maggie_proto/detection_fig.png"
- "figures/maggie_proto/detection_moose_fig.png"
- "tidy_data/meta_analysis/mammals_meta.Rdata"

**Mytilus**
*Script:* "scripts/processing/figure 3/fig3_mytilus_mortality_under_fucus.R"
*Input:* "tidy_data/mytilus_mortality.csv"
*Output:* 
-"figures/fig3_mytilus_mortality_fucus_presence.jpg"
- "figures/maggie_proto/myt_fig.png"
- "tidy_data/meta_analysis/mytilus_meta.Rdata"

**Nucella**
*Script:* "scripts/processing/figure 3/fig3_nucella_mortality.R"
*Input:* "tidy_data/nucella_mortality.csv"
*Output:* 
- "figures/nucella_mort_pre_vs_post.jpg"
- "tidy_data/meta_analysis/nucella_meta.Rdata"

**Phyllospadix**
*Script:* "scripts/processing/figure 3/fig3_phyllospadix_calvert.R"
*Input:* "tidy_data/Hakai_surfgrass_density_flowering.csv"
*Output:* 
- "tidy_data/meta_analysis/phyllospadix_meta.Rdata"

**Scoter**
*Script:* "scripts/processing/figure 3/fig3_surf_scoter.R"
*Input:* "tidy_data/surf_scoter2021.csv"   ***raw data folder empty
*Output:* 
- "figures/surfscoter_deltasd_binary.png"
- "figures/surfscoter_deltacts.png"
- "tidy_data/meta_analysis/scoter_meta.RData"

**Semibalanus**
*Script:* "scripts/processing/figure 3/fig3_semibalanus.R"
*Input:*
- "./raw_data/semibalanus/SBHW_SHADE_surveys.csv"  ***raw data folder empty
- "./raw_data/semibalanus/SBHW_SHADE_PlotInfo.csv"
*Output:* 
- "./tidy_data/meta_analysis/semibalanus_meta.Rdata"

**Songbirds**
*Script:* "scripts/processing/figure 3/fig3_semibalanus.R"
*Input:*
- "tidy_data/passerine_BirdNET_2021.csv"  ***raw data folder empty
*Output:* 
- "tidy_data/meta_analysis/songbirds_meta.Rdata"

**Algae**
*Script:* "scripts/processing/figure 3/fig3_baumlab-algae.R"
*Input:* "./raw_data/heatdome_seaweeds_baumlab.csv"
*Output:* 
- "tidy_data/meta_analysis/algae_meta.Rdata"

"figure 3"" scripts not used in meta-analysis:
- cfbirdheatdome.R - makes a plot that is not included
- fig3_Victoria_intertidal_seaweed.R - makes a plot that ?? intertidal_veg
- Victoria_intertidal_seaweed.R - makes a plot that is not saved
- Heatdome_intertidal_veg_analysis.R - makes "supplemental_algae.jpg" cant find this 
- updated_heatdome_analysis_and_plots--BriamTimmer--SamStarko.R - analyses seaweeeds

### Step 2: The tidied data were compiled:

*Script:* 
"scripts/processing/figure 3/meta_analysis_prep.R"

*Input data:*

"./data/tidy_data/fig3/meta_analysis/aphids_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/balanus_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/bat_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/hawks_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/honeybee_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/interiorshrub_meta.Rdata" # not used in script
"./data/tidy_data/fig3/meta_analysis/lupine_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/mammals_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/mytilus_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/nucella_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/phyllospadix_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/scoter_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/semibalanus_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/songbirds_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/algae_meta.Rdata"
"./data/tidy_data/fig3/meta_analysis/interiorshrubMAS_meta.RData" # not used in script

*Output data:* 
"data/tidy_data/fig3/meta_analysis/heatdome_meta.csv"


