library(tidyverse)
library(elevatr)
library(rgdal)
library(sf)
library(purrr)
library(tidyhydat)

md<-read_csv("streamflow_treeholes/input/Hydro_station_metadata.csv")

#===========streamflow metadata ===============


md["year_to"]<-md["Year To"]
md["year_from"]<-md["Year From"]
md["station_number"]<-md["Station Number"]

#here I filter for stations that have data 2000-2021 (946 stations)
md.active<- md %>% 
  filter(year_to >2020) %>% 
  filter(year_from <2001)

stations<-as.list(md.active$station_number)
stations_ec<-list()

for (i in 1:length(stations))
{
  stations_ec[i]<-paste0("EC_", stations[i], "_1")
  }

#listname <-c("EC_08EF001_1", "EC_05DA009_1")

#this is how to get elevations if make a file with lat long called examp_df

#df_elev_aws <- get_elev_point(examp_df, prj = prj_dd, src = "aws")

#importing the GDB polygon shape files

sp<-readOGR("streamflow_treeholes/watersheds/WSC_Basins.gdb")
avail.stations<-ogrListLayers("streamflow_treeholes/watersheds/WSC_Basins.gdb") %>% as.list()

#!! the file "a00000004.gdbtable" is too large for github and must be manually added to the WSC_Basins.gdb folder


#what stations are in both databases

avail.stations.df<- avail.stations %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(station_id = V1)

stations_ec.df<- stations_ec %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(station_id = V1)

stations.both<-stations_ec.df %>% 
  inner_join(avail.stations.df) %>% 
  mutate(station_id = as.character(station_id)) %>% 
  select(station_id) #now we are down to 668 catchments!

#lets try extracting one station

sp1<-readOGR("watersheds/WSC_Basins.gdb", layer = "EC_08EF001_1")
sp2<-readOGR("watersheds/WSC_Basins.gdb", layer = paste0("EC_", stations[482], "_1"))

writeOGR(sp1, "08EF001", driver = "ESRI Shapefile", layer = "EC_08EF001_1")

writeOGR(sp1, dsn = paste0("watershed.polygons/","08EF001"), driver = "ESRI Shapefile", layer = "EC_08EF001_1")

#code below adapted from line 326 https://github.com/CSHS-CWRA/Webinar_SpatialVectorProcessing_20201202/blob/main/cshs_web_sf_examples.rmd

#wsc_gdb <- "watersheds/WSC_Basins.gdb"  #where the GDB file is
fr_polygons <- list() #empty list to populate
#wsc_fn <- here::here("input", "catchments.rda")

for (i in 1:length(stations)) 
{
  fr_polygons <- readOGR(wsc_gdb, 
                              layer = stations.both$station_id[i])
  station_wsc<-stations.both$station_id[i] %>% str_split("_") %>% map(2)
  writeOGR(fr_polygons, dsn = paste0("watershed.polygons/",station_wsc), driver = "ESRI Shapefile", layer = stations.both$station_id[i])
}


#==========output============

write_csv(streamflow_treeholes/processed_data/stations.both, "stations_both.csv")
write_csv(streamflow_treeholes/processed_data/md.active, "md_active.csv")
