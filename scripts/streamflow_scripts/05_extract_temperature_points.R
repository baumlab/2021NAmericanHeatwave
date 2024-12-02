library(terra)
library(tidyverse)

#read in stations and temperature changes tif

md.active<-read_csv("data/streamflow_wildfires/processed_data/md_active.csv")

temp_changes <- rast('data/streamflow_wildfires/input/week_before_htdome_daymet_tmax.tif')
air_anom_mean<-rast("data/streamflow_wildfires/input/tmax_deviation_new5days.tif")
air_anom_max<-rast("data/streamflow_wildfires/input/tmax_deviation_max_new5days.tif")
surface_anom_mean<-rast("data/streamflow_wildfires/input/lst_deviation_new5days.tif")
surface_anom_max<-rast("data/streamflow_wildfires/input/lst_max_deviation_new5days2.tif")


#extract the lat and long for 212 stations

station_shortlist <- all_flow %>% 
  left_join(md.active, by = c("station"="Station Number")) %>%
  select(station, "Longitude", "Latitude")

shortlist2 <- station_shortlist[, c("Longitude", "Latitude")] %>% as.matrix()             

shortlist_vec <- vect(shortlist2 ,
                 crs="+proj=longlat +datum=WGS84")

#extract temp_changes data for stations

shortlist_proj <- project(shortlist_vec,
                            crs(temp_changes))

station_temp_change <- extract(temp_changes, shortlist_proj)

temp_change_loc<- cbind(station_temp_change,station_shortlist) %>% 
  select(-ID) %>% 
  rename(temp_change = max)



#repeat for air_anom_mean

shortlist_proj2 <- project(shortlist_vec,
                          crs(air_anom_mean))

station_air_anom_mean <- extract(air_anom_mean, shortlist_proj2)

air_anom_mean_loc<- cbind(station_air_anom_mean,station_shortlist) %>% 
  select(-ID,-Longitude,-Latitude) %>% 
  rename(air_anom_mean = mean)



#repeat for air_anom_max

shortlist_proj3 <- project(shortlist_vec,
                           crs(air_anom_max))

station_air_anom_max <- extract(air_anom_max, shortlist_proj3)

air_anom_max_loc<- cbind(station_air_anom_max,station_shortlist) %>% 
  select(-ID,-Longitude,-Latitude) %>% 
  rename(air_anom_max = max)



#repeat for surface_anom_mean

shortlist_proj4 <- project(shortlist_vec,
                           crs(surface_anom_mean))

station_surface_anom_mean <- extract(surface_anom_mean, shortlist_proj4)

surface_anom_mean_loc<- cbind(station_surface_anom_mean,station_shortlist) %>% 
  select(-ID,-Longitude,-Latitude) %>% 
  rename(surface_anom_mean = mean)



#repeat for surface_anom_max

shortlist_proj5 <- project(shortlist_vec,
                           crs(surface_anom_max))

station_surface_anom_max <- extract(surface_anom_max, shortlist_proj5)

surface_anom_max_loc<- cbind(station_surface_anom_max,station_shortlist) %>% 
  select(-ID,-Longitude,-Latitude) %>% 
  rename(surface_anom_max = max)



#combine all temp measures

all_temp<-temp_change_loc %>% 
  left_join(air_anom_mean_loc) %>% 
  left_join(air_anom_max_loc) %>% 
  left_join(surface_anom_mean_loc) %>% 
  left_join(surface_anom_max_loc)

#=== outputs ==========

write_csv(temp_change_loc, "data/streamflow_wildfires/processed_data/temp_change_loc.csv")
write_csv(air_anom_mean_loc, "data/streamflow_wildfires/processed_data/air_anom_mean_loc.csv")
write_csv(air_anom_max_loc, "data/streamflow_wildfires/processed_data/air_anom_max_loc.csv")
write_csv(surface_anom_mean_loc, "data/streamflow_wildfires/processed_data/surface_anom_mean_loc.csv")
write_csv(surface_anom_max_loc, "data/streamflow_wildfires/processed_data/surface_anom_max_loc.csv")
write_csv(all_temp, "data/streamflow_wildfires/processed_data/all_temp.csv")
