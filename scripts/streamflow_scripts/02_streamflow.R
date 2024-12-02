library(tidyverse)
library(tidyhydat)
library(lubridate)

stations.both <- read_csv("data/streamflow_wildfires/processed_data/stations_both.csv")

#Caution: line below will take 5-10 minutes
#The existing local version of hydat, published on 2022-04-18, is the most recent version available.
sql<-download_hydat(dl_hydat_here = NULL, ask = TRUE)

#example: one station
flow_example<-hy_daily_flows(
  station_number = c("05AB021"),
  hydat_path = NULL,
  prov_terr_state_loc = NULL,
  start_date = "1980-06-01",
  end_date = "2021-08-31",
  symbol_output = "code"
)

station_noEC<-stations.both[,1] %>% 
  separate_wider_delim(station_id, delim = "_", names = c("delete", "station_id", "delete2")) %>% 
  select(-c(delete, delete2)) %>% 
  as.data.frame() %>% 
  rename(station_number = station_id) %>% 
  mutate(station_number = as.character(station_number))

#station.list<-station_noEC[,"station_number"] %>% as.list()

flow_all <-matrix(ncol=5, nrow = 0) %>% as.data.frame()

for (i in 1:nrow(stations.both))
{
  flow_one<-
    hy_daily_flows(
    station_number = station_noEC[i,"station_number"],
    hydat_path = NULL,
    prov_terr_state_loc = NULL,
    start_date = "2000-06-01",
    end_date = "2021-08-31",
    symbol_output = "code"
  )
  
  flow_all<-rbind(flow_all, flow_one)
}

#=====output========  

#write_csv(flow_all, "flow_all.csv")
#the line above creates a file that is too large for github
#if you need to save this file, do so outside of the repo
#this file is used again in scripts 04 and 07



