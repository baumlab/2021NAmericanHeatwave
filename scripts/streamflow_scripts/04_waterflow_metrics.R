# Load packages
library(dplyr) 
library(tidyr) 
library(stringr) 
library(lubridate) ## Deal with dates

all_dats<-read_csv("data/streamflow_wildfires/processed_data/waterflow2021_dailymean.csv")

#flow_all<- read_csv("flow_all.csv")

#=== 2000-2020 ==========

# first working with 2000-2020 data, isolate the days of interest into three datasets

earlyjune<-flow_all%>% 
  mutate(DayMonth = format(as.Date(Date), "%d-%m"),
         DayMonth = as.character(DayMonth)) %>% 
  mutate(Year = format(as.Date(Date), "%y")) %>% 
  filter(DayMonth %in% c("11-06","12-06", "13-06","14-06","15-06","16-06", "17-07"))

june<-flow_all%>% 
  mutate(DayMonth = format(as.Date(Date), "%d-%m"),
         DayMonth = as.character(DayMonth)) %>% 
  mutate(Year = format(as.Date(Date), "%y")) %>% 
  filter(DayMonth %in% c("25-06","26-06", "27-06","28-06","29-06","30-06", "01-07","02-07"))

june2wk<-flow_all%>% 
  mutate(DayMonth = format(as.Date(Date), "%d-%m"),
         DayMonth = as.character(DayMonth)) %>% 
  mutate(Year = format(as.Date(Date), "%y")) %>% 
  filter(DayMonth %in% c("25-06","26-06", "27-06","28-06","29-06",
                         "30-06", "01-07","02-07", "03-07", "04-07",
                         "05-07", "06-07","07-07", "08-07"))
aug<-flow_all%>% 
  mutate(Month = format(as.Date(Date), "%m")) %>% 
  mutate(Year = format(as.Date(Date), "%y")) %>% 
  filter(Month =="08")

flow_all %>% filter(between(Date, as.Date('2019-06-25'), as.Date('2019-07-02'))) %>% 
  group_by(STATION_NUMBER) %>% 
  summarize(mean = mean(Value, na.rm=TRUE)) %>% 
  view() #559 in 2019, 373 stations reporting in 2020, 647 in 2000, 660 in 2010

#then calculate means over the periods of interest for each year in each station
#then calculate within-period means, filtering stations with at least 50% of days with data in at least 80% years

earlyjune_mean<- earlyjune %>% 
  group_by(STATION_NUMBER, Year) %>% 
  summarize(flow_days = sum(!is.na(Value)),
            mean_flow = if (flow_days > 3){mean(Value, na.rm=TRUE)} else {NA})%>% 
  group_by(STATION_NUMBER) %>% 
  summarize(earlyjune_flow_years = sum((flow_days>3)),
            earlyjune_mean = if(earlyjune_flow_years>15) {mean(mean_flow, na.rm=TRUE)} else {NA}) 

june_mean<- june %>% 
  group_by(STATION_NUMBER, Year) %>% 
  summarize(flow_days = sum(!is.na(Value)),
            mean_flow = if (flow_days > 3){mean(Value, na.rm=TRUE)} else {NA})%>% 
  group_by(STATION_NUMBER) %>% 
  summarize(june_flow_years = sum((flow_days>3)),
            june_mean = if(june_flow_years>15) {mean(mean_flow, na.rm=TRUE)} else {NA}) 

june_2wkmean<- june2wk %>% 
  group_by(STATION_NUMBER, Year) %>% 
  summarize(flow_days = sum(!is.na(Value)),
            mean_flow = if (flow_days > 6){mean(Value, na.rm=TRUE)} else {NA})%>% 
  group_by(STATION_NUMBER) %>% 
  summarize(june2wk_flow_years = sum((flow_days > 6)),
            june_2wkmean = if(june2wk_flow_years>15) {mean(mean_flow, na.rm=TRUE)} else {NA}) 

aug_mean<- aug %>% 
  group_by(STATION_NUMBER, Year) %>% 
  summarize(flow_days = sum(!is.na(Value)),
            mean_flow = if (flow_days > 15){mean(Value, na.rm=TRUE)} else {NA})%>% 
  group_by(STATION_NUMBER) %>% 
  summarize(aug_flow_years = sum((flow_days > 15)),
            aug_mean = if(aug_flow_years>15) {mean(mean_flow, na.rm=TRUE)} else {NA}) 

#combine all historical flow metrics

prior_means<- june_mean %>% 
  left_join(june_2wkmean, by = c("STATION_NUMBER"= "STATION_NUMBER")) %>% 
  left_join(aug_mean, by = c("STATION_NUMBER"= "STATION_NUMBER")) %>% 
  left_join(earlyjune_mean, by = c("STATION_NUMBER"= "STATION_NUMBER"))

#=== 2021 ==========

#create same metrics for 2021 

june21<-all_dats%>% 
  filter(date >'2021-06-25') %>% #for greater than date, it includes the date (as if >=)
  filter(date <'2021-07-03') %>% #for less than date, it excludes the date as expected
group_by(station) %>% 
  summarize(june21_flow_days = sum(!is.na(daily_flow)),
            june21_mean = if (june21_flow_days > 3){mean(daily_flow, na.rm=TRUE)} else {NA},
            Approval.Name = first(Approval.Name), #we will just do this once, given generally always consistent over days
            Grade.Name = first(Grade.Name)) #we will just do this once, given generally always consistent over days

june2wk21<-all_dats%>% 
  filter(date >'2021-06-25') %>% 
  filter(date <'2021-07-09') %>% 
  group_by(station) %>% 
  summarize(june2wk21_flow_days = sum(!is.na(daily_flow)),
            june2wk21_mean = if (june2wk21_flow_days > 6){mean(daily_flow, na.rm=TRUE)} else {NA}) 

aug21 <-all_dats%>% 
  filter(date >'2021-08-01') %>% 
  filter(date <'2021-09-01') %>% 
  group_by(station) %>% 
  summarize(aug21_flow_days = sum(!is.na(daily_flow)),
            aug21_mean = if (aug21_flow_days > 15){mean(daily_flow, na.rm=TRUE)} else {NA}) 

earlyjune21<-all_dats%>% 
  filter(date >'2021-06-11') %>% #for greater than date, it includes the date (as if >=)
  filter(date <'2021-06-18') %>% #for less than date, it excludes the date as expected
  group_by(station) %>% 
  summarize(earlyjune21_flow_days = sum(!is.na(daily_flow)),
            earlyjune21_mean = if (earlyjune21_flow_days > 3){mean(daily_flow, na.rm=TRUE)} else {NA})


dome_means<- june21 %>% 
  left_join(june2wk21, by =c("station"="station")) %>% 
  left_join(aug21, by =c("station"="station")) %>% 
  left_join(earlyjune21, by =c("station"="station"))

#=== combine 2000-2020 and 2021 ==========

#finally, join the prior and heat dome years together

all_flow <- dome_means %>% 
  left_join(prior_means, by = c("station"= "STATION_NUMBER")) %>% 
  mutate(june_ratio = june21_mean/june_mean,
         june2wk_ratio = june2wk21_mean/june_2wkmean,
         aug_ratio = aug21_mean/aug_mean,
         earlyjune_ratio = earlyjune21_mean/earlyjune_mean)

#=== output ==========

write_csv(june_mean, "data/streamflow_wildfires/processed_data/june_mean.csv")
write_csv(june_2wkmean, "data/streamflow_wildfires/processed_data/june_2wkmean.csv")
write_csv(aug_mean, "data/streamflow_wildfires/processed_data/aug_mean.csv")
write_csv(all_flow, "data/streamflow_wildfires/processed_data/all_flow.csv")
