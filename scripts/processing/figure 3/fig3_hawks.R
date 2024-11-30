library(dplyr)
library(ggplot2)
library(raster)
library(tidyverse)

#data wrangling from Lionel/Erin
traveldistances<-read.csv("tidy_data/FEHAall2020_2022_distancetravelledovertime_filledin_June25toJuly7.csv", header=TRUE)
traveldistances$year.f<-as.factor(traveldistances$year)#convert year to factor

#drop 2022
traveldistances<-traveldistances[traveldistances$year<2022,]

#set for daytime
traveldistances.daytime<-traveldistances
traveldistances.daytime$hr.c<-traveldistances.daytime$hour-12.5
traveldistances.daytime$hr.c2<-traveldistances.daytime$hr.c^2
traveldistances.daytime<-subset(traveldistances.daytime, hour>5&hour<19)#6am to 6pm 

#convert km
traveldistances.daytime$distance<-ifelse(traveldistances.daytime$distance==0,1,traveldistances.daytime$distance)
traveldistances.daytime$distance.km<-traveldistances.daytime$distance/1000

#subset to 3 hawks 
traveldistances.daytime<-traveldistances.daytime[traveldistances.daytime$id %in% c("191","803","804"),]
nrow(traveldistances.daytime) #2054

#Courtney 9/22
#summary hawks travel by day -mean and total 
hawksplot<-
group_by(traveldistances.daytime, date)%>%
  mutate(mean.distance=mean(distance.km, na.rm=TRUE),
         sum.distance=sum(distance.km, na.rm=TRUE))
#convert to julian date 
hawksplot$doy<-lubridate::yday(hawksplot$date)

hawksplot<-mutate(hawksplot, Period=case_when(doy<185~"During", #June 25-July 2 
                                                         doy>=185~"Post")) #July 3-July 7

hawk_url = "http://phylopic.org/assets/images/submissions/5c90ad4f-f3fc-4210-ad27-e8ea01f5c2f6.256.png"
hawk = readPNG(getURLContent(hawk_url), 
                native = T)
hawk2 <- rasterGrob(hawk, interpolate = TRUE)

hawk_fig<-ggplot(hawksplot)+
  geom_jitter(mapping = aes(x=Period, y=sum.distance, colour=Period), 
              size=1, alpha=0.5, position=position_dodge(width=1))+
  geom_pointrange(mapping = aes(x=Period, y=sum.distance, colour=Period),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("violetred4", "indianred2"))+
  theme_bw()+ ylab("total distance/day (km)")+
  annotation_custom(hawk2, xmin=0.5, xmax=1.1, ymin=1500, ymax=2000)+
  theme(legend.position="none")

ggsave(file="figures/maggie_proto/hawk_fig.png", hawk_fig, width = 5, height = 4, dpi = 300)



ggplot(hawksplot)+
  geom_jitter(mapping = aes(x=Period, y=mean.distance, colour=Period), 
              size=1, alpha=0.5, position=position_dodge(width=1))+
  geom_pointrange(mapping = aes(x=Period, y=sum.distance, colour=Period),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("violetred4", "indianred2"))+
  theme_bw()+ ylab("total distance/day (km)")


#simple linear mixed model 
library(lme4)
library(lmerTest)
#look at data distribution- normal?
hist(hawksplot$mean.distance)# left skewed
hist(log(hawksplot$mean.distance+1))# better?

hawksmod<-lmer(log(mean.distance)~ Period + (1|id), hawksplot)
summary(hawksmod) #post>during 


#####Meta-analysis data wrangling----
#Courtney Collins 
                                
#add pre/during/post heat dome categories
traveldistances.daytime<-mutate(traveldistances.daytime, 
                   time_period = case_when(date %in% c("2021-06-25", "2021-06-26", 
                                                    "2021-06-27", "2021-06-28", "2021-06-29",
                                                    "2021-06-30", "2021-07-01", "2021-07-02") ~ "during", 
                                        date %in% c("2021-07-03", "2021-07-04",
                                                    "2021-07-05", "2021-07-06",
                                                    "2021-07-07") ~ "post", 
                                        TRUE ~ "pre"))%>%
   #time_period  = fct_relevel(time_period, c("pre", "during", "post"))%>%
  mutate(treat = if_else(time_period=="pre", "control", "treatment"))
  # mutate(treat=if_else(time_period=="during", "treatment", "control"))

#calculate means, sd, sample sizes pre/during/post
means<-traveldistances.daytime %>% 
  select(id, year, date, time_period, treat, distance.km)%>% group_by(id, date) %>%
  mutate(daily_dist=mean(distance.km), recs=n())%>%group_by(time_period)%>% 
  mutate(mean=mean(daily_dist), var=sd(daily_dist), samp=n())%>% ungroup(.) %>% 
  select(c(time_period, treat, mean, var, samp))

meanduring<-filter(means, time_period!="post")
meanspost<-filter(means, time_period!="during")

#pull wide to look like Maggie's metadataset 
metadataduring<- dplyr::select(meanduring, mean, var, samp, treat)%>% distinct(.)%>%
  pivot_wider(names_from = "treat", values_from = c("samp", "mean", "var"))
metadatapost<- dplyr::select(meanspost, mean, var, samp, treat)%>% distinct(.)%>%
  pivot_wider(names_from = "treat", values_from = c("samp", "mean", "var"))

#add identifier cols 
metadataduring$unique_species<-row.names(metadataduring)
metadataduring$comparison_type<-"temporal_during" 
metadataduring$dates_control <-traveldistances.daytime %>% 
  filter(time_period == "pre") %>% 
  distinct(date) %>% 
  as.vector(.)
metadataduring$dates_treatment <- traveldistances.daytime %>% 
  filter(time_period == "during") %>% 
  distinct(date) %>% 
  as.vector(.)

metadatapost$unique_species<-row.names(metadatapost)
metadatapost$comparison_type<-"temporal_post" 
metadatapost$dates_control <-traveldistances.daytime %>% 
  filter(time_period == "pre") %>% 
  distinct(date) %>% 
  as.vector(.)
metadatapost$dates_treatment <-traveldistances.daytime %>% 
  filter(time_period == "post") %>% 
  distinct(date) %>% 
  as.vector(.)

#join 
metadatadf<-rbind(metadataduring, metadatapost)

#add other cols
metadatadf$study_id<-"Hawks_Bayne_2022"
metadatadf$response_id<-"1"
metadatadf$response_def<-"average daily distance km"
metadatadf$response_units<-"distance km"
metadatadf$region<-"terrestrial"
metadatadf$movement<-"motile"



hawks_meta<-dplyr::select(metadatadf, response_id, unique_species, study_id, comparison_type, response_def, 
                     response_units, region, movement, mean_control, mean_treatment, var_control, var_treatment, 
                     samp_control, samp_treatment, dates_control, dates_treatment)


#save as Rdata
save(hawks_meta, file="tidy_data/meta_analysis/hawks_meta.Rdata")


