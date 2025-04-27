library(dplyr)
library(lubridate)
library(anytime)
library(ggplot2)


#2021 year alone

wildfires_2021<-read.csv("data/streamflow_wildfires/processed_data/wildfire_stats_2021.csv")
wildfires_2021$year<-as.factor(wildfires_2021$year)

#last 5 years
wildfires_2016<-read.csv("data/streamflow_wildfires/processed_data/wildfire_stats_2016_2020.csv")
wildfires_2016$year<-as.factor(wildfires_2016$year)

#2000-2021
wildfires_2000<-read.csv("data/streamflow_wildfires/processed_data/wildfire_stats_2000_2020.csv")

wildfires_2000$year<-as.factor(wildfires_2000$year)




#for 2000-2020 years: reformat data, calculate stand dev, filter only for summer months
fire00 <-wildfires_2000 %>% 
  mutate(MonthDay = format(as.Date(Date), "%m-%d"),
         Month = format (as.Date(Date),"%m"),
         Block = "2000-2020") %>% 
  filter(Month %in%c("06","07","08")) %>% 
  select(-Date)

#calculate the mean for 2000-2020
prior_mean_2000 <- fire00 %>% 
  group_by(MonthDay) %>% 
  summarize(
    mean = mean(fire_med_high, na.rm=TRUE),
    sd1 = mean+sd(fire_med_high, na.rm=TRUE),
    sd1low = mean-sd(fire_med_high, na.rm=TRUE),
    sd2 = mean+2*sd(fire_med_high, na.rm=TRUE),
    sd2low = mean-2*sd(fire_med_high, na.rm=TRUE),
    Block = first(Block))

#ensure the lower bound of the standard deviation is 0
prior_mean_2000$sd1low[prior_mean_2000$sd1low < 0] <- 0



#repeat for 2016-2020 data
fire16 <-wildfires_2016 %>% 
  mutate(MonthDay = format(as.Date(Date), "%m-%d"),
         Month = format (as.Date(Date),"%m"),
         Block = "2016-2020") %>% 
  filter(Month %in%c("06","07","08")) %>% 
  select(-Date)

prior_mean_2016 <- fire16 %>% 
  group_by(MonthDay) %>% 
  summarize(
    mean = mean(fire_med_high, na.rm=TRUE),
    sd1 = mean+sd(fire_med_high, na.rm=TRUE),
    sd1low = mean-sd(fire_med_high, na.rm=TRUE),
    sd2 = mean+2*sd(fire_med_high, na.rm=TRUE),
    sd2low = mean-2*sd(fire_med_high, na.rm=TRUE),
    Block = first(Block))



#for 2021 year: reformat data, filter only for summer months, create dummy variables for SD for later plot
fire21 <- wildfires_2021 %>% 
  mutate(MonthDay = format(as.Date(Date), "%m-%d"),
         Month = format (as.Date(Date),"%m"),
         Block = "2021") %>% 
  rename(Date = Date) %>% 
  mutate(
    mean = fire_med_high,
    sd1 = fire_med_high,
    sd1low = fire_med_high,
    sd2 = fire_med_high,
    sd2low = fire_med_high) %>% 
  filter(Month %in%c("06","07","08")) %>% 
  select(MonthDay, mean, sd1, sd1low, sd2, sd2low, Block)


#combine the 2021 dataframe with the mean from the 2000-2020 and 2016-2020 dataframes
combine_fire <- prior_mean_2000 %>% 
  #rbind(prior_mean_2016) %>% 
  rbind(fire21) %>% 
  mutate(MonthDayYear = mdy(paste(MonthDay, "2000", sep="-")))




##############
##plot wildfires


p2<-ggplot(data = combine_fire, aes(x = MonthDayYear, y = mean, group = Block))+
  geom_ribbon(aes(y = mean, ymin = sd1low, ymax = sd1, fill = Block), alpha = 0.5, fill = "darkgrey")+
  geom_ribbon(aes(y = mean, ymin = sd2low, ymax = sd2, fill = Block), alpha = 0.3, fill = "darkgrey")+
  geom_line(aes(y = mean, colour = Block), size = 1)+
  #geom_line(aes(group = MonthDayYear))+ vertical lines for every day
  scale_color_manual(name = "", values = c("#0000FF", "#FF0000"))+
  ylab(bquote('Area of active fires '(km^2))) +
  xlab("Day") +
  scale_x_date(date_labels = "%b")+  
  theme_bw()+
  geom_vline(xintercept = as.numeric(as.Date("2000-06-25")), linetype="dotted") +
  geom_vline(xintercept = as.numeric(as.Date("2000-07-02")), linetype="dotted") +
  theme(legend.key = element_blank(), 
        legend.title = element_text(),
        legend.position = c(0.8, 0.8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
        #plot.margin = unit(c(5.5,30,20,5.5),"pt"))
  ggtitle("a   Wildfire activity") +
  theme(plot.title = element_text(face="bold", hjust=0))

p2


#combine with streamflow plots; note that this requires the output from the script 07_streamflow_visualization.R (in the streamflow_treeholes/scripts folder) so you will need to run that first for this to work.

b3<-grid.arrange(p2, b2, nrow=2, heights=c(1,1.5))

ggsave(b3, filename = "figures_tables/Figure6.tiff", width = 10 , height = 12,device = tiff, dpi = 300)
