##### Heat Dome paper
##### Bat Data

# From Matt Mitchell
# Here is the daily bat call data from the UBC Farm from June/July for 2020 and 2021.
#
#I think the data is pretty straight forward - there is a row for each date and a tally of 
#identified bat calls for each species. Some of the calls could not be identified by the 
#program (“NoID”) and I also added a summed total of calls for each date. The program that 
#I used for this analysis (Kaleidoscope Pro from Wildlife Acoustics) has different settings 
#for the automatic identification of bat calls, and I used their “Balanced” setting (as 
#opposed to Liberal or Conservative). There is always some uncertainty around automated 
#identification of bat calls, so those results should be taken with a grain of salt, 
#especially for species with a low number of calls. I have much stronger confidence in 
#the overall number of calls recorded each day.

#LASCIN - Lasiurus cinereus, Hoary bat
#LASNOC - Lasionycteris noctivagans, Silver-haired bat
#MYOCAL - Myotis californicus, Californian myotis
#MYOLUC - Myotis lucifugus, Little brown myotis (endangered species)
#MYOYUM - Myotis yumanensis, Yuma myotis
#MYOCIL - Myotis ciliolabrum, Western small-footed myotis
#MYOVOL - Myotis volans, Long-legged myotis
#MYOEVO - Myotis evotis, Long-eared myotis
#TADBRA - Tadarida brasiliensis, Mexican free-tailed bat
#EPTFUS - Eptesicus fuscus, Big brown bat
#NYCMAC - Nyctinomops macrotis, Big free-tailed bat

# july 11, 2022
# july 12, 2022
# aug 4, 2022

############################################################
## get the data ready for plotting and analysis
## currently - separate csv files for 2020, 2021
## transform data from wide format to long
## shorten the dates
## stack the files
## etc
############################################################


library(tidyverse)

bat.2021 <- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/bat_2021_day.csv",
                    stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

bat.2020<- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/bat_2020_day.csv",
                    stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

## turn data into 'long' format from 'wide' format

bat.2021.long <- gather(bat.2021, species, abundance, EPTFUS:TOTAL, factor_key=TRUE)
bat.2020.long <- gather(bat.2020, species, abundance, EPTFUS:TOTAL, factor_key=TRUE)

# format the date
bat.2021.long$DATE <- as.Date(bat.2021.long$DATE, format="%Y-%m-%d")
bat.2020.long$DATE <- as.Date(bat.2020.long$DATE, format="%Y-%m-%d")

str(bat.2020.long)
str(bat.2021.long)

# truncate 2021 data: June 18 to July 9 2021 (inclusive)

bat.2021.long.short <- filter(bat.2021.long, DATE > "2021-06-17")
bat.2021.long.short <- filter(bat.2021.long.short, DATE < "2021-07-10")

# truncate 2020 data: June 18 to July 9 2020 (inclusive)

bat.2020.long.short <- filter(bat.2020.long, DATE > "2020-06-17")
bat.2020.long.short <- filter(bat.2020.long.short, DATE < "2020-07-10")



# change column name of bat.2020.long from year to Year
names(bat.2020.long)[names(bat.2020.long)=="year"] <- "Year"
names(bat.2020.long.short)[names(bat.2020.long.short)=="year"] <- "Year"


# add a column in each dataframe for 'pre', 'during', and 'post' heatdomw

bat.2020.long.short <- bat.2020.long.short %>% mutate(Period =
                                        case_when(DATE < "2020-06-25" ~ "Pre",
                                                  DATE < "2020-07-02" ~ "During", 
                                                  DATE <= "2020-07-09" ~ "Post"))



bat.2021.long.short <- bat.2021.long.short %>% mutate(Period =
                                                        case_when(DATE < "2021-06-25" ~ "Pre",
                                                                  DATE < "2021-07-02" ~ "During", 
                                                                  DATE <= "2021-07-09" ~ "Post"))


# stack the two data frames together
bat.2020.2021 <- rbind(bat.2020.long, bat.2021.long)

bat.2020.2021.short <- rbind(bat.2020.long.short, bat.2021.long.short)


# quick check
str(bat.2020.2021) #1464 obs of 5 variables
str(bat.2020.2021.short) #528 obs, 6 variables


# make a new column with just month and day
bat.2020.2021$Month.Day <- format(bat.2020.2021$DATE, "%m-%d")
bat.2020.2021.short$Month.Day <- format(bat.2020.2021.short$DATE, "%m-%d")

# quick check
str(bat.2020.2021) #1464 obs of 6 variables
str(bat.2020.2021.short)


# make Year a factor
bat.2020.2021$Year <- as.factor(bat.2020.2021$Year)

# save the compiled file as a csv

#write.csv(bat.2020.2021, "/Users/michelletseng/Desktop/heat-dome-working-group/bat.2020.2021.csv", row.names=FALSE)

#write.csv(bat.2020.2021.short, "/Users/michelletseng/Desktop/heat-dome-working-group/bat.2020.2021.short.csv", row.names=FALSE)

##############################
## csv of bat data includes 2020, 2021
## just June 18 to July 9, both years
## UBC Farm ultrasonic microphone (1 site)
## species IDs are not super accurate
## best to use 'total' calls
## Matt Mitchell, UBC Postoc - Juli Carrillo lab
## used Kaleidoscope Pro from Wildlife Acoustic
## to parse the bat call data
##############################

library(tidyverse)
library(lmerTest)
library(lme4)

bat.both.years<- read.csv("tidy_data/bat.2020.2021.short.csv",
                    stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

str(bat.both.years)
head(bat.both.years)

# make Year a factor
bat.both.years$Year <- as.factor(bat.both.years$Year)

# format DATE
bat.both.years$DATE <- as.Date(bat.both.years$DATE, format="%Y-%m-%d")

# just use 'TOTAL' and not individual species
bat.filter.total <- filter(bat.both.years, species == "TOTAL")

# set order of levels in "Period"
bat.filter.total$Period <- factor(bat.filter.total$Period, levels = c("Pre", "During", "Post"))

# add a column with the dates of 'Period'
bat.filter.total <- bat.filter.total %>% mutate(Period.2 =
                                                        case_when(Period == "Pre" ~ "Pre (Jun 18-24)",
                                                                  Period =="During" ~ "During (Jun 25-Jul 2)", 
                                                                  Period == "Post" ~ "Post (Jul 3-9)"))



# set order of levels in "Period.2"
bat.filter.total$Period.2 <- factor(bat.filter.total$Period.2, levels = c("Pre (Jun 18-24)", "During (Jun 25-Jul 2)", "Post (Jul 3-9)"))

str(bat.filter.total)


### plot all bat occurrences over time

ggplot(bat.filter.total, aes(x=Month.Day, y=abundance, group=Year, colour=Year))+
  geom_line()+
  geom_point()+
  scale_colour_manual(values=c("#3333FF", "#FF9900"))+
  scale_y_continuous(breaks=seq(0,1800,200))+
  #facet_grid(Year~.)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Total bat calls per day at UBC Farm")+
  xlab("Date")+
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(angle=45, vjust=0.5, size=5))+
  geom_vline(aes(xintercept="06-25"), colour="black", linetype="dashed", lwd=0.2)+
  geom_vline(aes(xintercept="07-02"), colour="black", linetype="dashed", lwd=0.2)
#theme(legend.title=element_blank())
  #ggtitle("Total bat calls recorded at UBC Farm")


## plot average number of calls in Pre, During, Post
ggplot(bat.filter.total, aes(x=Period.2, y=abundance))+ 
  geom_boxplot(aes(colour=Year),outlier.shape = NA, width= 0.7)+
  #geom_boxplot(width=0.5)+
  geom_point(aes(fill=factor(Year)), shape=21,position=position_jitterdodge(0.2),alpha=0.5, size=3)+
  #geom_jitter(shape=16,position=position_jitter(0.2),size=3) +
  scale_y_continuous(breaks=seq(0,1800,200))+
  xlab("Period") +
  ylab("Total bat calls at UBC Farm") +
  scale_colour_manual(values=c("#3333FF", "#FF9900"),name="Year")+
  scale_fill_manual(values=c("#3333FF", "#FF9900"),name="Year")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.title = element_text(size=14))+
  theme(axis.text.x  = element_text(angle=0, vjust=0.6, size=8))+
  theme(axis.title.x = element_text(angle=0, vjust=0, size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(text=element_text(size=12))

#try plot of difference between 2020-2021
bat.filter.total_wide<-select(bat.filter.total, Year, abundance, Period, species, Period.2, no)%>%
                         pivot_wider(., names_from = c("Period", "Year"), 
                              values_from = "abundance")%>%
                         mutate(diff_Pre=Pre_2021-Pre_2020, 
                                diff_During=During_2021-During_2020, 
                                diff_Post=Post_2021-Post_2020)

#put diffs in one column
bat.filter.total_wide<-mutate(bat.filter.total_wide, diff_Pre=case_when(
                              Period.2 =="During (Jun 25-Jul 2)"~diff_During, 
                              Period.2 =="Post (Jul 3-9)"~diff_Post, 
                              TRUE~diff_Pre))%>%
                              select(-diff_During, -diff_Post)%>%
                              rename(diff=diff_Pre)

## plot difference in calls in Pre, During, Post between 2020-2021
ggplot(bat.filter.total_wide, aes(x=Period.2, y=diff))+ 
  geom_boxplot(aes(fill=Period.2), width= 0.7)+
  #geom_boxplot(width=0.5)+
  #geom_point(aes(fill=factor(Year)), shape=21,position=position_jitterdodge(0.2),alpha=0.5, size=3)+
  #geom_jitter(shape=16,position=position_jitter(0.2),size=3) +
  #scale_y_continuous(breaks=seq(0,1800,200))+
  xlab(" ") +
  ylab("Change in total bat calls (2021-2020)") +
  #scale_colour_manual(values=c("#3333FF", "#FF9900"),name="Year")+
  #scale_fill_manual(values=c("#3333FF", "#FF9900"),name="Year")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  scale_fill_manual(values = c("#FFCCBC","#B71C1C", "#FF7043"))+
  scale_x_discrete(labels=c("Pre (Jun 18-24)" = "Pre", 
                            "During (Jun 25-Jul 2)" = "During", 
                            "Post (Jul 3-9)"= "Post"))

 
  
  
## stats

## average number of calls per day
## pre, during, post heat dome dates

str(bat.filter.total)

# linear model
# data do not meet assumptions b/c a lot more variation in 'post'
# need to ask group for suggestions

bat.calls.lm <- lm(abundance ~ Period + Year, data=bat.filter.total)
anova(bat.calls.lm)

#Response: abundance
#Df  Sum Sq Mean Sq F value    Pr(>F)    
#Period     2 2360838 1180419 12.8266 4.967e-05 ***
#Year       1    1443    1443  0.0157     0.901    
#Residuals 40 3681154   92029                      
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Maggie prototyping figure 

#tamiasciurus hudsonicus
bat_url = "http://phylopic.org/assets/images/submissions/18bfd2fc-f184-4c3a-b511-796aafcc70f6.128.png"
bat = readPNG(getURLContent(bat_url), 
               native = T)
bat2 <- rasterGrob(bat, interpolate = TRUE)


#just making it 2021 data during the heatdome rather than differences
bat_fig <- bat.filter.total %>%
  select(Year, no, abundance, Period, Month.Day) %>%
  filter(Year==2021)%>%
  ggplot()+
  geom_jitter(mapping = aes(x=Period, y=abundance, colour=Period), 
              size=1, alpha=0.5, position=position_dodge(width=1))+
  geom_pointrange(mapping = aes(x=Period, y=abundance, colour=Period),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(bat2, xmin=0.4, xmax=1, ymin=1000, ymax=1250)+
  theme(legend.position="none")+
  xlab("")+
  ylab("number of calls")

ggsave(file="figures/maggie_proto/bat_fig.png", bat_fig ,width = 5, height = 4, dpi = 300)


