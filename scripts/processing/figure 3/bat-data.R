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


### Oct 31 2022 - chat with Matt - bat life history? why the delay in total calls in 2022?
# why don't we see a delay in june ? 2020, 2021, and 2022 june data look the same
# bat activity rather than bat abundance - calls don't necessarily correlate with numbers
# bats also migrate
# low bat activity in june because maybe not a lot of insects around


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
# Oct 14, 2022
# Oct 31, 2022
# Nov 5, 2022

############################################################
## get the data ready for plotting and analysis
## currently - separate csv files for 2020, 2021
## transform data from wide format to long
## shorten the dates
## stack the files
## etc
############################################################





library(tidyverse)

bat.2022.c <- read.csv("/Users/sandraemry/Documents/UBCFarm_BDM_Bat_HeatDome_2020-2022_JunJulAug.csv",
                       stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

#bat.2022.b <- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/bat_2022_day_aug_included.csv",
#                       stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

#bat.2022 <- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/bat_2022_day.csv",
#                     stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 
#
#bat.2021 <- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/bat_2021_day.csv",
#                    stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

#bat.2020<- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/bat_2020_day.csv",
#                    stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

## turn data into 'long' format from 'wide' format


bat.2022.long <- gather(bat.2022.c, species, abundance, CORTOW:TOTAL, factor_key=TRUE)
#bat.2021.long <- gather(bat.2021, species, abundance, EPTFUS:TOTAL, factor_key=TRUE)
#bat.2020.long <- gather(bat.2020, species, abundance, EPTFUS:TOTAL, factor_key=TRUE)

# format the date

bat.2022.long$DATE <- as.Date(bat.2022.long$DATE, format="%Y-%m-%d")

#bat.2021.long$DATE <- as.Date(bat.2021.long$DATE, format="%Y-%m-%d")
#bat.2020.long$DATE <- as.Date(bat.2020.long$DATE, format="%Y-%m-%d")

str(bat.2022.long)

#str(bat.2020.long)
#str(bat.2021.long)


# truncate 2022 data: June 18 to July 9 2022 (inclusive)

#bat.2022.long.short <- filter(bat.2022.long, DATE > "2022-06-16")
#bat.2022.long.short <- filter(bat.2022.long.short, DATE < "2022-07-11")

# truncate 2021 data: June 18 to July 9 2021 (inclusive)

#bat.2021.long.short <- filter(bat.2021.long, DATE > "2021-06-16")
#bat.2021.long.short <- filter(bat.2021.long.short, DATE < "2021-07-11")

# truncate 2020 data: June 18 to July 9 2020 (inclusive)

#bat.2020.long.short <- filter(bat.2020.long, DATE > "2020-06-16")
#bat.2020.long.short <- filter(bat.2020.long.short, DATE < "2020-07-11")



# add a column in each dataframe for 'pre', 'during', and 'post' heatdomw

#bat.2020.long.short <- bat.2020.long.short %>% mutate(Period =
#                                        case_when(DATE < "2020-06-25" ~ "Pre",
#                                                  DATE < "2020-07-03" ~ "During", 
#                                                  DATE <= "2020-07-10" ~ "Post"))



#bat.2021.long.short <- bat.2021.long.short %>% mutate(Period =
#                                                        case_when(DATE < "2021-06-25" ~ "Pre",
#                                                                  DATE < "2021-07-03" ~ "During", 
#                                                                  DATE <= "2021-07-10" ~ "Post"))


#bat.2022.long.short <- bat.2022.long.short %>% mutate(Period =
#                                                        case_when(DATE < "2022-06-25" ~ "Pre",
#                                                                  DATE < "2022-07-03" ~ "During", 
#                                                                  DATE <= "2022-07-10" ~ "Post"))


# stack the three data frames together
#bat.2020.to.2022 <- rbind(bat.2020.long, bat.2021.long, bat.2022.long)

#bat.2020.to.2022.short <- rbind(bat.2020.long.short, bat.2021.long.short, bat.2022.long.short)


# quick check
#str(bat.2020.to.2022) #
#str(bat.2020.to.2022.short) #


# make a new column with just month and day
bat.2022.long$Month.Day <- format(bat.2022.long$DATE, "%m-%d")

# make a new colum with just year
bat.2022.long$Year <- format(bat.2022.long$DATE, "%Y")

#bat.2020.to.2022.short$Month.Day <- format(bat.2020.to.2022.short$DATE, "%m-%d")

# quick check
#str(bat.2020.to.2022) #
#str(bat.2020.to.2022.short)


# make Year a factor
bat.2022.long$Year <- as.factor(bat.2022.long$Year)
#bat.2020.to.2022.short$Year <- as.factor(bat.2020.to.2022.short$Year)

# save the compiled file as a csv

#write.csv(bat.2020.2021, "/Users/michelletseng/Desktop/heat-dome-working-group/bat.2020.2021.csv", row.names=FALSE)

#write.csv(bat.2020.2021.short, "/Users/michelletseng/Desktop/heat-dome-working-group/bat.2020.2021.short.csv", row.names=FALSE)

#write.csv(bat.2020.to.2022, "/Users/michelletseng/Desktop/heat-dome-working-group/bat-2020-to-2022.csv", row.names=FALSE)
#write.csv(bat.2020.to.2022, "/Users/michelletseng/Desktop/heat-dome-working-group/bat-2020-to-2022-with-aug.csv", row.names=FALSE)

#write.csv(bat.2020.to.2022.short, "/Users/michelletseng/Desktop/heat-dome-working-group/bat-2020-to-2022-short.csv", row.names=FALSE)

##############################
## csv of bat data includes 2020, 2021, 2022
## just June 17 to July 10, 
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
library(visreg)
library(MASS)

#bats<- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/bat-2020-to-2022-short.csv",
#                    stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

#bats.all.dates <- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/bat-2020-to-2022-with-aug.csv",
#                           stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

# make Year a factor
#bats$Year <- as.factor(bats$Year)
#bats.all.dates$Year <- as.factor(bats.all.dates$Year)

# format DATE
#bats$DATE <- as.Date(bats$DATE, format="%Y-%m-%d")
#bats.all.dates$DATE <- as.Date(bats.all.dates$DATE, format="%Y-%m-%d")
#bats.all.dates$Month.Day <- as.Date(bats.all.dates$Month.Day, format="%m-%d")

#str(bats)
#str(bats.all.dates)


# just use 'TOTAL' and not individual species
#bat.filter.total <- filter(bats, species == "TOTAL")
bat.2022.long.total <- filter(bat.2022.long, species=="TOTAL")


# set order of levels in "Period"
#bat.filter.total$Period <- factor(bat.filter.total$Period, levels = c("Pre", "During", "Post"))

# add a column with the dates of 'Period'
#bat.filter.total <- bat.filter.total %>% mutate(Period.2 =
#                                                        case_when(Period =="Pre" ~ "Pre (Jun 17-24)",
#                                                                  Period =="During" ~ "During (Jun 25-Jul 2)", 
#                                                                  Period =="Post" ~ "Post (Jul 3-10)"))

#str(bat.filter.total)


# set order of levels in "Period.2"
#bat.filter.total$Period.2 <- factor(bat.filter.total$Period.2, levels = c("Pre (Jun 17-24)", "During (Jun 25-Jul 2)", "Post (Jul 3-10)"))

#str(bat.filter.total)


### histogram of the data

ggplot(bats.all.dates.total, aes(x=abundance, fill=Year)) +
  geom_histogram(binwidth=20, alpha=.5, position="identity")+
  #scale_fill_manual(values=c("#3333FF", "#FF9900"))+
  facet_grid(Year ~ .) +
  theme_bw()



### plot all bat occurrences over time (full dataset)

ggplot(bat.2022.long.total, aes(x=Month.Day, y=abundance, group=Year, colour=Year))+
  geom_line()+
  geom_point()+
  #scale_colour_manual(values=c("#3333FF", "#FF9900"))+
  scale_y_continuous(breaks=seq(0,1800,200))+
  #facet_grid(Year~.)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Total bat calls per day at UBC Farm")+
  xlab("Date")+
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=5))+
  geom_vline(aes(xintercept="06-25"), colour="red", linetype="dashed", lwd=0.2)+
  geom_vline(aes(xintercept="07-02"), colour="red", linetype="dashed", lwd=0.2)+
  #geom_vline(aes(xintercept="06-17"), colour="black", linetype="dashed", lwd=0.2)+
  #geom_vline(aes(xintercept="07-10"), colour="black", linetype="dashed", lwd=0.2)+
  #annotate("text", x = 21, y=1550, label = "pre HD", size=4)+
  annotate("text", x = 28, y=1550, label = "HD", size=4)
  #annotate("text", x = 36, y=1550, label = "post HD", size=4)
#theme(legend.title=element_blank())



## Why are there so few bat calls in 2022?
## stacked barplot

bat.no.total <- filter(bats.all.dates, species != "TOTAL")

ggplot(data=bat.no.total, aes(x=Month.Day, y=abundance, fill=species)) +
  geom_bar(stat="identity")+
  facet_grid(Year~.)+
  scale_fill_brewer(palette="Set3")+
  ylab("Ball calls")+
  xlab("Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  #ggtitle("All tanks")+
  theme(plot.title = element_text(size=14))+
  theme(axis.text.x  = element_text(angle=90, vjust=0.6, size=8))+
  theme(axis.title.x = element_text(angle=0, vjust=0, size=12))+
  theme(axis.text.y = element_text(size=12))+
  theme(text=element_text(size=12))+
  geom_vline(aes(xintercept="06-25"), colour="red", linetype="dashed", lwd=0.2)+
  geom_vline(aes(xintercept="07-02"), colour="red", linetype="dashed", lwd=0.2)+
  geom_vline(aes(xintercept="06-17"), colour="black", linetype="dashed", lwd=0.2)+
  geom_vline(aes(xintercept="07-10"), colour="black", linetype="dashed", lwd=0.2)+
  annotate("text", x = 21, y=1550, label = "pre HD", size=4)+
  annotate("text", x = 28, y=1550, label = "HD", size=4)+
  annotate("text", x = 36, y=1550, label = "post HD", size=4)




##########################################################
## Michelle revisiting the bat figure ## 
## need a 'pre-post' figure or a 'pre-during-post' figure 
## Sept 22, 2022

library(tidyverse)

bat <- read.csv("/Users/michelletseng/Desktop/urgent-stuff-to-do/bat.2020.2021.short.csv",
                stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

## just use 'total' calls

bat.totals <- filter(bat, species == "TOTAL")
str(bat.totals)

bat.totals$Year <- as.factor(bat.totals$Year)
bat.totals$Period.2 <- ifelse(bat.totals$Year =="2020", "Pre", "Post") #create a new pre/post variable where 2020 = Pre, 2021=Post
bat.totals$Period.2 <- factor(bat.totals$Period.2, levels = c("Pre", "Post"))


bat.totals %>%
  ggplot()+
  geom_jitter(mapping = aes(x=Year, y=abundance, colour=Period.2), 
              size=1, alpha=0.5, position=position_dodge(width=1))+
  geom_pointrange(mapping = aes(x=Year, y=abundance, colour=Period.2),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4"))+
  xlab("")+
  ylab("total bat calls")+
  theme_bw()+
  theme(legend.position="none")




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


#######################################
#######################################
## messing around with glm

## following Dolph's R tips page for fitting glm models
## https://www.zoology.ubc.ca/~schluter/R/Model.html


z1 <- glm(abundance ~ Period*Year, family = poisson(link="log"), data=bat.filter.total)
z2 <- glm(abundance ~ Period*Year, family = quasipoisson(link="log"), data=bat.filter.total)

summary(z1)

summary(z2)

#glm(formula = abundance ~ Period * Year, family = quasipoisson(link = "log"), 
#    data = bat.filter.total)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-31.951   -4.894   -0.639    5.967   20.560  
#
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             5.3174     0.3103  17.134  < 2e-16 ***
#PeriodDuring            0.2080     0.4178   0.498 0.621377    
#PeriodPost              1.4017     0.3421   4.097 0.000211 ***
#Year2021                0.3520     0.4050   0.869 0.390296    
#PeriodDuring:Year2021  -0.1190     0.5516  -0.216 0.830378    
#PeriodPost:Year2021    -0.5732     0.4590  -1.249 0.219342    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for quasipoisson family taken to be 137.4322)
#
#Null deviance: 11295.8  on 43  degrees of freedom
#Residual deviance:  5880.9  on 38  degrees of freedom
#AIC: NA
#
#Number of Fisher Scoring iterations: 5




anova(z1, test="Chisq")
anova(z2, test="F")

#Analysis of Deviance Table

#Model: quasipoisson, link: log

#Response: abundance

#Terms added sequentially (first to last)


#             Df Deviance Resid. Df Resid. Dev       F    Pr(>F)    
#NULL                           43    11295.8                      
#Period       2   5110.8        41     6185.1 18.5938 2.339e-06 ***
#  Year       1      3.3        40     6181.8  0.0239    0.8781    
#Period:Year  2    300.9        38     5880.9  1.0947    0.3450    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


########################
###
### negative binomial GLM
###
########################

## message from Diane re which model to use:

#I suggest instead of using quasilikelihood you use one of the following 
#methods for dealing with overdispersion in Poisson models:
#(1) rescale y variable, (2) use package dispmod, (3) use negative binomial instead. 
#I can provide more details on any of these options.

#Neg bin code is glm.nb (abundance ~Period*Year, link = “log”, data = bat.filter.total), 
#option to add an init.theta = …command to help it converge


############ reformat bat data for meta-analysis ############ 

bat.2022.long.total %>% 
  separate(Month.Day, into = c("month", "day"), sep = "-", remove = F) %>% 
  filter(month != "06") %>% 
  mutate(treatment = case_when(Year == "2020" ~ "control", 
                               Year == "2021" ~ "treatment",
                               TRUE ~ "treatment")) %>% 
  mutate(comparison_type = case_when(Year == "2020" ~ "temporal", 
                                     Year == "2021" ~ "temporal_during",
                                     TRUE ~ "temporal_post")) %>% 
  group_by(comparison_type) %>% 
  add_tally() %>% 
  mutate(mean = mean(abundance),
         var = var(abundance)) %>% 
  ungroup() %>% 
  mutate(samp = n) %>% 
  select(treatment, comparison_type, mean, var, samp) %>% 
  distinct(.keep_all = TRUE) 


bat_meta <- bat.2022.long.total %>% 
  filter(Month.Day %in% c("06-25", "06-26", "06-27", "06-28", "06-29", "06-30", "07-01", "07-02")) %>% 
  mutate(treatment = case_when(Year == "2020" ~ "control", 
                               Year == "2021" ~ "treatment",
                               TRUE ~ "treatment")) %>% 
  mutate(comparison_type = case_when(Year == "2020" ~ "temporal", 
                               Year == "2021" ~ "temporal_during",
                               TRUE ~ "temporal_post")) %>% 
  group_by(comparison_type) %>% 
  mutate(mean = mean(abundance),
         var = sd(abundance)) %>% 
  ungroup() %>% 
  group_by(treatment) %>% 
  mutate(samp = 8) %>% 
  select(treatment, comparison_type, mean, var, samp) %>% 
  distinct(.keep_all = TRUE) 

  
bat_meta <- rbind(bat_meta[1, ], bat_meta)

bat_meta$comparison_type[1:2] <- c("temporal_during", "temporal_post")
  
bat_meta <- bat_meta %>% 
  pivot_wider(names_from = c(treatment),
              values_from = c(mean, var, samp)) %>% 
  mutate(response_id = 1) %>% 
  mutate(unique_species = 1) %>% 
  mutate(study_id = "Bat_UBC_Farm") %>% 
  mutate(response_def = "number_of_bat_calls") %>% 
  mutate(response_units = "number") %>% 
  mutate(region = "terrestrial") %>% 
  mutate(movement = "motile") %>% 
  relocate(., mean_control:samp_treatment, .after = movement) %>% 
  relocate(., comparison_type, .after = movement)

bat_meta$dates_control <- bat.2022.long.total %>% 
  filter(Year == "2020", Month.Day %in% c("06-25", "06-26", "06-27", "06-28", "06-29", "06-30", "07-01", "07-02")) %>% 
  distinct(DATE) %>% 
  mutate(DATE = as.character(DATE)) %>% 
  as.vector(.)

bat_meta$dates_treatment[1] <- bat.2022.long.total %>% 
  filter(Year == "2021", Month.Day %in% c("06-25", "06-26", "06-27", "06-28", "06-29", "06-30", "07-01", "07-02")) %>% 
  distinct(DATE) %>% 
  mutate(DATE = as.character(DATE)) %>% 
  as.vector(.)

bat_meta$dates_treatment[2] <- bat.2022.long.total %>% 
  filter(Year == "2022", Month.Day %in% c("06-25", "06-26", "06-27", "06-28", "06-29", "06-30", "07-01", "07-02")) %>% 
  distinct(DATE) %>% 
  mutate(DATE = as.character(DATE)) %>% 
  as.vector(.)

save(bat_meta, file="./tidy_data/meta_analysis/bat_meta.Rdata")  
           
