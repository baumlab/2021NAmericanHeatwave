
#### Heat Dome Project
#### Honey Bee Data
#### Data come from www.mybroodminder.com
#### Broodminder is hardware/software that tracks the weight, humidity, and temperature
#### of honeybee hives
#### hives came from locations all over BC

#### Alison McAfee (Postdoc UBC MSL & North Carolina State University)
#### Michelle Tseng - UBC

#### weight data: Rose Prairie, Fort St. John, Dawson Creek, West Creston, South Creston
#### East Creston, Dome Creek, Prince George, Pitt Meadows, Cobble Hill, Williams Lake, Telkwa

#### temperature data (for supp mat figure)
#### Same as weight data, plus Kelowna

#### honey bee stats for the heat dome group
#### this is a subset of the total data/analysis/data wrangling 




##########################################
### For main text Figure 3 
### Honey Bee Hive Growth Rate Data
### Pre, During, Post 2021 Heat Dome
### Boxplots
##########################################


library(tidyverse)
library(lmerTest)
library(lme4)

hive.weight.summary.3b <- read.csv("tidy_data/hive-weight-summary-by-period.csv",
                                   stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

str(hive.weight.summary.3b)

### anonomize the hive names
hive.weight.summary.3b$hive.2 <- factor(hive.weight.summary.3b$hive, labels=c(1:15))
hive.weight.summary.3b$hive.2 <- as.factor(hive.weight.summary.3b$hive.2)


# set order of levels for Period

hive.weight.summary.3b$Period <- factor(hive.weight.summary.3b$Period, levels = c("Pre", "During", "Post"))


############################################################
##
## boxplot of hive growth rate for pre, during, and post heat wave
##
############################################################

# rename the levels for "Above.38"
labels <- c(No = "Max hive temp below 38", Yes = "Max hive temp above 38")


## boxplot of hive growth rate for pre, during, and post heat wave

ggplot(hive.weight.summary.3b)+ 
  geom_boxplot(aes(x=Period, y=growth.rate, fill=Period), 
               width= 0.5)+
  #geom_boxplot(width=0.5)+
  #geom_jitter(shape=21,position=position_jitter(0.1),size=3) +
  #xlab("Pre, during, or post Heat Dome") +
  
  ylab(expression("Hive growth rate "~(delta~g/ g))) + xlab(" ")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  scale_fill_manual(values = c("#FFCCBC","#B71C1C", "#FF7043"))

## stats ##
## linear mixed effects models, hive = random effect

hive.growth.lmer <- lmer(growth.rate ~ Period + Above.38 +
                           (1|hive), data=hive.weight.summary.3b)
anova(hive.growth.lmer)


#Type III Analysis of Variance Table with Satterthwaite's method
#            Sum Sq   Mean Sq NumDF DenDF F value Pr(>F)
#Period   0.0045831 0.0022915     2    28  0.2080 0.8135
#Above.38 0.0040635 0.0040635     1    13  0.3688 0.5541



##################################################
### For Supplmental Materials
### Honey Bee Hive temperature vs ambient temperature
### Shows that honey bees can regulate the temperature of the hive
### 
##################################################

hives2 <- read.csv("/Users/michelletseng/Desktop/heat-dome-working-group/honey-bee-data/all.hives.2.filtered.csv",
                   stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 

str(hives2)

# format date and time
hives2$Local_TimeStamp <-as.POSIXct(hives2$Local_TimeStamp, format="%Y-%m-%d %H:%M")

# format date
hives2$Date <- as.Date(hives2$Date, format = "%Y-%m-%d")

# summary of max-min temps per hive
hives2.summary <- group_by(hives2, hive, Hive_Position) %>%
  summarise(max=max(Temperature), min=min(Temperature))

hives2.summary

#set order of factors
hives2$Hive_Position <- factor(hives2$Hive_Position, levels = c("Above Upper Brood Box", "Above Lower Brood Box", "Scale Under Hive"))

### remove all hives that don't have ambient temperature data
### assuming here that 'scale under hive' is where the ambient temperature logger is
hives3 <- group_by(hives2, hive) %>%
  filter(any(Hive_Position == "Scale Under Hive"))

#### remove KC002 because no temp logger data within hive
####  SG004 looks super wonky
hives4 <- filter(hives3, hive != "KC002")
hives4 <- filter(hives4, hive != "SG004")

## anonomize hive names
hives4$hive.2 <- factor(hives4$hive, labels=c(1:19))
hives4$hive.2 <- as.factor(hives4$hive.2)

### summarise - avg temp and max time per day
hives4.summary.temp <- group_by(hives4, hive, hive.2, Hive_Position, Date) %>%
  summarise(avg.temp.day = mean(Temperature, na.rm=TRUE), max.temp.day = max(Temperature, na.rm=TRUE))

####################################
### plot average daily temperature
####################################

ggplot(hives4.summary.temp, aes(x=Date, y=avg.temp.day, group=Hive_Position, colour=Hive_Position))+
  geom_line(size=0.5)+
  geom_point(size=0.5)+
  scale_colour_manual(values=c("#3333FF", "#FF9900", "#CCCCCC"),
                      labels=c("Upper Hive", "Lower Hive", "Ambient"))+
  #scale_y_continuous(breaks=seq(15,30,1))+
  facet_wrap(~hive.2)+
  ylim(10,50)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab(expression("Average daily temperature"~(degree*C))) +
  xlab("Date")+
  geom_vline(xintercept = as.Date(c("2021-06-25", "2021-07-02")), 
             color = "black", linetype="dashed", lwd = 0.3) +
  geom_hline(yintercept = 38, colour="black", linetype="dashed", lwd=0.3)+
  theme(legend.title=element_blank())+
  theme(axis.text.x  = element_text(angle=0, vjust=0.6, size=7))




########################################################################
### plot max daily temperature
### Heat Dome paper - supplementary figures
### shows temperature regulation within the hives
### max temp goes above 38 (critical bee temp) but hive temp rarely does
########################################################################


ggplot(hives4.summary.temp, aes(x=Date, y=max.temp.day, group=Hive_Position, colour=Hive_Position))+
  geom_line(size=0.5)+
  geom_point(size=0.5)+
  scale_colour_manual(values=c("#3333FF", "#FF9900", "#CCCCCC"),
                      labels=c("Upper Hive", "Lower Hive", "Ambient"),
                      name="Temperature")+
  #scale_y_continuous(breaks=seq(15,30,1))+
  facet_wrap(~hive.2)+
  ylim(10,70)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab(expression("Maximum daily temperature"~(degree*C))) +
  xlab("2021 Date")+
  geom_vline(xintercept = as.Date(c("2021-06-25", "2021-07-02")), 
             color = "black", linetype="dashed", lwd = 0.3) +
  geom_hline(yintercept = 38, colour="black", linetype="dashed", lwd=0.3)+
  #theme(legend.title=element_blank())+
  theme(axis.text.x  = element_text(angle=0, vjust=0.6, size=7))



#maggie prototype figure
pacman::p_load(png, grid, RCurl, rphylopic, patchwork, gridExtra)

library(RCurl)

#tamiasciurus hudsonicus
bee_url = "http://phylopic.org/assets/images/submissions/b199a5f5-20c4-4cc9-9c54-1b51578c2487.128.png"
bee = readPNG(getURLContent(bee_url), 
               native = T)
bee2 <- rasterGrob(bee, interpolate = TRUE)

hive <- hive.weight.summary.3b %>%
  ggplot()+
  geom_jitter(mapping = aes(x = Period, y = growth.rate, colour=Period), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(mapping = aes(x = Period, y = growth.rate, colour=Period),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(bee2, xmin=0.5, xmax=1.2, ymin=0.27, ymax=0.52)+
  theme(legend.position="none")

ggsave(file="figures/maggie_proto/bee_fig.png", hive, width = 5, height = 4, dpi = 300)

################################################################################
##### META-ANALYSIS DATA FORMATTING -----------------------
################################################################################
# Katie Goodwin Dec 6, 2022
library(tidyverse)

# each period is own column
hive_meta1 <- hive.weight.summary.3b %>%  
  pivot_wider(id_cols = hive.2, names_from = Period, values_from = growth.rate) 

# temporal_during comparison
hive_metad <- hive_meta1 %>% 
 summarize(response_id = 1,
            unique_species = 1,
           species = "honeybees",
            study_id = "honeybees",
            comparison_type = "temporal_during",
            response_def = "hive growth rate",
            response_units = "delta~g/ g",
            region = "terrestrial",
            movement = "motile",
            mean_control = mean(Pre),
            mean_treatment = mean(During),
            var_control = sd(Pre),
            var_treatment = sd(During),
            samp_control = n(),
            samp_treatment = n())

# temporal_post comparison
hive_metap <- hive_meta1 %>% 
  summarize(response_id = 1,
            unique_species = 1,
            study_id = "honeybees",
            comparison_type = "temporal_post",
            response_def = "hive growth rate",
            response_units = "delta~g/ g",
            region = "terrestrial",
            movement = "motile",
            mean_control = mean(Pre),
            mean_treatment = mean(Post),
            var_control = sd(Pre),
            var_treatment = sd(Post),
            samp_control = n(),
            samp_treatment = n())


# combine 2 comparisons
hive_meta <- rbind(hive_metad, hive_metap)

save(hive_meta, file="./tidy_data/meta_analysis/honeybee_meta.RData")

