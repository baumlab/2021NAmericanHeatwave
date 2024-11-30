#### heat dome project
#### aphid data from Michelle Franklin, Agriculture and Agrifood Canada
#### michelle.franklin@AGR.GC.CA
#### collaboration with Warren Wong (PhD student), Juli Carrillo UBC-LFS

#### Background
#### AAFC have been testing aphid resistance of a few varieties of blueberries
#### and raspberries; plants were sampled regularly for aphids from 2019-2021
#### Blueberries were grown at a few different locations, Raspberries at 1 location
#### Total of 10 terminals per plant were sampled
#### the number of terminals with or without aphids was recorded
#### total terminals should add to 10 (a few times it adds to 9 b/c
#### the plant did not have 10 terminals)
#### Every once in awhile the leaves were brought back to the lab and all of the
#### aphids on the leaves were counted

## post = the number of the post (helps to keep track of where the plants were)
## plants were planted between posts


library(tidyverse)
library(lubridate)
library(scales)

### chatted with Michelle Franklin on Oct 13, 2022
### Best to just use location CB (Clearbrook sub-station, Abbotsford BC) 
### for both blueberries and raspberries
### Best data are July 2019, 2020, 2021, for both blueberries and raspberries
### sites were sprayed insecticide, but similar insecticide used each year

#The dominant aphid species on blueberry is Ericaphis fimbriata and 
#the aphid on raspberry is Amphorophora agathonica.

aphids <- read.csv("./tidy_data/aphids-on-berries.csv",
                  stringsAsFactors=FALSE, strip.white=TRUE, na.strings=c("NA", "")) 


str(aphids)
aphids$total.terminals <- aphids$terminal.with.aphid + aphids$terminal.without.aphids

## need a column for proportion of terminals with aphids
## on most of the plants, 10 terminals were counted, 
## data were then recorded as 'number of terminals with aphids', 'number of terminals without aphids'

aphids$prop.term.with.aphids <- aphids$terminal.with.aphid/aphids$total.terminals


head(aphids)

# format date

aphids$date <- as.Date(aphids$date, format = "%d-%B-%y")    
str(aphids)

## make a column for just year, just month, just month-day

aphids$year <- format(aphids$date, format="%Y")
aphids$month <- format(aphids$date, format="%m")

aphids$month.day <- format(aphids$date, format="%m-%d")
#aphids$month.day <- format(lubridate::parse_date_time(aphids$month.day, "md"), "%m-%d")
#aphids$month.day <- as.Date(aphids$month.day, format = "%m-%d")



str(aphids)

## just july to get rid of seasonality patterns 
aphids.july <- filter(aphids, month == "07")

## just location = CB
aphids.july <- filter(aphids.july, location == "CB")

## blueberries, location=CB, month=july
blueberries <- filter(aphids.july, crop=="blueberry")

## raspberries, location = CB, month = july
raspberries <- filter(aphids.july, crop=="raspberry") 

##################### Reformat data for meta-analysis

blueberries_meta <- blueberries %>% 
  select(crop, prop.term.with.aphids, year) %>% 
  mutate(treatment = case_when(
    year %in% c("2019", "2020") ~ "control",
    year == "2021" ~ "treatment")) %>% 
  group_by(treatment) %>% 
  filter(!is.na(prop.term.with.aphids)) %>% 
  add_tally() %>%
  mutate(mean = mean(prop.term.with.aphids),
         var = sd(prop.term.with.aphids)) %>% 
  select(crop, treatment, mean, var, n) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment),
              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 1) %>% 
  mutate(unique_species = 1) %>% 
  mutate(study_id = "aphids_agrifood_canada") %>% 
  mutate(species = "aphids") %>% 
  mutate(comparison_type = "temporal_post") %>% 
  mutate(response_def = "proportion_plant_tips_with_aphids") %>% 
  mutate(response_units = "proportion") %>% 
  mutate(region = "terrestrial") %>% 
  mutate(movement = "sessile") %>% 
  relocate(., mean_control:samp_treatment, .after = movement) %>% 
  ungroup()

raspberries_meta <- raspberries %>% 
  select(crop, prop.term.with.aphids, year, date) %>% 
  mutate(treatment = case_when(
    year %in% c("2019", "2020") ~ "control",
    year == "2021" ~ "treatment")) %>% 
  group_by(treatment) %>% 
  filter(!is.na(prop.term.with.aphids)) %>% 
  add_tally() %>%
  mutate(mean = mean(prop.term.with.aphids),
         var = sd(prop.term.with.aphids)) %>% 
  select(crop, treatment, mean, var, n) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment),
              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 1) %>% 
  mutate(unique_species = 2) %>% 
  mutate(study_id = "aphids_agrifood_canada") %>% 
  mutate(species = "aphids") %>% 
  mutate(comparison_type = "temporal_post") %>% 
  mutate(response_def = "proportion_plant_tips_with_aphids") %>% 
  mutate(response_units = "proportion") %>% 
  mutate(region = "terrestrial") %>% 
  mutate(movement = "sessile") %>% 
  relocate(., mean_control:samp_treatment, .after = movement) %>% 
  ungroup() 

aphids_meta <- rbind(blueberries_meta, raspberries_meta)

aphids_meta$dates_control <- raspberries %>% 
  filter(year %in% c("2019", "2020")) %>% 
  distinct(date) %>% 
  mutate(date = as.character(date)) %>% 
  as.vector()

aphids_meta$dates_treatment <- raspberries %>% 
  filter(year %in% c("2021")) %>% 
  distinct(date) %>% 
  mutate(date = as.character(date)) %>% 
  as.vector()

save(aphids_meta, file="./tidy_data/meta_analysis/aphids_meta.Rdata")

### how many sampling dates were there? 

aphids.july.1 <- group_by(aphids.july, crop, date) %>%
  summarise(n.terminals=length(terminal.with.aphid), n.aphids=length(aphid.number))



########

# plot proportion of plant terminals with aphids over time, for blueberries
# keep varieties together or separate?

## blubeberry varieties combined
ggplot(blueberries, aes(x=year, y=prop.term.with.aphids))+
  geom_boxplot(outlier.shape = NA, width= 0.5, fill="grey90")+
  geom_jitter(shape=21,position=position_jitter(0.1),size=3) +
  #geom_point(size=2)+
  #facet_grid(.~variety)+
  theme_bw()+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  #panel.border = element_blank(),
  #panel.background = element_blank())+ 
  ylab("Proportion of plant terminals with aphids")+
  xlab("Date") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=8))+
  ggtitle("Blueberry aphids, Ericaphis fimbriata, sampled in July 2019-2021, all blueberry varieties combined")+
  theme(plot.title = element_text(size=8))

## blueberry varieties separated
ggplot(blueberries, aes(x=year, y=prop.term.with.aphids))+
  geom_boxplot(outlier.shape = NA, width= 0.5, fill="grey90")+
  geom_jitter(shape=21,position=position_jitter(0.1),size=3) +
  #geom_point(size=2)+
  facet_grid(.~variety)+
  theme_bw()+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  #panel.border = element_blank(),
  #panel.background = element_blank())+ 
  ylab("Proportion of plant terminals with aphids")+
  xlab("Date") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=8))+
  ggtitle("Blueberry aphids, Ericaphis fimbriata, sampled in July 2019-2021, blueberry varieties separated")+
  theme(plot.title = element_text(size=8))

# plot terminals with aphids over time, for raspberries
# keep varieties together or separate?

ggplot(raspberries, aes(x=year, y=prop.term.with.aphids))+
  geom_boxplot(outlier.shape = NA, width= 0.5, fill="grey90")+
  geom_jitter(shape=21,position=position_jitter(0.1),size=3) +
  #geom_point(size=2)+
  #facet_grid(.~variety)+
  theme_bw()+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  #panel.border = element_blank(),
  #panel.background = element_blank())+ 
  ylab("Proportion of plant terminals with aphids")+
  xlab("Date") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10))+
  ggtitle("Raspberry aphids, Amphorophora agathonica, sampled in July 2019-2021, all raspberry varieties combined")+
  theme(plot.title = element_text(size=9))

## raspberries, varieties separated
ggplot(raspberries, aes(x=year, y=prop.term.with.aphids))+
  geom_boxplot(outlier.shape = NA, width= 0.5, fill="grey90")+
  geom_jitter(shape=21,position=position_jitter(0.1),size=3) +
  #geom_point(size=2)+
  facet_grid(.~variety)+
  theme_bw()+
  theme(#axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  #panel.border = element_blank(),
  #panel.background = element_blank())+ 
  ylab("Proportion of plant terminals with aphids")+
  xlab("Date") +
  theme(axis.title.x = element_text(size=12),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=6))+
  ggtitle("Raspberry aphids, Amphorophora agathonica sampled in July 2019-2021, raspberry varieties separated")+
  theme(plot.title = element_text(size=8))

