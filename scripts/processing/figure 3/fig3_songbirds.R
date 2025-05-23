#install.packages("here")
### import library
library(tidyverse)
library(here)
library(purrr)
library(patchwork)
pacman::p_load(png, grid, RCurl, rphylopic, patchwork, gridExtra)

library(RCurl)

### read-in data
data <- read.csv("data/tidy_data/fig3/passerine_BirdNET_2021.csv")
data_weather <- read.csv("tidy_data/en_climate_daily_BC_1096468_2021_P1D.csv")
data %>%
  group_by(month, day, site) %>%
  summarize(num_detections = n()) %>%
  unite("date", month:day) %>%
  pivot_wider(id_cols = site,
              names_from = date, 
              values_from = num_detections)

### ARU data cleaning and wrangling
data_sub <- data %>%
  mutate(month = as.numeric(month), day = as.numeric(day)) %>%
  filter((month == 6 & day >= 15) | (month == 7 & day <= 8)) 

data_sub %>%
  group_by(month, day) %>%
  summarize(num_sites = unique(site) %>% length(),
            num_species = unique(common_name) %>% length(),
            num_detections = n())
# there is uneven sites across the dates, resulting from misfunctioning 
# of some ARUs on site (stop recording for a couple of days and recover).
# select the ARUs with stable performance and have data across the days.

ARU_sites <- data_sub %>%
  group_nest(month, day) %>%
  mutate(sites = map(.x = data, 
                     .f =~ .x %>%
                       pull(site) %>%
                       unique())) %>%
  pull(sites) %>%
  Reduce(intersect, .)
not_working <- c("N_22", "N_10", "N_21")

ARU_sites <- ARU_sites[! ARU_sites %in% not_working]

data_sub_1 <- data_sub %>%
  filter(site %in% ARU_sites)

data_sub_1 %>%
  group_by(month, day) %>%
  summarize(num_sites = unique(site) %>% length(),
            num_species = unique(common_name) %>% length(),
            num_detections = n())

####Activity of all species----
#Courtney wrangling 9/22
#all counts per day 

trend <- data_sub_1 %>%
 # filter(confidence >= 0.125) %>% #don't think we need because lumping all species 
  group_by(month, day) %>%
  mutate(calls_per_day = n())%>%
          dplyr::select(month, day, calls_per_day) %>%
  distinct(.)
trend$days<-as.numeric(row.names(trend))
trend<-mutate(trend, Period=case_when(days<10~"Pre", 
                                      days>=10 & days<=18~"During", 
                                      days>18~"Post"))
# all spp 
#need to re-order the x axis 
ggplot(trend)+
  geom_jitter(mapping = aes(x=Period, y=calls_per_day, colour=Period), 
              size=1, alpha=0.5, position=position_dodge(width=1))+
  geom_pointrange(mapping = aes(x=Period, y=calls_per_day, colour=Period),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()
#no effect 

##activity of 6 species of interest 
data_sub_2<-subset(data_sub_1, common_name=="Olive-sided Flycatcher"|
                     common_name=="Pacific Wren" |
                     common_name=="Orange-crowned Warbler" |
                     common_name=="Ruby-crowned Kinglet"|
                     common_name=="Swainson's Thrush"|
                     common_name=="Western Wood-Pewee")
trend2 <- data_sub_2 %>%
  filter(confidence >= 0.125) %>% #here used the most common value from above (all but OSFL)
  group_by(month, day) %>%
  mutate(calls_per_day = n())%>%
  select(month, day, calls_per_day) %>%
  distinct(.)
trend2$days<-as.numeric(row.names(trend2))

#June 
trend2<-mutate(trend2, Period=case_when(days<11~"Pre",  
                                      days>=10 & days<=18~"During", 
                                      days>18~"Post"))

#songbird png
song_url = "http://phylopic.org/assets/images/submissions/32b14b84-f53c-4558-a832-12ea41f56f29.256.png"
song = readPNG(getURLContent(song_url), 
               native = T)
song2 <- rasterGrob(song, interpolate = TRUE)

#need to re-order the x axis 
songbird_6species<- ggplot(trend2)+
  geom_jitter(mapping = aes(x=factor(Period, level=c("Pre", "During", "Post")), y=calls_per_day, colour=Period), 
              size=1, alpha=0.5, position=position_dodge(width=1))+
  geom_pointrange(mapping = aes(x=factor(Period, level=c("Pre", "During", "Post")), y=calls_per_day, colour=Period),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("violetred4", "indianred2","gray69"))+
  annotation_custom(song2, xmin=0.5, xmax=1.75, ymin=3000, ymax=3500)+
  theme_bw()+
  theme(legend.position="none")
  
ggsave(file="figures/maggie_proto/songbird6_fig.png", songbird_6species, width = 5, height = 4, dpi = 300)

  
#calls increase post... they report this as well.. 
  

####Meta-analysis data wrangling Katie re-do to be same format as others----
#Dec 16, 2022
##activity of 6 species of interest
data_sub_2<-subset(data_sub_1, common_name=="Olive-sided Flycatcher"|
                     common_name=="Pacific Wren" |
                     common_name=="Orange-crowned Warbler" |
                     common_name=="Ruby-crowned Kinglet"|
                     common_name=="Swainson's Thrush"|
                     common_name=="Western Wood-Pewee")%>%
  filter(confidence >= 0.125) %>%
  unite("date", year:day, sep = "-")#%>%mutate(doy=lubridate::yday(date))

#add pre/during/post heat dome categories
data_sub_2$date <- as.Date(data_sub_2$date)
summary(data_sub_2$date)
means <- mutate(data_sub_2, 
              heatdome = case_when(date < "2021-06-25" ~ "pre", 
                                   date <= "2021-07-02" & date >= "2021-06-25" ~ "during", 
                                   date > "2021-07-02" ~ "post"))
means$heatdome <- as.factor(means$heatdome)
ggplot(means) +
  geom_histogram(aes(x = date, fill = heatdome))# looks correct

# response variable: daily calls (now each row is number of call/day for a given species at a given site)
songbirds1 <- means %>%
  group_by(scientific_name, date, heatdome, site) %>%
  summarize(daily_calls = n())

# pivot_wider so each part of heat dome is own variable
songbirds2 <- songbirds1 %>%  
  pivot_wider(id_cols = c(scientific_name, date, site), names_from = heatdome, values_from = daily_calls) 

songbirds_during <- songbirds2 %>%  
  group_by(scientific_name) %>%
  summarize(response_id = 1,
            study_id = "songbirds",
           comparison_type = "temporal_during",
            response_def = "calls/day",
            response_units = "calls/day",
            region = "terrestrial",
            movement = "motile",
            mean_control = mean(pre, na.rm = T), # mean calls/day across sites and across days
            mean_treatment = mean(during, na.rm = T),
            var_control = sd(pre, na.rm = T),
            var_treatment = sd(during, na.rm = T),
            samp_control = sum(! is.na(pre)), # all non-nas in pre category 
            samp_treatment = sum(! is.na(during)),             
            dates_control = "2021-6-15 to 2021-6-24",
            date_treatment = "2021-6-25 to 2021-7-2") # number of days of observations during heat dome

songbirds_post <- songbirds2 %>%  
  group_by(scientific_name) %>%
  summarize(response_id = 1,
            study_id = "songbirds",
            comparison_type = "temporal_post",
            response_def = "calls/day",
            response_units = "calls/day",
            region = "terrestrial",
            movement = "motile",
            mean_control = mean(pre, na.rm = T), 
            mean_treatment = mean(post, na.rm = T),
            var_control = sd(pre, na.rm = T),
            var_treatment = sd(post, na.rm = T),
            samp_control = sum(! is.na(pre)), # all non-nas in pre category 
            samp_treatment = sum(! is.na(post)),    
            dates_control = "2021-6-15 to 2021-6-24",
            date_treatment = "2021-7-3 to 2021-7-8") # number of days of observations post heat dome

# combine 2 comparison types
songbirds_meta <- rbind(songbirds_during, songbirds_post)

# fix unique_species
songbirds_meta$unique_species <- plyr::revalue(songbirds_meta$scientific_name, c("Catharus ustulatus" = 1,
                                                                                 "Contopus cooperi" = 2,
                                                                                 "Contopus sordidulus" = 3,
                                                                                 "Leiothlypis celata" = 4,
                                                                                 "Regulus calendula" = 5,
                                                                                 "Troglodytes pacificus" = 6))
songbirds_meta$species <- songbirds_meta$scientific_name
songbirds_meta <- songbirds_meta[, -c(1)]
                                               
#save as Rdata
save(songbirds_meta, file="data/tidy_data/fig3/meta_analysis/songbirds_meta.Rdata")
