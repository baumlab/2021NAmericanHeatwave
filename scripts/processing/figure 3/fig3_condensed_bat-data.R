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


##########################################################
## Michelle Tseng revisiting the bat figure ## 
## need a 'pre-post' figure or a 'pre-during-post' figure 
## Sept 22, 2022

library(tidyverse)

bat <- read.csv("data/tidy_data/fig3/bat.2020.2021.short.csv",
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

z1 <- glm(abundance ~ Period*Year, family = poisson(link="log"), data=bat.filter.total)
z2 <- glm(abundance ~ Period*Year, family = quasipoisson(link="log"), data=bat.filter.total)

summary(z1)
summary(z2)

anova(z1, test="Chisq")
anova(z2, test="F")


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
           
