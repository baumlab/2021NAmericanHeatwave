## Sandra Emry 
## May 4, 2022
## Nucella lamellosa mortality at 1001 Steps from 2011-2021

library(tidyverse)
library(here)
library(janitor)
library(lubridate)

nucella <- read_csv(here("tidy_data", "nucella_mortality.csv")) %>% 
  clean_names()

nucella <- nucella %>% 
  mutate(date = dmy(date)) %>% 
  mutate(year = year(date)) %>% 
  mutate(shore_level = factor(shore_level, levels = c("very_low", "low", "mid"))) %>% 
  mutate(treatment = case_when(
    year %in% c("2011", "2012", "2013", "2014") ~ "control",
    year == "2021" ~ "treatment")) %>% 
  mutate(percent_survival = 100 - percent_dead)

## proportion of empty shells relative to historical means [2011-14 dead/total vs. 2021 dead/total]
## sites: Blue Heron and 1001 Steps, Waterloo

nucella_mort_pre_vs_post <- nucella %>% 
  filter((site == "1001_Steps" & shore_level == "low") | (site == "Blue_Heron")) %>% 
  group_by(site, time_period) %>% 
  summarise(avg_surv = mean(percent_survival), sd_surv = sd(percent_survival)) %>% 
  ggplot(data = .) + 
  geom_boxplot(aes(x = time_period, y = avg_mort, fill = site)) +
  labs(y = "mean survival (%)") + 
  theme_classic() 

ggsave("./figures/nucella_mort_pre_vs_post.jpg", plot = nucella_mort_pre_vs_post)

##  proportion of snails that were deemed to have died during the heat dome 
## [2021 (recent_empty + recent_hermit) / (live + recent_empty + recent_hermit)
## sites: 1001 Steps, Waterloo, Cedar, Blue Heron 

# # plot by shore height
# mort_from_HD <- nucella %>% 
#   filter(!is.na(percent_recent_dead)) %>% 
#   ggplot(data = .) + 
#   geom_boxplot(aes(x = shore_level, y = percent_recent_dead)) + 
#   geom_point(aes(x = shore_level, y = percent_recent_dead, color = site)) + 
#   theme_classic() + 
#   labs(x = "intertidal height", y = "mortality due to 2021 Heat Dome") + 
#   scale_x_discrete(breaks=c("very_low","low","mid"),
#                    labels=c("very low", "low", "mid"))
# 
# ggsave("./figures/nucella_mort_by_HD.jpg", plot = mort_from_HD)



# Formatting data for meta-analysis -----------------------------------------

nucella_meta <- nucella %>% 
  filter((site == "1001_Steps" & shore_level == "low") | (site == "Blue_Heron")) %>%
  group_by(treatment) %>% 
  add_tally() %>% 
  mutate(mean = mean(percent_survival),
            var = sd(percent_survival)) %>% 
  select(treatment, mean, var, n) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment),
              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 1) %>% 
  mutate(unique_species = 1) %>% 
  mutate(study_id = "Nucella_Harley") %>% 
  mutate(species = "Nucella lamellosa") %>% 
  mutate(comparison_type = "temporal_post") %>% 
  mutate(response_def = "survival") %>% 
  mutate(response_units = "percent") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "motile") %>% 
  relocate(., mean_control:samp_treatment, .after = movement) %>% 
  ungroup() 

nucella_meta$dates_control <- nucella %>% 
  filter(year != 2021) %>% 
  distinct(date) %>% 
  mutate(date = as.character(date)) %>% 
  as.vector()

nucella_meta$dates_treatment <- nucella %>% 
  filter(year == 2021) %>% 
  distinct(date) %>% 
  mutate(date = as.character(date)) %>% 
  as.vector()

save(nucella_meta, file="./tidy_data/meta_analysis/nucella_meta.Rdata")
  
  
