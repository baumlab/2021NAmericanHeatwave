# October 3, 2022
# Sandra Emry 
# working with Hakai phyllospadix data

library(tidyverse)


##################### Tidy Data provided by Hakai ########################

phyllo <- read_csv("./tidy_data/Hakai_surfgrass_density_flowering.csv")

# head(phyllo)

# calculate baseline shoot density and flowering density levels for August 
# pre is averaged across 2019 and 2020, averaged across 3 sites 
phyllo_sum <- phyllo %>% 
  mutate(month = factor(month, levels = c("May", "Jun", "Aug"))) %>% 
  mutate(treatment = case_when(year %in% c(2019, 2020) ~ "control",
                                 year == 2021 ~ "treatment")) %>% 
  filter(month == "Aug") %>% 
  group_by(treatment) %>% 
  summarise(avg_flowering_m2 = mean(flowering_shoots_m2, na.rm = T),
            avg_shoots_m2 = mean(shoots_m2)) 

phyllo_flower <- phyllo %>% 
  mutate(month = factor(month, levels = c("May", "Jun", "Aug"))) %>% 
  mutate(treatment = case_when(year %in% c(2019, 2020) ~ "control",
                                 year == 2021 ~ "treatment")) %>% 
  filter(month == "Aug") %>% 
  group_by(treatment) %>% 
  add_tally() %>% 
  mutate(mean = mean(flowering_shoots_m2, na.rm = T),
         var = sd(flowering_shoots_m2, na.rm = T)) %>% 
  select(treatment, mean, var, n) %>% ## select only August data to get rid of seasonality patterns
  distinct(.keep_all = TRUE) %>% 
  rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment),
                              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 1) %>% 
  mutate(unique_species = 1) %>% 
  mutate(study_id = "Phyllospadix_Calvert") %>% 
  mutate(species = "phyllospadix") %>% 
  mutate(comparison_type = "temporal_post") %>%
  mutate(response_def = "flowering shoot density") %>% 
  mutate(response_units = "flowering shoots/m2") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "sessile") %>% 
  relocate(., mean_control:samp_treatment, .after = movement) 
  
phyllo_shoots <- phyllo %>% 
  mutate(month = factor(month, levels = c("May", "Jun", "Aug"))) %>% 
  mutate(treatment = case_when(year %in% c(2019, 2020) ~ "control",
                                 year == 2021 ~ "treatment")) %>% 
  filter(month == "Aug") %>% ## select only August data to get rid of seasonality patterns
  group_by(treatment) %>% 
  add_tally() %>% 
  mutate(mean = mean(shoots_m2, na.rm = T),
         var = sd(shoots_m2, na.rm = T)) %>% 
  select(treatment, mean, var, n) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment),
              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 2) %>% 
  mutate(unique_species = 1) %>% 
  mutate(study_id = "Phyllospadix_Calvert") %>% 
  mutate(species = "phyllospadix") %>% 
  mutate(comparison_type = "temporal_post") %>%
  mutate(response_def = "shoot density") %>% 
  mutate(response_units = "shoots/m2") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "sessile") %>% 
  relocate(., mean_control:samp_treatment, .after = movement) 

phyllo_meta <- rbind(phyllo_flower, phyllo_shoots)

phyllo_meta$dates_control <- phyllo %>% 
  filter(year != "2021") %>% 
  filter(month == "Aug") %>% 
  unite(date, c(month, day, year), sep = "-") %>% 
  distinct(date) %>% 
  as.vector()

phyllo_meta$dates_treatment <- phyllo %>% 
  filter(year == "2021") %>% 
  filter(month == "Aug") %>% 
  unite(date, c(month, day, year), sep = "-") %>% 
  distinct(date) %>% 
  as.vector()
  
save(phyllo_meta, file="./tidy_data/meta_analysis/phyllospadix_meta.Rdata")




  
  
  
  
