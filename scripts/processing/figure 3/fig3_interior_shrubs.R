################################################################################
################################################################################
###                                                                          ###
###                         HEAT DOME WORKING GROUP                          ###
###                         INTERIOR SHRUBS PLOT                             ###
###                                                                          ###
################################################################################
################################################################################

# Script created: Sep 23, 2022
# Last Updated Sep 23, 2022
# Script Authors: Katie Goodwin, 
# Data Authors: Sarah Dickson-Hoyle, Lori Daniels, Bert Williams

# Figure to demonstrate the effects of the heat dome on leaf scorch of 2 culturally significant shrubs in Interior BC

# Contact for info on data collection: sarah.dickson-hoyle@ubc.ca

################################################################################
##### INPUT DATA AND CALL PACKAGES ---------------------------------------------
################################################################################
pacman::p_load(tidyverse)


shrub_full <- read_csv("data/tidy_data/fig3/shrubdata.csv") %>%
  filter(transect_no != "6 (2017 high severity)") 


shrub1 <- shrubs %>%
  filter(transect_no != "6 (2017 high severity)") %>%
  pivot_longer(cols = starts_with("X"), names_to = "id", values_to = "pres_scorch") %>%
  mutate(not_full_scorch = ifelse(pres_scorch > 0, "0", "1"))
shrub1$not_full_scorch <- as.numeric(shrub1$not_full_scorch) # new variable full scorch to make heat dome effect correct direction
# NOTE not_full_scorch can = 1 and still be some scorch on the plant!!!!!

# subset to four different rows for final meta-analysis (2 species and 2 ecosystems)
rose <- shrub_full %>% 
  group_by(ecosystem, plant) %>% 
  filter(plant == "prickly rose") %>%
  mutate(trt = case_when(transect_no %in% c("4 (IDF unburned)", "5 (2017 low severity)") ~ "control", 
                         transect_no %in% c("1 (2017 unburned)", "2 (2017 low severity)") ~ "trt")) %>%
  select(date, ecosystem, transect_no,plant, prop_full_scorch, trt) %>%
  filter(date %in% c("2021-08-20", "2021-08-23"))

sask <- shrub_full %>% 
  group_by(ecosystem, plant) %>%
  filter(plant == "saskatoon") %>%
  mutate(trt = case_when(transect_no %in% c("4 (IDF unburned)", "5 (2017 low severity)") ~ "control", 
                         transect_no %in% c("1 (2017 unburned)", "2 (2017 low severity)") ~ "trt")) %>%
  select(date, ecosystem, transect_no,plant, prop_full_scorch, trt) %>%
  filter(date %in% c("2021-08-20", "2021-08-23"))



#impact == both ponderosa pine sites (unburned and low severity)
#control == interior douglas-fir (unburned and low severity)

# putting data frames in correct format (no longer considering ecosystem since no unburn there)

# prickly rose
rose_meta1 <- rose %>% 
  mutate(prop_not_scorched = 1 - prop_full_scorch) %>%
  select(-prop_full_scorch) %>%
  pivot_wider(names_from = c(trt), values_from = c(prop_not_scorched)) %>%
  filter(ecosystem != "montane (interior Douglas-fir)") %>% #since all the controls were 0, i replaced the NAs with 0 and sliced off the IDF rows (since these were controls)
  replace(is.na(.), 1) 
  
rose_meta <- rose_meta1 %>%  
  summarize(response_id = 1,
            unique_species = 1,
            study_id = "interior_shrubs",
            species = "rose",
            comparison_type = "spatial",
            response_def = "proportion plants not 100% scorched",
            response_units = "proportion",
            region = "terrestrial",
            movement = "sessile",
            mean_control = mean(control), # 1- mean is the proportion of plants not 100% scorched
            mean_treatment = mean(trt),
            var_control = sd(control),
            var_treatment = sd(trt),
            samp_control = 40,
            samp_treatment = 40)

# saskatoon berry
sask_meta1 <- sask %>% 
  mutate(prop_not_scorched = 1 - prop_full_scorch) %>%
  select(-prop_full_scorch) %>%
  mutate(samp_size=20) %>%
  pivot_wider(names_from = c(trt), values_from = c(prop_not_scorched)) %>%
  filter(ecosystem != "montane (interior Douglas-fir)") %>% #since all the controls were 0, i replaced the NAs with 0 and sliced off the IDF rows (since these were controls)
  replace(is.na(.), 1) 

sask_meta <- sask_meta1 %>%  
  summarize(response_id = 1,
            unique_species = 1,
            study_id = "interior_shrubs",
            species = "rose",
            comparison_type = "spatial",
            response_def = "proportion plants not 100% scorched",
            response_units = "proportion",
            region = "terrestrial",
            movement = "sessile",
            mean_control = mean(control), # 1- mean is the proportion of plants not 100% scorched
            mean_treatment = mean(trt),
            var_control = sd(control),
            var_treatment = sd(trt),
            samp_control = 40,
            samp_treatment = 40)



# combine 2 species
shrub_meta <- rbind(rose_meta, sask_meta)

save(shrub_meta, file="data/tidy_data/fig3/meta_analysis/interiorshrubMAS_meta.RData")





