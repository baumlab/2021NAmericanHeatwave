# Sandra Emry, August 10th, 2023
# combining all meta-analysis data sets to be used in 'meta_analysis.R' script

library(tidyverse)

load("./data/tidy_data/fig3/meta_analysis/aphids_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/balanus_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/bat_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/hawks_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/honeybee_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/interiorshrub_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/lupine_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/mammals_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/mytilus_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/nucella_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/phyllospadix_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/scoter_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/semibalanus_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/songbirds_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/algae_meta.Rdata")
load("./data/tidy_data/fig3/meta_analysis/interiorshrubMAS_meta.RData")



algae_meta <- algae_meta %>% 
  mutate(response_id = as.numeric(response_id),
         unique_species = as.numeric(unique_species))%>%
  mutate(larger_group = case_when(species == "surfgrass" ~ "plants",
                                  TRUE ~ "algae"))
algae_meta$species[algae_meta$species == "surfgrass"] <- "phyllospadix"

mammals_meta <- mammals_meta %>%
  mutate(response_id = as.numeric(response_id),
         unique_species = as.numeric(unique_species))%>%
  select(-dates_control, -dates_treatment) %>% 
  mutate(larger_group = "mammals")

hawks_meta <- hawks_meta %>%
  mutate(response_id = as.numeric(response_id),
         unique_species = as.numeric(unique_species))%>%
  select(-dates_control, -dates_treatment) %>% 
  mutate(larger_group = "birds")

aphids_meta <- aphids_meta %>%
  mutate(response_id = as.numeric(response_id),
         unique_species = as.numeric(unique_species))%>%
  select(-dates_control, -dates_treatment) %>% 
  mutate(larger_group = "insects") %>% 
  mutate(species = case_when(crop == "blueberry" ~ "blueberry aphid", 
                             crop == "raspberry" ~ "raspberry aphid"))
  
scoter_meta <- scoter_meta %>%
  mutate(response_id = as.numeric(response_id),
         unique_species = as.numeric(unique_species)) %>%
  select(-dates_control, -date_treatment) %>% 
  mutate(larger_group = "birds")

songbirds_meta <- songbirds_meta %>%
  mutate(response_id = as.numeric(response_id),
         unique_species = as.numeric(unique_species))%>%
  select(-dates_control, -date_treatment) %>% 
  mutate(larger_group = "birds")

phyllo_meta <- phyllo_meta %>%
  select(-dates_control, -dates_treatment) %>% 
  mutate(larger_group = "plants")

nucella_meta <- nucella_meta %>%
  select(-dates_control, -dates_treatment) %>% 
  mutate(larger_group = "marine invertebrates")

balanus_meta <- balanus_meta %>%
  select(-dates_control, -dates_treatment) %>% 
  mutate(larger_group = "marine invertebrates")

mytilus_meta <- mytilus_meta %>%
  select(-dates_control, -dates_treatment) %>% 
  mutate(larger_group = "marine invertebrates")

lupine_meta <- lupine_meta %>%
  select(-dates_control, -date_treatment) %>% 
  mutate(larger_group = "plants")

shrub_meta <- shrub_meta %>%
  #select(-dates_control, -date_treatment) %>% 
  mutate(larger_group = "plants")

bat_meta <- bat_meta %>%
  select(-dates_control, -dates_treatment) %>% 
  mutate(larger_group = "mammals")

semibalanus_meta <- semibalanus_meta %>% 
  mutate(larger_group = "marine invertebrates")

hive_meta <- hive_meta %>% 
  mutate(larger_group = "insects")

heatdome <- bind_rows(phyllo_meta, nucella_meta, balanus_meta, mytilus_meta, lupine_meta,
                      shrub_meta, aphids_meta, bat_meta, hawks_meta, hive_meta,
                      scoter_meta, songbirds_meta, mammals_meta, semibalanus_meta, algae_meta) %>% 
  dplyr::rename(realm = region) %>%
  mutate(response_type = case_when(response_def %in% c("proportion_plant_tips_with_aphids", "abundance", "recruitment", "shoot density", "log survey counts", "survival") ~ "abundance/survival", 
                                   response_def %in% c("number_of_bat_calls", "calls/day", "daily detections", "average daily distance km") ~ "behaviour", 
                                   response_def == c("hive growth rate") ~ "growth",
                                   response_def == c("proportion plants not 100% scorched") ~ "damage",
                                   response_def %in% c("flowering shoot density", "log fruit count") ~ "reproduction"), 
         response_type_broad = case_when(response_type %in% c("abundance", "survival", "reproduction", "growth") ~ "direct", 
                                   response_type %in% c("damage", "behaviour") ~ "indirect")) %>% 
  relocate(response_type, .after = response_def) 







# joining the two species columns 
heatdome$species[which(is.na(heatdome$species))]<- heatdome$Species[which(is.na(heatdome$species))]
heatdome <-  select(heatdome, -Species)
heatdome <- heatdome %>% 
  mutate(species = case_when(study_id == "Bat_UBC_Farm" ~ "bat",
                             study_id == "Hawks_Bayne_2022" ~ "hawk",
                             study_id == "honeybees" ~ "honeybees",
                             .default = species))

write_csv(heatdome, "./heatdome_meta.csv")
