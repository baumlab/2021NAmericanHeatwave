# Sandra Emry, August 10, 2023
# reformatting data from Brian Timmer, Sam Starko (Baum lab)


#read in raw data
dat_raw <- read_csv("data/tidy_data/fig3/heatdome_seaweeds_baumlab.csv")

# head(dat_raw)

dat <- dat_raw %>% 
  dplyr::rename(Plot = Aux1, species = Classification) %>% 
  select(-...1) %>% 
  filter(Time != "early heatdome") %>% 
  filter(species %in% c("Acrosiphonia", "red", "Ulva", "Fucus", "kelp", "surfgrass")) %>% 
  pivot_wider(names_from = species, values_from = Count) %>% 
  group_by(Time) %>%
  add_tally() %>% 
  dplyr::mutate(across(Acrosiphonia:surfgrass, .fns = lst(mean, sd), .names = "{.fn}_{.col}")) %>% 
  select(Time, n:sd_surfgrass) %>% 
  distinct(.keep_all = TRUE) %>% 
  ungroup()

# Acrosiphonia ------------------------------------------------------------


# Acrosiphonia - during
acro_during <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "post heatdome") %>% 
  select(group, n:sd_Acrosiphonia) %>% 
  mutate(species = "acrosiphonia", 
         response_id = 1, 
         unique_species = 1,
         study_id = "Baum_Lab",
         comparison_type = "temporal_during",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_Acrosiphonia, 
                var = sd_Acrosiphonia, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

# Acrosiphonia - post
acro_post <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "late heatdome") %>% 
  select(group, n:sd_Acrosiphonia) %>% 
  mutate(species = "acrosiphonia", 
         response_id = 1, 
         unique_species = 1,
         study_id = "Baum_Lab",
         comparison_type = "temporal_post",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_Acrosiphonia, 
                var = sd_Acrosiphonia, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

acro <- rbind(acro_during, acro_post)

# Ulva ------------------------------------------------------------


# Ulva - during
ulva_during <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "post heatdome") %>% 
  select(group, n, mean_Ulva, sd_Ulva) %>% 
  mutate(species = "ulva", 
         response_id = 1, 
         unique_species = 2,
         study_id = "Baum_Lab",
         comparison_type = "temporal_during",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_Ulva, 
                var = sd_Ulva, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

# Ulva - post
ulva_post <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "late heatdome") %>% 
  select(group, n, mean_Ulva, sd_Ulva) %>% 
  mutate(species = "ulva", 
         response_id = 1, 
         unique_species = 2,
         study_id = "Baum_Lab",
         comparison_type = "temporal_post",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_Ulva, 
                var = sd_Ulva, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

ulva <- rbind(ulva_during, ulva_post)

# fucus ------------------------------------------------------------


# Fucus - during
fucus_during <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "post heatdome") %>% 
  select(group, n, mean_Fucus, sd_Fucus) %>% 
  mutate(species = "fucus", 
         response_id = 1, 
         unique_species = 3,
         study_id = "Baum_Lab",
         comparison_type = "temporal_during",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_Fucus, 
                var = sd_Fucus, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

# Fucus - post
fucus_post <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "late heatdome") %>% 
  select(group, n, mean_Fucus, sd_Fucus) %>% 
  mutate(species = "fucus", 
         response_id = 1, 
         unique_species = 3,
         study_id = "Baum_Lab",
         comparison_type = "temporal_post",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_Fucus, 
                var = sd_Fucus, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

fucus <- rbind(fucus_during, fucus_post)

# Kelp ------------------------------------------------------------


# kelp - during
kelp_during <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "post heatdome") %>% 
  select(group, n, mean_kelp, sd_kelp) %>% 
  mutate(species = "kelp", 
         response_id = 1, 
         unique_species = 4,
         study_id = "Baum_Lab",
         comparison_type = "temporal_during",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_kelp, 
                var = sd_kelp, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

# kelp - post
kelp_post <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "late heatdome") %>% 
  select(group, n, mean_kelp, sd_kelp) %>% 
  mutate(species = "kelp", 
         response_id = 1, 
         unique_species = 4,
         study_id = "Baum_Lab",
         comparison_type = "temporal_post",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_kelp, 
                var = sd_kelp, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

kelp <- rbind(kelp_during, kelp_post)

# surfgrass ------------------------------------------------------------


# surfgrass - during
surfgrass_during <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "post heatdome") %>% 
  select(group, n, mean_surfgrass, sd_surfgrass) %>% 
  mutate(species = "surfgrass", 
         response_id = 1, 
         unique_species = 5,
         study_id = "Baum_Lab",
         comparison_type = "temporal_during",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_surfgrass, 
                var = sd_surfgrass, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

# surfgrass - post
surfgrass_post <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "late heatdome") %>% 
  select(group, n, mean_surfgrass, sd_surfgrass) %>% 
  mutate(species = "surfgrass", 
         response_id = 1, 
         unique_species = 5,
         study_id = "Baum_Lab",
         comparison_type = "temporal_post",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_surfgrass, 
                var = sd_surfgrass, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

surfgrass <- rbind(surfgrass_during, surfgrass_post)

# red ------------------------------------------------------------


# red - during
red_during <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "post heatdome") %>% 
  select(group, n, mean_red, sd_red) %>% 
  mutate(species = "red", 
         response_id = 1, 
         unique_species = 6,
         study_id = "Baum_Lab",
         comparison_type = "temporal_during",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_red, 
                var = sd_red, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

# red - post
red_post <- dat %>% 
  mutate(group = case_when(Time == "before heatdome" ~ "control", 
                           TRUE ~ "treatment")) %>% 
  filter(Time != "late heatdome") %>% 
  select(group, n, mean_red, sd_red) %>% 
  mutate(species = "red", 
         response_id = 1, 
         unique_species = 6,
         study_id = "Baum_Lab",
         comparison_type = "temporal_post",
         response_def = "abundance",
         response_units = "percent_cover",
         region = "marine", 
         movement = "sessile") %>% 
  dplyr::rename(mean = mean_red, 
                var = sd_red, 
                samp = n) %>% 
  pivot_wider(names_from = group,
              values_from = c(mean, var, samp)) 

red <- rbind(red_during, red_post)


# Combing all species -----------------------------------------------------

algae_meta <- bind_rows(acro, fucus, ulva, red, kelp, surfgrass)

save(algae_meta, file="data/tidy_data/fig3/meta_analysis/algae_meta.Rdata")
