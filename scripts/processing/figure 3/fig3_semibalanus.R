### Creating metadata for Hesketh & Harley 2022 Semibalanus data 

library(tidyverse)

treatments <- read_csv("./raw_data/semibalanus/SBHW_SHADE_PlotInfo.csv")

treatments <- treatments %>% 
  select(plot_number, treatment_final_numeric) %>% 
  filter(treatment_final_numeric %in% c(1, 3)) #  1 = unshaded intact, 3 = shaded intact (based on metadata file)
#  1 = unshaded intact, 2 = shaded intact (based on Amelia's email response)
 
treatments$treatment_categorical[treatments$treatment_final_numeric == 1] <- "treatment"
treatments$treatment_categorical[treatments$treatment_final_numeric == 3] <- "control"

mort <- read_csv("./raw_data/semibalanus/SBHW_SHADE_BarnacleMort.csv")

mort <- left_join(mort, treatments, by = "plot_number")

mort <- mort %>%
  filter(!is.na(treatment_categorical))

survival <- mort %>% 
  filter(number_live + number_dead > 0) %>% 
  mutate(percent_survival = (number_live / (number_live + number_dead))*100) 

## calculating mean & variance
semibalanus_meta <- survival %>% 
  group_by(treatment_categorical) %>% 
  add_tally() %>% 
  mutate(mean = mean(percent_survival),
         var = sd(percent_survival)) %>% 
  select(treatment_categorical, mean, var, n) %>% 
  distinct(.keep_all = TRUE) %>% 
  dplyr::rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment_categorical),
              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 2) %>% 
  mutate(unique_species = 1) %>% 
  mutate(study_id = "Hesketh_Harley_2022") %>% 
  mutate(comparison_type = "spatial") %>%
  mutate(response_def = "survival") %>% 
  mutate(response_units = "percent") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "sessile") %>% 
  mutate(species = "Semibalanus cariosus") %>% 
  relocate(., mean_treatment:samp_control, .after = movement) 


# Barnacle recruits  ------------------------------------------------------

recruits <- read_csv("./raw_data/semibalanus/SBHW_SHADE_surveys.csv")

# Sent an email to Amelia about this data, but for now, I'm only going to look at recruitment of SECA
seca_recruits <- recruits %>% 
  filter(treatment_code %in% c("UR", "SR")) %>% # filter to only the removed plots
  filter(final_code == "SECA") %>%  # filter to only SECA barnacle recruits 
  mutate(treatment_code = case_when(treatment_code == "UR" ~ "treatment",
                                    TRUE ~ "control"))
bagl_recruits <- recruits %>% 
  filter(date != "2021-04-14") %>%  #take out the first date where everything was removed
  filter(treatment_code %in% c("UR", "SR")) %>% # filter to only the removed plots
  filter(final_code == "BAGL")  %>% # filter to only SECA barnacle recruits 
  mutate(treatment_code = case_when(treatment_code == "UR" ~ "treatment",
                                    TRUE ~ "control"))
chda_recruits <- recruits %>% 
  filter(date != "2021-04-14") %>%  #take out the first date where everything was removed
  filter(treatment_code %in% c("UR", "SR")) %>% # filter to only the removed plots
  filter(final_code == "CHDA")  %>% # filter to only SECA barnacle recruits 
  mutate(treatment_code = case_when(treatment_code == "UR" ~ "treatment",
                                    TRUE ~ "control"))

barnacle_recruits <- bind_rows(seca_recruits, bagl_recruits, chda_recruits) %>% 
  select(-block, -date) %>% 
  pivot_wider(names_from = final_code, values_from = abund) %>% 
  mutate(total_abund = SECA + BAGL + CHDA) 
# 
# ggplot(data = seca_recruits) + geom_boxplot(aes(x = treatment_code, y = abund))
# ggplot(data = bagl_recruits) + geom_boxplot(aes(x = treatment_code, y = abund))
# ggplot(data = chda_recruits) + geom_boxplot(aes(x = treatment_code, y = abund))
# 
# ggplot(data = barnacle_recruits) + geom_boxplot(aes(x = treatment_code, y = total_abund))


## CHDA: calculating mean & variance
chda_meta <- chda_recruits %>% 
  group_by(treatment_code) %>% 
  add_tally() %>% 
  mutate(mean = mean(abund),
         var = sd(abund)) %>% 
  select(treatment_code, mean, var, n) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment_code),
              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 1) %>% 
  mutate(unique_species = 3) %>% 
  mutate(study_id = "Hesketh_Harley_2022") %>% 
  mutate(comparison_type = "spatial") %>%
  mutate(response_def = "abundance") %>% 
  mutate(response_units = "raw counts") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "sessile") %>% 
  mutate(species = "Chthamalus dalli") %>% 
  relocate(., mean_treatment:samp_control, .after = movement) 

## BAGL: calculating mean & variance
bagl_meta <- bagl_recruits %>% 
  group_by(treatment_code) %>% 
  add_tally() %>% 
  mutate(mean = mean(abund),
         var = sd(abund)) %>% 
  select(treatment_code, mean, var, n) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment_code),
              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 1) %>% 
  mutate(unique_species = 2) %>% 
  mutate(study_id = "Hesketh_Harley_2022") %>% 
  mutate(comparison_type = "spatial") %>%
  mutate(response_def = "abundance") %>% 
  mutate(response_units = "raw counts") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "sessile") %>% 
  mutate(species = "Balanus glandula") %>% 
  relocate(., mean_treatment:samp_control, .after = movement) 

## SECA: calculating mean & variance
seca_meta <- seca_recruits %>% 
  group_by(treatment_code) %>% 
  add_tally() %>% 
  mutate(mean = mean(abund),
         var = sd(abund)) %>% 
  select(treatment_code, mean, var, n) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(samp = n) %>% 
  pivot_wider(names_from = c(treatment_code),
              values_from = c(mean, var, samp)) %>%
  mutate(response_id = 1) %>% 
  mutate(unique_species = 1) %>% 
  mutate(study_id = "Hesketh_Harley_2022") %>% 
  mutate(comparison_type = "spatial") %>%
  mutate(response_def = "abundance") %>% 
  mutate(response_units = "raw counts") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "sessile") %>% 
  mutate(species = "Semibalanus cariosus") %>% 
  relocate(., mean_treatment:samp_control, .after = movement) 

semibalanus_meta <- bind_rows(semibalanus_meta, bagl_meta, chda_meta, seca_meta)

save(semibalanus_meta, file="./tidy_data/meta_analysis/semibalanus_meta.Rdata")
