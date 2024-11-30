# Load libraries 
library(tidyverse)
library(here)

# Input here the folder path to input data
input_folder <- 
  here::here("data/raw_data/Table2")

inverts21 <-
  readr::read_csv(paste0(input_folder,
                         "/inverts_clean_2021.csv"))
inverts22 <-
  readr::read_csv(paste0(input_folder,
                         "/treehole_2022.csv"))

### 2021 data ####

species_df21 <- 
  inverts21 %>% 
  filter(!grepl('Pupa', species)) %>% 
  select(Treehole.Number,species,
         abundance) %>% 
  group_by(Treehole.Number, species) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  summarise(count = sum(abundance)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = species, values_from = count) %>%
  rename(treehole_id = "Treehole.Number")

dead21 <- 
  inverts21 %>% 
  filter(!grepl('Pupa', species)) %>% 
  filter(status=="dead") %>% 
  mutate(treehole_id = as.character(Treehole.Number),
         abundance = as.numeric(abundance)) %>% 
  select(treehole_id,abundance) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  group_by(treehole_id) %>%
  summarise(dead = sum(abundance)) %>% 
  ungroup() %>% 
  mutate_all(~replace_na(.,0)) 

abundance21 <- 
  species_df21 %>% 
  rowwise(treehole_id) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  mutate(abundance = sum(c_across(Ceratopogonid.A:Empidid.A))) %>% 
  select(treehole_id, abundance) %>% 
  mutate(treehole_id = as.character(treehole_id)) 

mortality21<- abundance21 %>% 
  full_join(dead21) %>% 
  mutate(treehole_id= paste("2021", treehole_id, sep = "_"),
         year = as.character("2021")) %>% 
  mutate_all(~replace_na(.,0)) 

## 2022 data


mortality22 <- 
  inverts22 %>% 
  filter(!grepl('Pupa', Morphospecies)) %>% 
  select("Treehole Number",Morphospecies,
         Amount) %>%
  rename(treehole_id = "Treehole Number",
         species = Morphospecies) %>% 
  mutate_all(~replace_na(.,0)) %>% 
  group_by(treehole_id) %>% 
  summarise(abundance = sum(Amount),
            dead = sum(Amount[which(species=="Dead")]),
            year = as.character("2022")) %>% 
  ungroup() %>% 
  filter(!grepl('G', treehole_id)) #remove ground holes

#combine

mortality <- mortality21 %>% 
  full_join(mortality22) %>% 
  mutate(mortality = dead/abundance) %>% 
  filter(!is.nan(mortality)) #for cases where no insects in treehole

plot(mortality$year, sqrt(mortality$mortality))

mean(mortality$mortality[which(mortality$year=="2021")]) #0.0375
mean(mortality$mortality[which(mortality$year=="2022")]) #0.0805


#=====output==========

write_csv(mortality, "figures_tables/Table2/treehole_invertebrate_mortality.csv")
