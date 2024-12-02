## Sandra Emry 
## May 4, 2022
## Mytilus trossolus mortality at Porteau Cove with and without fucus during the heat dome

library(tidyverse)
library(here)

# load & clean data -------------------------------------------------------

fucus_cover_mytilus_raw <- read_csv(here("data/tidy_data/fig3", "mytilus_mortality.csv"))

# calculate percent mortality
fucus_cover_mytilus <- fucus_cover_mytilus_raw %>% 
  mutate(mort = dead_Myt / (dead_Myt + live_Myt)*100) %>% 
  mutate(survival = 100-mort) %>% 
  mutate(fucus = factor(fucus, levels = c("absent", "present"))) %>% 
  select(boulder_id, fucus, fucus_cover, mort, survival, dead_Myt, live_Myt) %>% 
  filter(boulder_id != "B6a") ## Taking out observations on boulder 6a because it was north facing and is very different than the other 9 observations, and therefore not comparable. 

# Plot --------------------------------------------------------------------

mussel_mort <- ggplot(data = fucus_cover_mytilus) + 
  geom_boxplot(aes(x = fucus, y = mort, fill = fucus)) +
  labs(x = expression(paste(italic("F. distichus"), " presence")), 
       y = expression(paste(italic("M. trossulus "), "mortality (%)"))) +
  scale_fill_manual(values = c("snow3", "chartreuse4")) + 
  theme_classic() + 
  theme(legend.position = "none")

ggsave("figures_tables/raw figures/fig3_mytilus_mortality_fucus_presence.jpg", plot = mussel_mort)


# reformat for meta-analysis ----------------------------------------------

mytilus_meta <- fucus_cover_mytilus %>% 
  mutate(treatment = case_when(fucus == "present" ~ "control", 
         TRUE ~ "treatment")) %>% 
  group_by(treatment) %>% 
  add_tally() %>%
  rename(samp = n) %>% 
  mutate(mean = mean(survival),
            var = sd(survival)) %>% 
  select(treatment, mean, var, samp) %>% 
  distinct(.keep_all = TRUE) %>% 
  pivot_wider(names_from = c(treatment),
              values_from = c(mean, var, samp)) %>% 
  mutate(response_id = 1) %>% 
  mutate(unique_species = 1) %>% 
  mutate(study_id = "Mytilus_under_Fucus_Harley") %>% 
  mutate(species = "Mytilus trossulus") %>% 
  mutate(comparison_type = "spatial") %>% 
  mutate(response_def = "survival") %>% 
  mutate(response_units = "percent") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "sessile") %>% 
  relocate(., mean_treatment:samp_control, .after = movement)  

mytilus_meta$dates_control <- fucus_cover_mytilus_raw %>% 
  distinct(date)

mytilus_meta$dates_treatment <- fucus_cover_mytilus_raw %>% 
  distinct(date)

save(mytilus_meta, file="data/tidy_data/fig3/meta_analysis/mytilus_meta.Rdata")

  
# Maggie proto figure  ----------------------------------------------------


pacman::p_load(png, grid, RCurl, rphylopic, patchwork, gridExtra)

library(RCurl)

#tamiasciurus hudsonicus
myt_url = "http://phylopic.org/assets/images/submissions/61e2982f-07d4-44db-ac00-27a2d86ca855.128.png"
myt = readPNG(getURLContent(myt_url), 
               native = T)
myt2 <- rasterGrob(myt, interpolate = TRUE)

myt_plot<- fucus_cover_mytilus %>%
  ggplot()+
  geom_jitter(mapping = aes(x = fucus, y = mort), size=1, alpha=0.5, width=0.05, colour="violetred4")+
  geom_pointrange(mapping = aes(x = fucus, y = mort), colour="violetred4",
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  theme_bw()+
  annotation_custom(myt2, xmin=0.4, xmax=0.9, ymin=55, ymax=100)+
  theme(legend.position="none")+
  labs(x = expression(paste(italic("F. distichus"), " presence")), 
       y = expression(paste(italic("M. trossulus "), "mortality (%)")))

ggsave(file="figures/maggie_proto/myt_fig.png", myt_plot, width = 7, height = 5, dpi = 300)

