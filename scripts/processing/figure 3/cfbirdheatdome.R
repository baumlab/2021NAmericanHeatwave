pacman::p_load(tidyverse, lme4)

birds <- read_csv("tidy_data/cf_bird_data.csv")
#filtering and selecting the right dataframe
birds_tidy <- birds %>%
  filter(year != "2022") %>% 
  filter(month %in% c("may", "june", "july")) %>%
  mutate(heatdome = case_when(month %in% c("may") ~ "before", 
                              month %in% c("june") ~ "during", 
                              month %in% c("july") ~ "after"))
birds_summary <- birds_tidy %>%
  group_by(heatdome) %>%
  summarise(num_indiv_corr = number_individual_birds/number_of_surveys, 
            mean_num_indiv_corr = mean(num_indiv_corr),
            sd_indiv = sd(num_indiv_corr), 
            num_species_corr = number_of_species/number_of_surveys, 
            mean_num_species_corr = mean(num_species_corr),
            sd_species = sd(num_species_corr))
#plotting species and individuals 

  ggplot(aes(), 
             y=mean_num_indiv_corr, ymax=mean_num_indiv_corr+sd_indiv, ymin=mean_num_indiv_corr-sd_indiv))+
  geom_point(size=12)+
  geom_linerange(position=position_dodge(width = .75), size=1)

ggplot()+
  geom_jitter(birds_summary, mapping = aes(x=heatdome, y = num_indiv_corr, 
                                     colour=heatdome), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(birds_summary, mapping = aes(x = heatdome, 
                                           y = num_indiv_corr, colour=heatdome),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(rang2, xmin=0.5, xmax=1.1, ymin=0.0415, ymax=0.067)+
  theme(legend.position="none")+
  xlab("")+
  ylab("number corrected individuals")



ggsave("cfbirds_indiv.png")

birds_summary %>%
  ggplot(aes(x=factor(heatdome, level = c('before', 'during', 'after')), 
             y=mean_num_species_corr, ymax=mean_num_species_corr+sd_species, ymin=mean_num_species_corr-sd_species))+
  geom_point(size=12)+
  geom_linerange(position=position_dodge(width = .75), size=1)
  
ggsave("cfbirds_species.png")

mod_indiv <- lmer(number_individual_birds ~ heatdome + (1|id), data=birds_tidy)
mod_species <- lmer(number_of_species ~ heatdome + (1|id), data=birds_tidy)
summary(mod_species)

