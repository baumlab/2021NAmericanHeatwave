## Sandra Emry 
## May 6, 2022
## Balanus glandula mortality at Pruth Bay in naturally shaded vs sun plots

library(tidyverse)

#read in data
balanus_raw <- read_csv("tidy_data/balanus_mortality_pruth.csv")

balanus <- balanus_raw %>% 
  filter(!str_detect(site, "north")) %>% # omit the data from the north end of the beach, since they aren't paired with shade data and the character of the beach changes a bit between the center and the north end.
  separate(site, c("site", "exposure"), sep = 17, remove = FALSE) %>% #creating a sun/shade variable
  mutate(direction = case_when(str_detect(exposure, "south") ~ "south", 
                              TRUE ~ "mid")) %>% 
  mutate(exposure = case_when(str_detect(exposure, "sun") ~ "sun",
                              TRUE ~ "shade")) %>% 
  mutate(percent_mort = number_dead/(number_dead + number_alive)*100) %>% #calculate percent mortality
  mutate(percent_survival = 100 - percent_mort) %>% 
  mutate(direction = factor(direction, levels = c("south", "mid"))) 

balanus$site <- "pruth bay"

ggplot(data = balanus) + 
  geom_boxplot(aes(x = exposure, y = percent_mort, fill= exposure)) + 
  labs(x = "exposure", y =  "mortality (%)") +
  theme_classic() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  scale_fill_manual(values = c("#FFCCBC","#B71C1C", "#FF7043"))

balanus_pruth <- ggplot(data = balanus) + 
  geom_boxplot(aes(x = exposure, y = percent_mort, fill = direction)) + 
  scale_fill_manual(values = c("sandybrown", "olivedrab")) + 
  labs(x = "exposure", y = expression(paste(italic("Balanus glandula"), " mortality (%)"))) +
  theme_classic() 

ggsave("./figures/fig3_balanus_pruthbay_by_exposure.jpg", plot = balanus_pruth)

# Reformatting for meta-analysis ------------------------------------------

balanus_meta <- balanus %>% 
  select(site, exposure, percent_survival) %>% 
  mutate(treatment = case_when(exposure == "shade" ~ "control", 
                           TRUE ~ "treatment")) %>% 
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
  mutate(study_id = "Balanus_Exposure_Harley") %>% 
  mutate(species = "Balanus glandula") %>% 
  mutate(comparison_type = "spatial") %>% 
  mutate(response_def = "survival") %>% 
  mutate(response_units = "percent") %>% 
  mutate(region = "marine") %>% 
  mutate(movement = "sessile") %>% 
  relocate(., mean_control:samp_treatment, .after = movement) 


balanus_meta$dates_treatment <- balanus_raw %>% 
  distinct(date)

balanus_meta$dates_control <- balanus_raw %>% 
  distinct(date)

save(balanus_meta, file="./tidy_data/meta_analysis/balanus_meta.Rdata")

# maggie trying to harmonise visalisation ---------------------------------



pacman::p_load(png, grid, RCurl, rphylopic, patchwork, gridExtra)

library(RCurl)

#tamiasciurus hudsonicus
balanus_url = "http://phylopic.org/assets/images/submissions/feaab8ab-b887-4875-801c-5c94c4b5da4b.128.png"
balanus_png = readPNG(getURLContent(balanus_url), 
               native = T)
balanus2 <- rasterGrob(balanus_png, interpolate = TRUE)

maggie_balanus<- balanus %>%
  ggplot()+
  geom_jitter(mapping = aes(x = exposure, y = percent_mort, group=exposure), alpha=0.5, size=1, colour="violetred4", width=0.05, height=0.1)+
  geom_pointrange(mapping = aes(x = exposure, y = percent_mort), colour= "violetred4",
                  stat = "summary", size=1.2)+
  #scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(balanus2, xmin=0.3, xmax=1.1, ymin=60, ymax=100)+
  theme(legend.background = element_rect(colour="black", fill="white"),
                            legend.position=c(.9,.2))

#visualising
ggsave(file="figures/maggie_proto/balanus_fig.png", maggie_balanus, width = 5, height = 4, dpi = 300)





















