pacman::p_load(tidyverse, lme4, sf, rnaturalearth, rnaturalearthdata, viridis)

#read in all the files----
bcba <- read_csv("raw_data/surf scoter/historical_bcba.csv") %>% mutate(site="bcba")
bcbr <- read_csv("raw_data/surf scoter/historical_bcbr.csv") %>% mutate(site="bcbr")
bcci <- read_csv("raw_data/surf scoter/historical_bcci.csv") %>% mutate(site="bcci")
bcco <- read_csv("raw_data/surf scoter/historical_bcco.csv") %>% mutate(site="bcco")
bcdb <- read_csv("raw_data/surf scoter/historical_bcdb.csv") %>% mutate(site="bcdb")
bcde <- read_csv("raw_data/surf scoter/historical_bcde.csv") %>% mutate(site="bcde")
bcdu <- read_csv("raw_data/surf scoter/historical_bcdu.csv") %>% mutate(site="bcdu")
bcgs <- read_csv("raw_data/surf scoter/historical_bcgs.csv") %>% mutate(site="bcgs")
bche <- read_csv("raw_data/surf scoter/historical_bche.csv") %>% mutate(site="bche")
bcki <- read_csv("raw_data/surf scoter/historical_bcki.csv") %>% mutate(site="bcki")
bcla <- read_csv("raw_data/surf scoter/historical_bcla.csv") %>% mutate(site="bcla")
bclh <- read_csv("raw_data/surf scoter/historical_bclh.csv") %>% mutate(site="bclh")
bclq <- read_csv("raw_data/surf scoter/historical_bclq.csv") %>% mutate(site="bclq")
bcma <- read_csv("raw_data/surf scoter/historical_bcma.csv") %>% mutate(site="bcma")
bcnb <- read_csv("raw_data/surf scoter/historical_bcnb.csv") %>% mutate(site="bcnb")
bcnn <- read_csv("raw_data/surf scoter/historical_bcnn.csv") %>% mutate(site="bcnn")
bcpa <- read_csv("raw_data/surf scoter/historical_bcpa.csv") %>% mutate(site="bcpa")
bcpi <- read_csv("raw_data/surf scoter/historical_bcpi.csv") %>% mutate(site="bcpi")
bcpo <- read_csv("raw_data/surf scoter/historical_bcpo.csv") %>% mutate(site="bcpo")
bcpq <- read_csv("raw_data/surf scoter/historical_bcpq.csv") %>% mutate(site="bcpq")
bcrs <- read_csv("raw_data/surf scoter/historical_bcrs.csv") %>% mutate(site="bcrs")
bcsc <- read_csv("raw_data/surf scoter/historical_bcsc.csv") %>% mutate(site="bcsc")
bcsi <- read_csv("raw_data/surf scoter/historical_bcsi.csv") %>% mutate(site="bcsi")
bcso <- read_csv("raw_data/surf scoter/historical_bcso.csv") %>% mutate(site="bcso")
bcss <- read_csv("raw_data/surf scoter/historical_bcss.csv") %>% mutate(site="bcss")
bctl <- read_csv("raw_data/surf scoter/historical_bctl.csv") %>% mutate(site="bctl")
bcva <- read_csv("raw_data/surf scoter/historical_bcva.csv") %>% mutate(site="bcva") %>%
  rename(year_normal=normal_year)
bcvi <- read_csv("raw_data/surf scoter/historical_bcvi.csv") %>% mutate(site="bcvi")
bcwr <- read_csv("raw_data/surf scoter/historical_bcwr.csv") %>% mutate(site="bcwr")

tot_2021 <- read_csv("tidy_data/surf_scoter2021.csv") 

#turn them into one spreadsheet
hist_scoter <- rbind(bcba, bcbr, bcci, bcco,  bcdb, bcde, bcdu, bcgs, bche, bcki, 
                     bcla, bclh, bclq, bcma, bcnb, bcnn, bcpa, bcpi, bcpo, bcpq, 
                     bcrs, bcsc, bcsi, bcso, bcss, bctl, bcva, bcvi, bcwr) %>%
  rename(site_code = site, 
         species_code = SPECIES_CODE,
         count = NumberByPartyHours) %>%
  select(site_code, count, year_normal)
curr_scoter <- tot_2021 %>%
  rename(count = `count/hours`) %>%
  select(site_code, count, year_normal)
#the master dataset
#full_scoter <- rbind(hist_scoter, curr_scoter) %>%
#  mutate(heatdome = case_when(year_normal %in% 2021 ~ "yes", 
 #                   TRUE ~ "no"))

#write_csv(full_scoter, "full_scoter.csv")


#read in full dataset---- 
full_scoter<-read.csv("tidy_data/full_scoter.csv")

#lat lon coords for each site
coord <- tot_2021 %>%
  select(site_code, Lat, Lon)

hist_summary<- hist_scoter %>%
  filter(year_normal > 1995) %>%
  group_by(site_code) %>%
  summarise(sd=sd(count), 
            count=mean(count)) %>%
  mutate(year="historical") %>%
  pivot_longer(cols = c(sd, count), names_to = "stat", values_to = "value")

curr_summary <- tot_2021 %>%
  rename(value=Number_hours) %>%
  select(site_code, value) %>%
  mutate(year="2021", 
         stat="count")

comp <- rbind(hist_summary, curr_summary) %>%
  left_join(coord) 

mean_comp <- comp %>%
  filter(stat== "count") %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(diff=`2021` - `historical`) 

sd_comp <- comp %>%
  filter(stat=="sd") %>%
  select(site_code, stat, value)

delta_sd<- left_join(mean_comp, sd_comp, by="site_code") %>%
  rename(sd=value) %>%
  select(site_code, Lat, Lon, diff, sd) %>%
  mutate(delta_sd=diff/sd)

#plots

world <- ne_countries(scale = "medium", returnclass = "sf")

#difference in sd of counts 

ggplot(data=world)+
  geom_sf()+
  geom_point(data=delta_sd, aes(x=Lon, y=Lat, colour=delta_sd), size=3)+
  scale_colour_viridis_c()+
  coord_sf(xlim = c(-132.5,-122.5), ylim= c(47, 54.5))


#difference in mean counts 
ggplot(data=world)+
  geom_sf()+
  geom_point(data=mean_comp, aes(x=Lon, y=Lat, colour=diff), size=3)+
  scale_colour_viridis_c()+
  coord_sf(xlim = c(-132.5,-122.5), ylim= c(47, 54.5))+ theme_bw()+
  labs(colour="Change from historic")

ggsave("figures/surfscoter_deltacts.png")

ggplot(data=world)+
  geom_sf()+
  geom_point(data=delta_sd, aes(x=Lon, y=Lat, colour=delta_sd<0), size=3)+
  scale_colour_viridis(option="D" , discrete=TRUE)+
  coord_sf(xlim = c(-132.5,-122.5), ylim= c(47, 54.5))

ggsave("figures/surfscoter_deltasd_binary.png")

#make a boxplot version 
comp<-mutate(comp, year=factor(year,levels=c("historical", "2021")))
ggplot(comp)+ 
  geom_boxplot(aes(x=year, y=log(value), fill=year))+
  #geom_boxplot(width=0.5)+
  #geom_jitter(shape=21,position=position_jitter(0.1),size=3) +
  #xlab("Pre, during, or post Heat Dome") +
  ylab("survey counts (log)") + xlab(" ")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  scale_x_discrete(labels=c("historical" = "Pre", "2021" = "Post"))+
  scale_fill_manual(values = c("#FFCCBC", "#FF7043"))


#maggie proto figure 

pacman::p_load(png, grid, RCurl, rphylopic, patchwork, gridExtra)

library(RCurl)

#tamiasciurus hudsonicus
bird_url = "http://phylopic.org/assets/images/submissions/23c63b97-e0ab-4b43-89a4-e2358f4f09ec.256.png"
bird = readPNG(getURLContent(bird_url), 
               native = T)
bird2 <- rasterGrob(bird, interpolate = TRUE)

surf_scoter <- comp %>%
  filter(stat=="count") %>%
  mutate(year_fig = case_when(year=="historical" ~ "2020", 
                              TRUE ~ "2021")) %>%
  ggplot()+
  geom_jitter(mapping = aes(x = year_fig, y = log(value), colour=year), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(mapping = aes(x = year_fig, y = log(value), colour=year),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("indianred2", "gray69"))+
  theme_bw()+
  annotation_custom(bird2, xmin=0.42, xmax=1, ymin=2.8, ymax=4.5)+
  theme(legend.position="none")+
  scale_x_discrete("",
                   labels = c(
                     "2020" = "pre",
                     "2021" = "post"))+
  ylab("Log transformed abundances")
  

ggsave(file="figures/maggie_proto/bird_fig.png", surf_scoter, width = 5, height = 4, dpi = 300)
  
################################################################################
##### META-ANALYSIS DATA FORMATTING -----------------------
################################################################################
# Katie Goodwin Dec 6, 2022
library(tidyverse)

mean_comp$y2021 <- mean_comp$'2021' # to treat 2021 as a variable not a number

scoter_meta <- mean_comp %>% 
  summarize(response_id = 1,
            unique_species = 1,
            species = "Melanitta perspicillata",
            study_id = "scoter",
            comparison_type = "temporal_post",
            response_def = "log survey counts",
            response_units = "log counts",
            region = "marine",
            movement = "motile",
            mean_control = mean(log(historical)),
            mean_treatment = mean(log(y2021)),
            var_control = sd(log(historical)),
            var_treatment = sd(log(y2021)),
            samp_control = n(),
            samp_treatment = n(),
            dates_control = "mean 1986 to 2020",
            date_treatment = "2021")

save(scoter_meta, file="./tidy_data/meta_analysis/scoter_meta.RData")










