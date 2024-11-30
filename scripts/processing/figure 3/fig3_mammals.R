### Code for CT species responses to temp during the heat dome
# Katie Tjaden-McClement, Aug 29 2022

library(tidyverse)

itcha_daily_summary <- read.csv("tidy_data/ct_hd_temp_v_detections_fig_data.csv") %>%
  mutate(time_period = case_when(date %in% c("2021-06-25", "2021-06-26", 
                                          "2021-06-27", "2021-06-28", "2021-06-29",
                                          "2021-06-30", "2021-07-01", "2021-07-02") ~ "during", 
                              date %in% c("2021-06-17", "2021-06-18", 
                                          "2021-06-19", "2021-06-20", 
                                          "2021-06-21", "2021-06-22", 
                                          "2021-06-23", "2021-06-24") ~ "pre", 
                              TRUE ~ "post"), 
         time_period  = fct_relevel(time_period, c("pre", "during", "post"))) %>% 
  mutate(treat = if_else(time_period=="pre", "control", "treatment"))


ggplot(itcha_daily_summary, aes(x = date, y = mean_detections,
                                colour = mean_temp, group = Species)) +
  geom_vline(xintercept = c("2021-06-25", "2021-06-30"),
             colour = "black", linetype = "dashed") +
  geom_point(alpha = 0.6) +
  geom_line(alpha = 0.6) +
  viridis::scale_color_viridis(option = "inferno") +
  labs(x = "", y = "Mean # of daily detections per station",
       colour = "Mean \ntemperature") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,
                                   size = 7))  +
  facet_wrap(~Species)

#multipanel version- just take one spp 
# interesting dip for heat-sensitive moose (Alces alces) -Cole Burton
cols<- c("#FFCCBC","#F08080","#B71C1C", "#FF7043", "#FF5733",  '#FFA07A')

ggplot(subset(itcha_daily_summary,Species=="Alces alces"),
              aes(x = date, y = mean_detections,
                                colour = mean_temp, group = Species)) +
  geom_vline(xintercept = c("2021-06-25", "2021-06-30"),
             colour = "black", linetype = "dashed") +
  geom_point() +
  geom_line() +
  scale_colour_gradientn(colours = c("#FFCCBC","#B71C1C","#FF7043"))+
  #viridis:: scale_color_viridis(option = "rocket", begin=1, end=0.3)+
  labs(x = "", y = "Mean # of daily detections per station",
       colour = "Mean \ntemperature") +
  #facet_wrap(~Species)+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
         size = rel(1)), legend.key = element_blank(), #legend.title=element_blank(),
              panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,
                                   size = 7))  +
  annotate("text", label="Pre", x=1.5, y=0.1, size=5, fontface=2) +
  annotate("text", label="During", x=11, y=0.1, size=5, fontface=2)+
annotate("text", label="Post", x=15.5, y=0.1, size=5, fontface=2)

           
## Maggie playing around with standardising this time series 

pacman::p_load(png, grid, RCurl, rphylopic, patchwork, gridExtra)

library(RCurl)

#alces alces
alces_url = "http://phylopic.org/assets/images/submissions/1a20a65d-1342-4833-a9dd-1611b9fb383c.128.png"
alces = readPNG(getURLContent(alces_url), 
                native = T)
alces2 <- rasterGrob(alces, interpolate = TRUE)

a <- itcha_daily_summary %>%
  filter(Species == c("Alces alces")) %>%
  ggplot()+
  geom_jitter(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(alces2, xmin=0.5, xmax=1.1, ymin=0.06, ymax=0.1)+
  theme(legend.position="none")+
  xlab("")

#canis lupus 
canis_url = "http://phylopic.org/assets/images/submissions/e4e306cd-73b6-4ca3-a08c-753a856f7f12.128.png"
canis = readPNG(getURLContent(canis_url), 
                native = T)
canis2 <- rasterGrob(canis, interpolate = TRUE)
         
b <- itcha_daily_summary %>%
  filter(Species == c("Canis lupus")) %>%
  ggplot()+
  geom_jitter(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(canis2, xmin=0.5, xmax=1.1, ymin=0.03, ymax=0.046)+
  theme(legend.position="none")+
  xlab("")

#lepus americanus
lepus_url = "http://phylopic.org/assets/images/submissions/f69eb95b-3d0d-491d-9a7f-acddd419afed.128.png"
lepus = readPNG(getURLContent(lepus_url), 
                native = T)
lepus2 <- rasterGrob(lepus, interpolate = TRUE)

c <- itcha_daily_summary %>%
  filter(Species == c("Lepus americanus")) %>%
  ggplot()+
  geom_jitter(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(lepus2, xmin=0.5, xmax=1.1, ymin=0.055, ymax=0.08)+
  theme(legend.position="none")+
  xlab("")

#odocoileus hemionus
odo_url = "http://phylopic.org/assets/images/submissions/833aec60-8e2d-4275-bd0d-101bf1a6e8e4.128.png"
odo = readPNG(getURLContent(odo_url), 
                native = T)
odo2 <- rasterGrob(odo, interpolate = TRUE)

d <- itcha_daily_summary %>%
  filter(Species == c("Odocoileus hemionus")) %>%
  ggplot()+
  geom_jitter(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(odo2, xmin=0.5, xmax=1.1, ymin=0.038, ymax=0.056)+
  theme(legend.position="none")+
  xlab("")

#rangifer tarandus
rang_url = "http://phylopic.org/assets/images/submissions/a7e8f29e-a4e2-499a-b913-491f78f44f1d.128.png"
rang = readPNG(getURLContent(rang_url), 
              native = T)
rang2 <- rasterGrob(rang, interpolate = TRUE)

e <- itcha_daily_summary %>%
  filter(Species == c("Rangifer tarandus")) %>%
  ggplot()+
  geom_jitter(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(rang2, xmin=0.5, xmax=1.1, ymin=0.0415, ymax=0.067)+
  theme(legend.position="none")+
  xlab("")

#tamiasciurus hudsonicus
tami_url = "http://phylopic.org/assets/images/submissions/dad08fea-5263-4f57-a37b-c27cbe0eb9a5.128.png"
tami = readPNG(getURLContent(tami_url), 
               native = T)
tami2 <- rasterGrob(tami, interpolate = TRUE)

f <- itcha_daily_summary %>%
  filter(Species == c("Tamiasciurus hudsonicus")) %>%
  ggplot()+
  geom_jitter(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome), size=1, alpha=0.5, width=0.05)+
  geom_pointrange(mapping = aes(x = heatdome, y = mean_detections, colour=heatdome),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "violetred4", "indianred2"))+
  theme_bw()+
  annotation_custom(tami2, xmin=0.5, xmax=1.1, ymin=0.03, ymax=0.045)+
  theme(legend.position="none")+
  xlab("")

#visualising
grid_plot <- grid.arrange(a,b,c,d,e,f, ncol=2)
#saving
g <- arrangeGrob(a,b,c,d,e,f, ncol=2) #generates g
ggsave(file="figures/maggie_proto/detection_fig.png", g, width = 5, height = 6, dpi = 300)

ggsave(file="figures/maggie_proto/detection_moose_fig.png", a, width = 5, height = 4, dpi = 300)



### Heat Dome Modelling
### Itcha camera trap data
### Katie Tjaden-McClement

## Set up
library(tidyverse)
library(rstanarm)
library(shinystan)

options(mc.cores = parallel::detectCores())

# load cleaned data
itcha_hd_periods <- read.csv("tidy_data/itcha_hd_periods.csv")

##### Modelling - Treatment Period Scale #####
hd_model1 <- stan_glmer(detections ~ 
                          heat_dome + # binary heat dome variable
                          effort + # number of active camera-days during each tratment period
                          (1 + heat_dome | Species) + # slopes and intercepts for heat dome for each species
                          (1 | grid/Deployment.Location.ID), #account for repeated sampling of each station across periods and spatial grouping of camera grids
                        data = itcha_hd_periods,
                        family = neg_binomial_2 #negative binomial model to account for high proportion of 0's in data
)

summary(hd_model1)

#plot estimated effect sizes for heat dome
plot(hd_model1, pars = names(hd_model1$coefficients[c(2, 154,
                                                      156, 158, 160, 162,
                                                      164)]))

# can view caterpillar plots, estimated effects, graphical posterior predictive checks:
launch_shinystan(hd_model1)

# look at predicted values
yrep <- posterior_predict(hd_model1, draws = 4000) %>% 
  as_tibble()
hd_model_predict <- colMeans(yrep)

hd_model_predict <- tibble(itcha_hd_periods,
                           predicted = as.vector(hd_model_predict)) %>% 
  group_by(Species, trt_week) %>% 
  dplyr::summarise(mean_predicted_detections = mean(predicted)) %>% 
  mutate(effort = if_else(trt_week == "heat_dome", 6, 8),
         predicted_detections_per_day = mean_predicted_detections/effort)

hd_model_predict$trt_week <- fct_relevel(hd_model_predict$trt_week,
                                         levels = c("before", "heat_dome", "after"))

ggplot(hd_model_predict, aes(x = trt_week, 
                             y = predicted_detections_per_day, 
                             group = Species,
                             colour = Species)) +
  geom_point(alpha = 0.5) +
  geom_line()

# check that priors are weakly informative
posterior_vs_prior(hd_model1, pars = names(hd_model1$coefficients[c(2, 154,
                                                                    156, 158, 160, 162,
                                                                    164)]))
# definitely sufficiently wide



##### Daily-scale Modeling #####

itcha_hd_daily <- read.csv("tidy_data/ct_hd_daily_detection_data.csv")

# remove NAs
itcha_hd_daily <- itcha_hd_daily %>% 
  filter(!is.na(temperature),
         !is.na(Elevation))

# center and scale predictors:
itcha_hd_daily$temp_cc <- scale(itcha_hd_daily$temperature)[,1]
itcha_hd_daily$elev_cc <- scale(itcha_hd_daily$Elevation)[,1]

head(itcha_hd_daily)

# are temp and elevation highly correlated?
ggplot(itcha_hd_daily, aes(x = Elevation, y = temperature)) +
  geom_point() +
  geom_smooth(method = "lm")

itcha_hd_daily_cor <- itcha_hd_daily %>% 
  filter(!is.na(temperature),
         !is.na(Elevation))

cor(itcha_hd_daily_cor$Elevation, itcha_hd_daily_cor$temperature) #-0.2301

### Comparing negative binomial vs. zero-inflated models 
#just using temp, no random effects or elevation so things run quickly
hd_daily_nb <- MASS::glm.nb(detections ~ temp_cc,
                            data = itcha_hd_daily)
summary(hd_daily_nb)
AIC(hd_daily_nb) #5074.731

hd_daily_zinb <- pscl::zeroinfl(detections ~ temp_cc,
                                data = itcha_hd_daily,
                                dist = "negbin")
summary(hd_daily_zinb)
AIC(hd_daily_zinb) #5076.798

# the negative binomial model is > 2 AIC lower than zero-inflated
# negative binomial is a better fit

### Full Bayesian models
hd_daily_model1 <- stan_glmer(detections ~ temp_cc + elev_cc +
                                (1 + temp_cc + elev_cc | Species) + 
                                (1 | grid/Deployment.Location.ID),
                              data = itcha_hd_daily,
                              family = neg_binomial_2,
                              adapt_delta = 0.99)
# ^ model takes ~30-40 minutes to run

#launch_shinystan(hd_daily_model1)

## Visualize effects
# all effects
plot(hd_daily_model1, 
     pars = names(hd_daily_model1$coefficients[c(2, 3, 149, 150,
                                                 152, 153, 155, 156,
                                                 158, 159, 161, 162,
                                                 164, 165)]))

# temperature effects
plot(hd_daily_model1, 
     pars = names(hd_daily_model1$coefficients[c(2, 149, 152, 155,
                                                 158, 161, 164)]))

# elevation effects
plot(hd_daily_model1, 
     pars = names(hd_daily_model1$coefficients[c(3, 150, 153, 156, 
                                                 159, 162, 165)]))

# Model with interactions between temperature and elevation:
hd_daily_model2_int <- stan_glmer(detections ~ temp_cc*elev_cc +
                                    (1 + temp_cc*elev_cc | Species) + 
                                    (1 | grid/Deployment.Location.ID),
                                  data = itcha_hd_daily,
                                  family = neg_binomial_2,
                                  adapt_delta = 0.99)
# ran in 45 minutes, no warnings

summary(hd_daily_model2_int)

# all effects
plot(hd_daily_model2_int, 
     pars = names(hd_daily_model2_int$coefficients[c(2, 3, 4, 150, 151,
                                                     152, 154, 155, 156,
                                                     158, 159, 160, 162, 163,
                                                     164, 166, 167, 168, 170,
                                                     171, 172)]))

loo::loo_compare(loo(hd_daily_model1), loo(hd_daily_model2_int))
#                     elpd_diff se_diff
# hd_daily_model3      0.0       0.0   
# hd_daily_model4_int -3.1       1.5 

# the model without interactions is preferred based on LOOIC

## From discussions with Cole: Do we need to include elevation in these models
## or is temperature and a random grid effect enough?



####Meta-analysis data wrangling----
#Courtney Collins 11/3/22
#pull in raw detections data
detections<-read.csv("raw_data/ct_hd_daily_detection_data.csv")

#add pre/during/post heat dome categories
detections <- mutate(detections, 
                     time_period = case_when(date %in% c("2021-06-25", "2021-06-26", 
                                        "2021-06-27", "2021-06-28", "2021-06-29",
                                        "2021-06-30", "2021-07-01", "2021-07-02") ~ "during", 
                            date %in% c("2021-06-17", "2021-06-18", 
                                        "2021-06-19", "2021-06-20", 
                                        "2021-06-21", "2021-06-22", 
                                        "2021-06-23", "2021-06-24") ~ "pre", 
                            TRUE ~ "post")) %>%
     #  heatdome  = fct_relevel(heatdome, c("pre", "during", "post"))%>%
  mutate(treat = if_else(time_period=="pre", "control", "treatment"))

#filter out zeroes
#calculate means, sd, sample sizes pre/during/post
means<-detections%>%group_by(Species, time_period, treat, date)%>%
  summarise(daily_det=sum(detections))%>%group_by(Species, time_period)%>% 
   mutate(mean=mean(daily_det), var=sd(daily_det), samp=n())%>% ungroup(.)
meansduring<-filter(means, time_period!="post")
meanspost<-filter(means, time_period!="during")

#pull wide to look like Maggie's metadataset 
metadataduring<- dplyr::select(meansduring, -date, -daily_det, -time_period)%>% distinct(.)%>%
  pivot_wider(names_from = "treat", values_from = c("samp", "mean", "var"))
metadatapost<- dplyr::select(meanspost, -date, -daily_det, -time_period)%>% distinct(.)%>%
  pivot_wider(names_from = "treat", values_from = c("samp", "mean", "var"))

#add identifier cols 
metadataduring$unique_species<-row.names(metadataduring)
metadataduring$comparison_type<-"temporal_during" 
metadataduring$dates_control <- detections %>% 
  filter(time_period == "pre") %>% 
  distinct(date) %>% 
  as.vector(.)
metadataduring$dates_treatment <- detections %>% 
  filter(time_period == "during") %>% 
  distinct(date) %>% 
  as.vector(.)

metadatapost$unique_species<-row.names(metadatapost)
metadatapost$comparison_type<-"temporal_post" 
metadatapost$dates_control <- detections %>% 
  filter(time_period == "pre") %>% 
  distinct(date) %>% 
  as.vector(.)
metadatapost$dates_treatment <- detections %>% 
  filter(time_period == "post") %>% 
  distinct(date) %>% 
  as.vector(.)

#join 
metadatadf<-rbind(metadataduring, metadatapost)

#add other cols
metadatadf$study_id<-"Mammals_McClement_2022"
metadatadf$response_id<-"1"
metadatadf$response_def<-"daily detections"
metadatadf$response_units<-"counts"
metadatadf$region<-"terrestrial"
metadatadf$movement<-"motile"


metadatadf<-metadatadf%>%
  separate(Species, into = c("genus", "species"))

#random effect column is DeploymentLocation.ID but I don't know how to get this into the metadata format
#not doing this ^

mammals_meta<-dplyr::select(metadatadf, response_id, unique_species, study_id, comparison_type, response_def, 
                   response_units, region, movement, mean_control, mean_treatment, var_control, var_treatment, 
                   samp_control, samp_treatment, , dates_control, dates_treatment, Species)


#save as Rdata
save(mammals_meta, file="tidy_data/meta_analysis/mammals_meta.Rdata")
           