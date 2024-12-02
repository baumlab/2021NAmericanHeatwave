################################################################################
################################################################################
###                                                                          ###
###                         HEAT DOME WORKING GROUP                          ###
###                            LUPLAT DEMOG PLOT                             ###
###                                                                          ###
################################################################################
################################################################################

# Created May 4, 2022
# Last Updated May 5, 2022
# Authors: Katie Goodwin

# Figure to demonstrate the effects of the heat dome on LUPLAT demography (specifically fruit counts)

# Data collected in Skagit Valley and E.C. Manning Provincial Parks, BC from 2020-2021
# Email katie.goodwin@botany.ubc.ca for full data collection protocols


################################################################################
##### INPUT DATA AND CALL PACKAGES ---------------------------------------------
################################################################################
luplat <- read.csv("data/tidy_data/fig3/LUPLATheatdome_fruits_20_21.csv") #fruit counts for luplat in 2020 and 2021

str(luplat)

# correct classes
luplat$site <- as.factor(luplat$site)
luplat$transect <- as.factor(luplat$transect)
luplat$id <- as.factor(luplat$id)
luplat$yr <- as.factor(luplat$yr)

summary(luplat)

library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)

# create new variable: 1 + log fruit counts
luplat <- luplat %>%
  mutate(luplat, logfruit = (log(1 + fruitcount))) 

################################################################################
##### FRUIT COUNT DIFFERENCES --------------------------------------------------
################################################################################

# fruit counts in 2020 and 2021 (not using because log relationship is better)
ggplot(data = luplat) +
  geom_boxplot(mapping = aes (x = yr, y = fruitcount)) +
  theme_bw()

# produce final figure of log fruit counts and year
pdf(file = "/figures/LUPLAT_fruitcount.pdf",  
    width = 5, 
    height = 5) 
dev.off()

ggplot(data = luplat) +
 # geom_jitter(mapping = aes (x = yr, y = logfruit), 
#            shape=21,position=position_jitter(0.1),size=3)+
  geom_violin(mapping = aes (x = yr, y = logfruit, fill=yr), trim=T)+
               theme_classic()+
  geom_boxplot( aes (x = yr, y = logfruit), width=0.1, alpha=0.2)+
  labs(y= "Fruits (log)", x = " ") + 
  scale_fill_manual(values = c("#FFCCBC","#B71C1C", "#FF7043"))+
  scale_x_discrete(labels=c("2020" = "Pre", "2021" = "During"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))
################################################################################
##### SUMMARY STATISTICS -------------------------------------------------------
################################################################################

# mean and sd for fruit count between years
summary <- luplat %>%
  group_by(yr) %>%
  summarize(meanfruit = mean(fruitcount),
            sdfruit = sd(fruitcount),
            meanlogfruit = mean(logfruit),
            sdlogfruit = sd(logfruit))

#success or fail fruitingvarialbes
luplat <- mutate(luplat, fruitsuccess = ifelse(fruitcount == "0", 0, 1))

# mean percent loss fruit for a plant from 2020 to 2021
lossindiv <- luplat %>%
  pivot_wider(names_from = "yr", values_from = c("fruitcount", "logfruit")) 

lossindiv <- mutate(lossindiv, fruitloss = (fruitcount_2020 - fruitcount_2021)/fruitcount_2020*100)
lossindiv$fruitloss[is.infinite(lossindiv$fruitloss)] <- NA #removes those that had no fruits in 2020 but did in 2021 so we dont have infinite values n=44 plants   

lossindivsum <- summarize(lossindiv, meanpercentloss = mean(fruitloss, na.rm = TRUE),
                          minpercentloss = min(fruitloss, na.rm = TRUE),
                          maxpercentfruitloss = max(fruitloss, na.rm=TRUE))

hist(lossindiv$fruitloss, na.rm = TRUE)

# number of potentially reproductive plants that produced fruits in 2020 but not 2021







################################################################################
##### MODELLING DIFFERENCES IN FRUIT COUNT BETWEEN YEARS -----------------------
################################################################################

#### SIMPLE T-TEST -------------------------------------------------------------
# simple but a lot of pseudoreplication issues in here
t.test(logfruit ~ yr, data = luplat) 
# p < 1x10^-16, t = 30.495, df = 1077.6, 95% CI = (1.61,1.83)
# group means (2020 = 2.0529, 2021 = 0.335) - log fruits not actual fruits

#### mixed effects model -------------------------------------------------------
# more complicated but deals with pseudoreplication (i.e id nested within transect nested within site)
m1 <- glmer(fruitcount ~ yr + (1 | id/transect/site), luplat, family = poisson)
anova(m1)
summary(m1)

# model diagnostics
# homoegneity
res1 <- resid(m1)
fit1 <- fitted(m1)
plot(fit1,res1)
# normality
hist(res1)

# check for overdispersion (from zuur et al 2009)
N  <- nrow(luplat) #calculate sample size #TS is the data frame 
p  <- length(fixef(m1)) + 1  #'+1' is due to sigma_flock ### fixef only accounts for fixed effects not random. remember no sigma epsilon b/c this is Poisson, not Gaussian
sum(res1^2) / (N - p) # not overdispersed - poisson appropriate





# maggie prototype figure

pacman::p_load(png, grid, RCurl, rphylopic, patchwork, gridExtra)

lupine_url = "http://phylopic.org/assets/images/submissions/f1cfa22f-a198-41b2-b22a-1cdaec1c9e1c.512.png"
lupine = readPNG(getURLContent(lupine_url), 
               native = T)
lupine2 <- rasterGrob(lupine, interpolate = TRUE)

lup <- luplat %>%
  ggplot()+
  geom_jitter(mapping = aes(x = yr, y = logfruit, colour=yr), size=1, alpha=0.1, width=0.05)+
  geom_pointrange(mapping = aes(x = yr, y = logfruit, colour=yr),
                  stat = "summary", position=position_dodge(width=1), size=1.2)+
  scale_colour_manual(values =c("gray69", "indianred2"))+
  theme_bw()+
  annotation_custom(lupine2, xmin=0.43, xmax=0.85, ymin=3, ymax=6)+
  theme(legend.position="none")+
  scale_x_discrete("",
                   labels = c(
                     "2020" = "pre",
                     "2021" = "post"))

ggsave(file="figures/maggie_proto/lupine_fig.png", lup, width = 5, height = 4, dpi = 300)


################################################################################
##### META-ANALYSIS DATA FORMATTING -----------------------
################################################################################
# Katie Goodwin Nov 21, 2022

lupine_meta1 <- luplat %>% # format for fruit count
 pivot_wider(id_cols = c(site, transect, id),
      names_from = c(yr), values_from = c(fruitcount, logfruit)) %>%
  summarize(response_id = 1,
            unique_species = 1,
            study_id = "lupine",
           comparison_type = "temporal_post",
            response_def = "fruit count",
            response_units = "# fruits",
            region = "terrestrial",
            movement = "sessile",
            mean_control = mean(fruitcount_2020),
            mean_treatment = mean(fruitcount_2021),
            var_control = sd(fruitcount_2020),
            var_treatment = sd(fruitcount_2021),
            samp_control = n(),
           samp_treatment = n()) 
            

lupine_meta <- luplat %>% # format for log fruit count (not sure which response will use)
  pivot_wider(id_cols = c(site, transect, id),
              names_from = c(yr), values_from = c(fruitcount, logfruit)) %>%
  summarize(response_id = 1,
            unique_species = 1,
            species = "Lupinus latifolius",
            study_id = "lupine",
            comparison_type = "temporal_post",
            response_def = "log fruit count",
            response_units = "log( fruits + 1)",
            region = "terrestrial",
            movement = "sessile",
            mean_control = mean(logfruit_2020),
            mean_treatment = mean(logfruit_2021),
            var_control = sd(logfruit_2020),
            var_treatment = sd(logfruit_2021),
            samp_control = n(),
            samp_treatment = n(),
            dates_control = "07-27-2020 to 09-04-2020",
            date_treatment = "07-12-2021 to 09-12-2021") 
  
# lupine_meta <- rbind(lupine_meta1, lupine_meta2)

save(lupine_meta, file="data/tidy_data/fig3/meta_analysis/lupine_meta.Rdata")
