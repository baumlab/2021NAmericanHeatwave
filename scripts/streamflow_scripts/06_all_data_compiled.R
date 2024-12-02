library(MuMIn)
library(visreg)
library(tidyverse)
library(jtools)
library(car)
library(ggExtra)
library(gridExtra)
library(cowplot)

all_flow<-read_csv("data/streamflow_wildfires/processed_data/all_flow.csv") #streamflow changes
all_temp<-read_csv("data/streamflow_wildfires/processed_data/all_temp.csv") #temperature changes
glacier<-read_csv("data/streamflow_wildfires/input/glacier_snowcover_per_watersheds.csv") %>% 
  rename(n_NAs_ice = n_NAs,
    n_over_20_ice = n_over_20,
    n_less_20_ice = n_less_20,
    n_over_40_ice = n_over_40,
    n_less_40_ice = n_less_40)
snow<-read_csv("data/streamflow_wildfires/input/snowcover_per_watersheds.csv") %>% 
  rename(n_NAs_snow = n_NAs,
         n_over_20_snow = n_over_20,
         n_less_20_snow = n_less_20,
         n_over_40_snow = n_over_40,
         n_less_40_snow = n_less_40)


#======== organize data ===================

all_var<-all_flow %>% 
  left_join(all_temp, by =c("station"= "station")) %>% 
  left_join(glacier, by =c("station"= "Station")) %>% 
  left_join(snow, by =c("station"= "Station")) %>% 
  mutate(quality = if_else(Approval.Name =="PRELIMINARY", 0.25, 1),
         heatdome = if_else(is.nan(air_anom_max),0,1),
         snow20 = n_over_20_snow/(total_n - n_NAs_snow),
         snow40 = n_over_40_snow/(total_n - n_NAs_snow),
         ice20 = n_over_20_ice/(glacer_n - n_NAs_ice),
         ice40 = n_over_40_ice/(glacer_n - n_NAs_ice),
         glacier_prop = glacier_Area/Shap_Ar.x,
         cat0.0005g0.20s = if_else(glacier_prop<0.005&snow40<0.20, "no glacier, low snow",
                                   if_else(glacier_prop>0.005&snow40<0.20, "glacier, low snow", 
                                           if_else(glacier_prop>0.005&snow40>0.20, "glacier, high snow",NA_character_ ))),
         cat0.0005g0.25s = if_else(glacier_prop<0.005&snow40<0.25, "no glacier, low snow",
                                   if_else(glacier_prop>0.005&snow40<0.25, "glacier, low snow", 
                                           if_else(glacier_prop>0.005&snow40>0.25, "glacier, high snow",NA_character_ ))))

# rationale for galcier-snow categories given in later in script
# two categories were visualized initially, cat0.0005g0.25s chosen based on minimizing data loss


#====== global differences between 2021 and 2000-2021 =====

qqnorm(all_var$june_ratio [all_var$heatdome=="1"], pch = 1, frame = FALSE)
qqline(all_var$june_ratio [all_var$heatdome=="1"], col = "steelblue", lwd = 2)

shapiro.test(x = all_var$june_ratio [all_var$heatdome=="1"])

t.test(all_var$earlyjune_ratio [all_var$heatdome=="1"],mu=1)
t.test(all_var$june_ratio [all_var$heatdome=="1"],mu=1)
t.test(all_var$aug_ratio [all_var$heatdome=="1"],mu=1)

min(all_var$earlyjune_ratio [all_var$heatdome=="1"],na.rm=TRUE)#0.12
max(all_var$earlyjune_ratio [all_var$heatdome=="1"],na.rm=TRUE)#2.18

#======check correlations ==================

#model x var corelations, removing all rows with NA or character values

xvar<-all_var %>% select(june_ratio, aug_ratio, glacier_prop, glacier_Area, temp_change, snow20, snow40, ice20, ice40, 
                         air_anom_mean, air_anom_max, surface_anom_mean, surface_anom_max) %>% 
  filter(!is.na(air_anom_mean)) %>% 
  filter(!is.na(june_ratio)) %>% 
  filter(!is.na(aug_ratio)) %>% 
  filter(!is.na(ice20))

par(mfrow=c(1,1))
xvar.cor<-cor(xvar)
corrplot::corrplot(xvar.cor, method = "circle")

#things learnt from corrplot:
#1. the amount of snow with 20 and 40 as threshold highly corr, but..
#2. ice40 better corr than ice20 of june ratio...suggests using 40 as threshold
#3. within the temp type (air or surface), mean and max highly corr, but..
#4. low corr between air and surface
#5. temp_change most corr with air_anom_max, least with surface
#6. june_ratio most corr with temp_change
#7. aug_ratio most corr with surface (either mean or max)
#8. strong corr of glacier prop with snow

#======== slim dataset for analysis ========

#adding to xvar snow/ice categories and data quality

xvar<-all_var %>% select(station, june_ratio, aug_ratio,cat0.0005g0.20s, cat0.0005g0.25s, glacier_prop, glacier_Area, temp_change, snow20, snow40, ice20, ice40, 
                         air_anom_mean, air_anom_max, surface_anom_mean, surface_anom_max, quality) %>% 
  filter(!is.na(air_anom_mean)) %>% 
  filter(!is.na(june_ratio)) %>% 
  filter(!is.na(aug_ratio)) %>% 
  filter(!is.na(ice40))

#model xvar transformations

plot(xvar$june_ratio ~ xvar$temp_change) #pretty linear
plot(xvar$june_ratio ~ xvar$snow40) #definitely asymptotic
plot(xvar$june_ratio ~ xvar$glacier_prop) #definitely asymptotic
plot(xvar$june_ratio ~ xvar$ice40) #vague positive, most points at 100%
plot(xvar$june_ratio ~ xvar$air_anom_mean) #pretty linear
plot(xvar$june_ratio ~ xvar$surface_anom_mean) #vague_neg (max a bit stronger)

plot(xvar$aug_ratio ~ xvar$temp_change) #vague linear
plot(xvar$aug_ratio ~ xvar$snow40) #noisy asymptotic
plot(xvar$aug_ratio ~ xvar$glacier_prop) #definitely asymptotic, clustered left
plot(xvar$aug_ratio ~ xvar$ice40) #vague positive, most points at 100%
plot(xvar$aug_ratio ~ xvar$air_anom_mean) #vaguelinear
plot(xvar$aug_ratio ~ xvar$surface_anom_max) #vague_neg (max a bit stronger)

#transform xvar with nonlinear relationships to june or aug ratio

plot(xvar$june_ratio ~ sqrt(xvar$snow40)) #better
plot(xvar$june_ratio ~ sqrt(xvar$glacier_prop)) #better

# explore transformations of response variables in full models

m11<-lm(sqrt(june_ratio) ~ sqrt(glacier_prop)*temp_change + sqrt(snow40)*temp_change + ice40*temp_change+
          sqrt(glacier_prop)*surface_anom_max + sqrt(snow40)*surface_anom_max + ice40*surface_anom_max, data = all_var)
Anova(m11)
summary(m11)#rsq = 0.66

par(mfrow=c(2,2))
plot(m11) #residuals better with sqrt ratio, prob why rsq better too

visreg::visreg(m11, "temp_change", by = "snow40", breaks = 3)
#notethat the largest temp changes occurred where there was a lot of snow in the watershed
visreg::visreg(m11, "temp_change")
visreg::visreg(m11, "snow40")
visreg::visreg(m11, "ice40")
visreg::visreg(m11, "surface_anom_max") #surprisingly negative

m20<-lm(sqrt(aug_ratio) ~ sqrt(glacier_prop)*temp_change + sqrt(snow40)*temp_change + ice40*temp_change+
          sqrt(glacier_prop)*surface_anom_max + sqrt(snow40)*surface_anom_max + ice40*surface_anom_max, data = all_var)
Anova(m20)
summary(m20)#rsq = 0.28
par(mfrow=c(2,2))
plot(m20)

visreg::visreg(m20, "temp_change", by = "ice40", breaks = 3)
#notethat the largest temp changes occurred where there was a lot of snow in the watershed
visreg::visreg(m20, "temp_change")
visreg::visreg(m20, "glacier_prop")
visreg::visreg(m20, "ice40")
visreg::visreg(m20, "surface_anom_max")

#now that variables appropriately transformed, move to model selection


#=== definitions ===
xvar_trans<- xvar %>% 
  mutate(sqrt_june_ratio = sqrt(june_ratio),
         sqrt_aug_ratio = sqrt(aug_ratio),
         sqrt_snow40 = sqrt(snow40),
         sqrt_glacier_prop = sqrt(glacier_prop))

x.variables<-c("sqrt_glacier_prop", "temp_change", "sqrt_snow40","ice40","surface_anom_max")

#====scale all x variables =======

x_var_scale<-xvar_trans %>% 
  select(all_of(x.variables)) %>% 
  transmute(across(sqrt_glacier_prop:surface_anom_max, scale, .names = "{.col}")) %>% 
  as.data.frame()

scaleset<-xvar_trans%>% 
  select(sqrt_june_ratio, sqrt_aug_ratio, cat0.0005g0.20s, cat0.0005g0.25s, quality, june_ratio, aug_ratio) %>% 
  cbind(x_var_scale)



#=== model list =======

models<-function(file,response)
{
  y<-file[,response]
  
  m0<-lm(y~1, data = file)
  
  tc<-lm(y~temp_change, data = file) 
  
  su<-lm(y~surface_anom_max, data = file)
  
  sn<-lm(y~sqrt_snow40, data = file)
  
  gl<-lm(y~sqrt_glacier_prop, data = file) 
  
  tc_sn<-lm(y~temp_change + sqrt_snow40, data = file)
  
  tc.sn<-lm(y~temp_change *sqrt_snow40, data = file)
  
  tc_gl<-lm(y~temp_change + sqrt_glacier_prop, data = file)
  
  tc.gl<-lm(y~temp_change *sqrt_glacier_prop, data = file)
  
  tc_ic<-lm(y~temp_change + ice40, data = file)
  
  tc.ic<-lm(y~temp_change *ice40, data = file)
  
  su_sn<-lm(y~surface_anom_max + sqrt_snow40, data = file)
  
  su.sn<-lm(y~surface_anom_max *sqrt_snow40, data = file)
  
  su_gl<-lm(y~surface_anom_max + sqrt_glacier_prop, data = file)
  
  su.gl<-lm(y~surface_anom_max *sqrt_glacier_prop, data = file)
  
  su_ic<-lm(y~surface_anom_max + ice40, data = file)
  
  su.ic<-lm(y~surface_anom_max *ice40, data = file)
  
  tc_sn_gl<-lm(y~temp_change + sqrt_snow40 + sqrt_glacier_prop, data = file) 
  
  tc.sn_tc.gl<-lm(y~temp_change*sqrt_snow40 + temp_change*sqrt_glacier_prop, data = file)
  
  tc.sn.gl<-lm(y~temp_change*sqrt_snow40*sqrt_glacier_prop, data = file)
  
  tc_ic_gl<-lm(y~temp_change + ice40 + sqrt_glacier_prop, data = file) 
  
  tc.ic_tc.gl<-lm(y~temp_change*ice40 + temp_change*sqrt_glacier_prop, data = file)
  
  tc.ic.gl<-lm(y~temp_change*ice40*sqrt_glacier_prop, data = file)
  
  su_sn_gl<-lm(y~surface_anom_max + sqrt_snow40 + sqrt_glacier_prop, data = file) 
  
  su.sn_su.gl<-lm(y~surface_anom_max*sqrt_snow40 + surface_anom_max*sqrt_glacier_prop, data = file)
  
  su.sn.gl<-lm(y~surface_anom_max*sqrt_snow40*sqrt_glacier_prop, data = file)
  
  su_ic_gl<-lm(y~surface_anom_max + ice40 + sqrt_glacier_prop, data = file) 
  
  su.ic_su.gl<-lm(y~surface_anom_max*ice40 + surface_anom_max*sqrt_glacier_prop, data = file)
  
  su.ic.gl<-lm(y~surface_anom_max*ice40*sqrt_glacier_prop, data = file)
 
  compare<-model.sel(m0,tc,su,sn,gl,tc_sn, tc.sn, tc_gl, tc.gl, tc_ic, tc.ic, su_sn,
                     su.sn, su_gl, su.gl, su_ic, su.ic, tc_sn_gl, tc.sn_tc.gl, tc.sn.gl,
                     tc_ic_gl,tc.ic_tc.gl, tc.ic.gl, su_sn_gl, su.sn_su.gl, su.sn.gl, su_ic_gl,
                     su.ic_su.gl,su.ic.gl)
                     
  print(compare)
}


#===== compete models ======

output_june<-models(scaleset,"sqrt_june_ratio") #best is temp_change * sqrt_snow40 
#just behind it is temp change * sqrt_snow40 * sqrt_glacier_prop at delta = 2.01
output_june$model = row.names(output_june)
output_june %>% as.data.frame()  %>% select(model, df:weight) %>% write_csv("processed_data/aic_june.csv")

output_aug<-models(scaleset,"sqrt_aug_ratio") #best is surface_anom_max * sqrt_snow40 * sqrt_glacier_prop 
# next best is surface_anom_max * sqrt_glacier_prop, but def not as good at delta = 3.82
output_aug$model = row.names(output_aug)
output_aug %>% as.data.frame()  %>% select(model, df:weight) %>% write_csv("processed_data/aic_aug.csv")


MAjune<-model.avg(output_june, subset = delta<5, revised.var = TRUE)
MAjune %>% summary()


#================== data quality =======================

#some streamflow data is "preliminary", we can downweight these

#here I check if data quality influences model

mj<-glm(sqrt_june_ratio ~ temp_change  * sqrt_snow40, family = gaussian, data = scaleset)
mjw<-glm(sqrt_june_ratio ~ temp_change * sqrt_snow40, weight = quality, family = gaussian, data = scaleset)
anova(mj, mjw, test = "Chi") #no sig difference
Anova(mj)
Anova(mjw)#results qualitatively the same

ma<-glm(sqrt_aug_ratio ~  surface_anom_max * sqrt_snow40 * sqrt_glacier_prop, family = gaussian, data = scaleset)
maw<-glm(sqrt_aug_ratio ~ surface_anom_max * sqrt_snow40 * sqrt_glacier_prop, weight = quality, family = gaussian, data = scaleset)
anova(ma, maw, test = "Chi") #no sig difference
Anova(ma)
Anova(maw)#results qualitatively the same


#================== visualization in continuous space ==================
mj<-glm(sqrt(june_ratio) ~ temp_change  * sqrt(snow40), family = gaussian, data = xvar)
ma<-glm(sqrt(aug_ratio) ~  surface_anom_max * sqrt(snow40) * sqrt(glacier_prop), family = gaussian, data = xvar)

visreg::visreg(mj, "temp_change", by = "snow40", breaks = 3, partial = TRUE)
visreg::visreg(ma, "surface_anom_max", by = "glacier_prop", breaks = 3, partial = TRUE)

#================== visualization in categorical space ==================


#create cutoff points for snow and glacier cover
#which recognize that these covary (there is never "no glacier, high snow")

par(mfrow=c(1,1))
plot(scaleset$sqrt_glacier_prop~scaleset$sqrt_snow40)
plot(sqrt(all_var$glacier_prop)~sqrt(all_var$snow40))
median(scaleset$sqrt_glacier_prop) #-0.36 (in original units 0.006)
median(scaleset$sqrt_snow40) # -0.22 (in original units 0.24)

#based on medians choose these categories:

#category 1 glacier < 0.005, snow < 0.25
#category 2 glacier >0.005, snow <0.025
#category 3 glacier >0.005, snow >0.025

#goal, chose cutoffs for "present" glacier and "high" snow so that there are comparable numbers of streams
# above and below the cutoffs

n<-0.005 #glacier
m <- 0.25 #snow40

xvar %>% 
  filter(glacier_prop < n) %>% 
  filter(snow40 < m) %>% 
  nrow() #82 streams "no glacier low snow"

xvar %>% 
  filter(glacier_prop < n) %>% 
  filter(snow40 > m) %>% 
  select(glacier_prop,snow40) %>% 
  nrow() #should be low...yes, just 1 stream with no glacier and high snow

xvar %>% 
  filter(glacier_prop > n) %>% 
  filter(snow40 < m) %>% 
  nrow() #64 in category "glacier, low snow"

xvar %>% 
  filter(glacier_prop > n) %>% 
  filter(snow40 > m) %>% 
  nrow() #36 in category "glacier, high snow"

#=== outputs ==========

write_csv(xvar, "data/streamflow_wildfires/processed_data/xvar_streamflow.csv")





