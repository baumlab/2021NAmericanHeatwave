#This is a mixture of Sam Starko's code with updates from Brian Timmer

##Load libraries
library(lubridate)
library(tidyverse)
library(lme4)
library(readxl)
library(tidyverse)
library(cowplot)
library(glmmTMB)
library(nlme)
library(lsmeans)
library(rstatix)
library(PMCMR)
library(cowplot)
library(Rmisc)

#read in summarized data
#data<-read.csv("./data/intertidal_veg_grouped.csv")
clean_long<-read.csv("C:\\Users\\Laminaria\\Dropbox\\PC\\Desktop\\heatdome intertidal veg\\updated 2023\\heatdome_data_2023update.csv")
clean_long <- read.csv("./raw_data/heatdome_seaweeds_baumlab.csv")


cleanlong2<-clean_long

#head(cleanlong2)

colnames(cleanlong2)[2] <- "Plot"
data2  <- cleanlong2 %>% spread(value = Count, key = Classification)

table(cleanlong2$Classification)


data3 <- data2 %>% group_by(Plot, Time) %>%
  dplyr::summarize(bleached_brown = mean(bleached_brown, na.rm = TRUE),
                   bleached_Fucus = mean(bleached_Fucus, na.rm = TRUE),
                   bleached_Acrosiphonia = mean(bleached_Acrosiphonia, na.rm = TRUE),
                   bleached_Ulva = mean(bleached_Ulva, na.rm = TRUE),
                   bleached_kelp = mean(bleached_kelp, na.rm = TRUE),
                   bleached_red = mean(bleached_red, na.rm = TRUE),
                   bleached_coral = mean(bleached_coral, na.rm = TRUE),
                   bleached_surfgrass = mean(bleached_surfgrass, na.rm = TRUE),
                   brown = mean(brown, na.rm = TRUE),
                   Ulva = mean(Ulva, na.rm = TRUE),
                   Acrosiphonia = mean(Acrosiphonia, na.rm = TRUE),
                   kelp = mean(kelp, na.rm = TRUE),
                   Fucus = mean(Fucus, na.rm = TRUE),
                   red = mean(red, na.rm = TRUE),
                   coral = mean(coral, na.rm = TRUE),
                   surfgrass = mean(surfgrass, na.rm = TRUE),
                   substrate = mean(substrate, na.rm = TRUE))

#check that data are out of 100 total points
data3[,3:19] %>% rowSums()
#data3 <- data3[-1,]

#Convert data to proportion
data3$bleached_brown_prop <- data3$bleached_brown/100
data3$bleached_Fucus_prop <- data3$bleached_Fucus/100
data3$bleached_Ulva_prop <-  data3$bleached_Ulva/100
data3$bleached_Acrosiphonia_prop <-  data3$bleached_Acrosiphonia/100
data3$bleached_kelp_prop <-  data3$bleached_kelp/100
data3$bleached_red_prop <-  data3$bleached_red/100
data3$bleached_coral_prop <-  data3$bleached_coral/100
data3$bleached_surfgrass_prop <-  data3$bleached_surfgrass/100
data3$brown_prop <- data3$brown/100
data3$Fucus_prop <- data3$Fucus/100
data3$Ulva_prop <- data3$Ulva/100
data3$Acrosiphonia_prop <- data3$Acrosiphonia/100
data3$kelp_prop <- data3$kelp/100
data3$red_prop <-  data3$red/100
data3$coral_prop <-  data3$coral/100
data3$surfgrass_prop <-  data3$surfgrass/100
data3$substrate_prop <-  data3$substrate/100


#####################################################
##################   Fucus   #######################
######################################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_Fucus_prop <- data3$bleached_Fucus_prop + data3$Fucus_prop
# Select only relevant columns  
Fucus_props <-data3%>%
  select(Plot, Time, total_Fucus_prop,bleached_Fucus_prop,Fucus_prop)
# remove all plots where Fucus was never seen
less_Fucus<- Fucus_props%>% group_by(Plot) %>% filter(sum(total_Fucus_prop,Fucus_prop,bleached_Fucus_prop) > 0)

# check remaining # of plots (n=?)
count(unique(less_Fucus$Plot))
#Create repeated measures plot to see plot-level effects
g1 <- ggplot(less_Fucus, aes(x = Time, y = total_Fucus_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total Fucus cover")

g2 <- ggplot(less_Fucus, aes(x = Time, y = bleached_Fucus_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_Fucus_prop<- lme(total_Fucus_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Fucus) 
anova(model_total_Fucus_prop)
emmeans(model_total_Fucus_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=less_Fucus) %>% emmeans("Time") %>% plot()

#Bleaching
model2bleached_Fucus_prop<- lme(bleached_Fucus_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Fucus) 
anova(model2bleached_Fucus_prop)
emmeans(model2bleached_Fucus_prop, list(pairwise ~ Time), adjust = "tukey")

#Healthy vegetation
model2Fucus_prop<- lme(Fucus_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Fucus) 
anova(model2Fucus_prop)
emmeans(model2Fucus_prop, list(pairwise ~ Time), adjust = "tukey")

less_Fucus %>% group_by(Time) %>%
  dplyr::summarize(bleached_Fucus_prop = mean(bleached_Fucus_prop))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_Fucus_sum <- summarySE(data = less_Fucus, measurevar = "bleached_Fucus_prop", groupvars = "Time")
bleached_Fucus_sum$group = "bleached_Fucus"
colnames(bleached_Fucus_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_Fucus_sum <- summarySE(data = less_Fucus, measurevar = "total_Fucus_prop", groupvars = "Time")
total_Fucus_sum$group = "Fucus"
colnames(total_Fucus_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
Fucus_sum <- summarySE(data = less_Fucus, measurevar = "Fucus_prop", groupvars = "Time")
Fucus_sum$group = "healthy"
colnames(Fucus_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_Fucussum2<-rbind(total_Fucus_sum, Fucus_sum, bleached_Fucus_sum)
total_Fucussum2$Prop2 <- total_Fucussum2$Prop*100
total_Fucussum2$CI2 <- total_Fucussum2$CI*100

#Rename grouping variables
total_Fucussum2$group <- gsub("Fucus", "Total", total_Fucussum2$group )
total_Fucussum2$group <- gsub("bleached_Total", "Bleached/Dead", total_Fucussum2$group )
total_Fucussum2$group <- gsub("healthy", "Healthy", total_Fucussum2$group )

#######################################################
#####THIS IS THE CODE FOR THE Fucus algae PLOT###########
#######################################################

p1<-ggplot(total_Fucussum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(-1,30))+
  ggtitle("Rockweed (n = 18)")+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  geom_errorbar(aes(ymin = Prop2 - CI2, ymax = Prop2 + CI2),
                width = 0.2, position = position_dodge(0))+
  xlab("")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  scale_x_discrete(labels=c("before heatdome" = "Before", "early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))# add letters after in editing software+
geom_text(label = c("A", "AB", "B", "A", "AB", "B", "A", "AB", "B"),  vjust = -1, hjust = 2)
#position = position_dodge(-.5),

#####################################################
##################   kelp   #######################
######################################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_kelp_prop <- data3$bleached_kelp_prop + data3$kelp_prop
# Select only relevant columns  
kelp_props <-data3%>%
  select(Plot, Time, total_kelp_prop,bleached_kelp_prop,kelp_prop)
# remove all plots where kelp was never seen
less_kelp<- kelp_props%>% group_by(Plot) %>% filter(sum(total_kelp_prop,kelp_prop,bleached_kelp_prop) > 0)

# check remaining # of plots (n=?)
count(unique(less_kelp$Plot))
#Create repeated measures plot to see plot-level effects
g1 <- ggplot(less_kelp, aes(x = Time, y = total_kelp_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total kelp cover")

g2 <- ggplot(less_kelp, aes(x = Time, y = bleached_kelp_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_kelp_prop<- lme(total_kelp_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_kelp) 
anova(model_total_kelp_prop)
emmeans(model_total_kelp_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=less_kelp) %>% emmeans("Time") %>% plot()

#Bleaching
model2bleached_kelp_prop<- lme(bleached_kelp_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_kelp) 
anova(model2bleached_kelp_prop)
emmeans(model2bleached_kelp_prop, list(pairwise ~ Time), adjust = "tukey")

#Healthy vegetation
model2kelp_prop<- lme(kelp_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_kelp) 
anova(model2kelp_prop)
emmeans(model2kelp_prop, list(pairwise ~ Time), adjust = "tukey")

less_kelp %>% group_by(Time) %>%
  dplyr::summarize(bleached_kelp = mean(bleached_kelp))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_kelp_sum <- summarySE(data = less_kelp, measurevar = "bleached_kelp_prop", groupvars = "Time")
bleached_kelp_sum$group = "bleached_kelp"
colnames(bleached_kelp_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_kelp_sum <- summarySE(data = less_kelp, measurevar = "total_kelp_prop", groupvars = "Time")
total_kelp_sum$group = "kelp"
colnames(total_kelp_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
kelp_sum <- summarySE(data = less_kelp, measurevar = "kelp_prop", groupvars = "Time")
kelp_sum$group = "healthy"
colnames(kelp_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_kelpsum2<-rbind(total_kelp_sum, kelp_sum, bleached_kelp_sum)
total_kelpsum2$Prop2 <- total_kelpsum2$Prop*100
total_kelpsum2$CI2 <- total_kelpsum2$CI*100

#Rename grouping variables
total_kelpsum2$group <- gsub("kelp", "Total", total_kelpsum2$group )
total_kelpsum2$group <- gsub("bleached_Total", "Bleached/Dead", total_kelpsum2$group )
total_kelpsum2$group <- gsub("healthy", "Healthy", total_kelpsum2$group )

#######################################################
#####THIS IS THE CODE FOR THE kelp algae PLOT###########
#######################################################

p2<-ggplot(total_kelpsum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(-1,33))+
  ggtitle("Kelp (n = 17)")+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  geom_errorbar(aes(ymin = Prop2 - CI2, ymax = Prop2 + CI2),
                width = 0.2, position = position_dodge(0))+
  xlab("")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  scale_x_discrete(labels=c("before heatdome" = "Before", "early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))# add letters after in editing software+
geom_text(label = c("A", "AB", "B", "A", "AB", "B", "A", "AB", "B"),  vjust = -1, hjust = 2)
#position = position_dodge(-.5),




#####################################################
##################   surfgrass   #######################
######################################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_surfgrass_prop <- data3$bleached_surfgrass_prop + data3$surfgrass_prop
# Select only relevant columns  
surfgrass_props <-data3%>%
  select(Plot, Time, total_surfgrass_prop,bleached_surfgrass_prop,surfgrass_prop)
# remove all plots where surfgrass was never seen
less_surfgrass<- surfgrass_props%>% group_by(Plot) %>% filter(sum(total_surfgrass_prop,surfgrass_prop,bleached_surfgrass_prop) > 0)

# check remaining # of plots (n=?)
count(unique(less_surfgrass$Plot))
#Create repeated measures plot to see plot-level effects
g1 <- ggplot(less_surfgrass, aes(x = Time, y = total_surfgrass_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total surfgrass cover")

g2 <- ggplot(less_surfgrass, aes(x = Time, y = bleached_surfgrass_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_surfgrass_prop<- lme(total_surfgrass_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_surfgrass) 
anova(model_total_surfgrass_prop)
emmeans(model_total_surfgrass_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=less_surfgrass) %>% emmeans("Time") %>% plot()

#Bleaching
model2bleached_surfgrass_prop<- lme(bleached_surfgrass_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_surfgrass) 
anova(model2bleached_surfgrass_prop)
emmeans(model2bleached_surfgrass_prop, list(pairwise ~ Time), adjust = "tukey")

#Healthy vegetation
model2surfgrass_prop<- lme(surfgrass_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_surfgrass) 
anova(model2surfgrass_prop)
emmeans(model2surfgrass_prop, list(pairwise ~ Time), adjust = "tukey")

less_surfgrass %>% group_by(Time) %>%
  dplyr::summarize(bleached_surfgrass = mean(bleached_surfgrass))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_surfgrass_sum <- summarySE(data = less_surfgrass, measurevar = "bleached_surfgrass_prop", groupvars = "Time")
bleached_surfgrass_sum$group = "bleached_surfgrass"
colnames(bleached_surfgrass_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_surfgrass_sum <- summarySE(data = less_surfgrass, measurevar = "total_surfgrass_prop", groupvars = "Time")
total_surfgrass_sum$group = "surfgrass"
colnames(total_surfgrass_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
surfgrass_sum <- summarySE(data = less_surfgrass, measurevar = "surfgrass_prop", groupvars = "Time")
surfgrass_sum$group = "healthy"
colnames(surfgrass_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_surfgrasssum2<-rbind(total_surfgrass_sum, surfgrass_sum, bleached_surfgrass_sum)
total_surfgrasssum2$Prop2 <- total_surfgrasssum2$Prop*100
total_surfgrasssum2$CI2 <- total_surfgrasssum2$CI*100

#Rename grouping variables
total_surfgrasssum2$group <- gsub("surfgrass", "Total", total_surfgrasssum2$group )
total_surfgrasssum2$group <- gsub("bleached_Total", "Bleached/Dead", total_surfgrasssum2$group )
total_surfgrasssum2$group <- gsub("healthy", "Healthy", total_surfgrasssum2$group )

#######################################################
#####THIS IS THE CODE FOR THE surfgrass algae PLOT###########
#######################################################

p3<-ggplot(total_surfgrasssum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(-13,52))+
  ggtitle("surfgrass (n = 7)")+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  geom_errorbar(aes(ymin = Prop2 - CI2, ymax = Prop2 + CI2),
                width = 0.2, position = position_dodge(0))+
  xlab("")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  scale_x_discrete(labels=c("before heatdome" = "Before", "early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))




#####################################################
##################   red   #######################
######################################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_red_prop <- data3$bleached_red_prop + data3$red_prop
# Select only relevant columns  
red_props <-data3%>%
  select(Plot, Time, total_red_prop,bleached_red_prop,red_prop)
# remove all plots where red was never seen
less_red<- red_props%>% group_by(Plot) %>% filter(sum(total_red_prop,red_prop,bleached_red_prop) > 0)

# check remaining # of plots (n=?)
count(unique(less_red$Plot))
#Create repeated measures plot to see plot-level effects
g1 <- ggplot(less_red, aes(x = Time, y = total_red_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total red cover")

g2 <- ggplot(less_red, aes(x = Time, y = bleached_red_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_red_prop<- lme(total_red_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_red) 
anova(model_total_red_prop)
emmeans(model_total_red_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=less_red) %>% emmeans("Time") %>% plot()

#Bleaching
model2bleached_red_prop<- lme(bleached_red_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_red) 
anova(model2bleached_red_prop)
emmeans(model2bleached_red_prop, list(pairwise ~ Time), adjust = "tukey")

#Healthy vegetation
model2red_prop<- lme(red_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_red) 
anova(model2red_prop)
emmeans(model2red_prop, list(pairwise ~ Time), adjust = "tukey")

less_red %>% group_by(Time) %>%
  dplyr::summarize(bleached_red = mean(bleached_red))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_red_sum <- summarySE(data = less_red, measurevar = "bleached_red_prop", groupvars = "Time")
bleached_red_sum$group = "bleached_red"
colnames(bleached_red_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_red_sum <- summarySE(data = less_red, measurevar = "total_red_prop", groupvars = "Time")
total_red_sum$group = "red"
colnames(total_red_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
red_sum <- summarySE(data = less_red, measurevar = "red_prop", groupvars = "Time")
red_sum$group = "healthy"
colnames(red_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_redsum2<-rbind(total_red_sum, red_sum, bleached_red_sum)
total_redsum2$Prop2 <- total_redsum2$Prop*100
total_redsum2$CI2 <- total_redsum2$CI*100

#Rename grouping variables
total_redsum2$group <- gsub("red", "Total", total_redsum2$group )
total_redsum2$group <- gsub("bleached_Total", "Bleached/Dead", total_redsum2$group )
total_redsum2$group <- gsub("healthy", "Healthy", total_redsum2$group )

#######################################################
#####THIS IS THE CODE FOR THE red algae PLOT###########
#######################################################

p4<-ggplot(total_redsum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(-1,31))+
  ggtitle("red (n = 27)")+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  geom_errorbar(aes(ymin = Prop2 - CI2, ymax = Prop2 + CI2),
                width = 0.2, position = position_dodge(0))+
  xlab("")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  scale_x_discrete(labels=c("before heatdome" = "Before", "early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))


#####################################################
##################   Ulva   #######################
######################################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_Ulva_prop <- data3$bleached_Ulva_prop + data3$Ulva_prop
# Select only relevant columns  
Ulva_props <-data3%>%
  select(Plot, Time, total_Ulva_prop,bleached_Ulva_prop,Ulva_prop)
# remove all plots where Ulva was never seen
less_Ulva<- Ulva_props%>% group_by(Plot) %>% filter(sum(total_Ulva_prop,Ulva_prop,bleached_Ulva_prop) > 0)

# check remaining # of plots (n=?)
count(unique(less_Ulva$Plot))
#Create repeated measures plot to see plot-level effects
g1 <- ggplot(less_Ulva, aes(x = Time, y = total_Ulva_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total Ulva cover")

g2 <- ggplot(less_Ulva, aes(x = Time, y = bleached_Ulva_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_Ulva_prop<- lme(total_Ulva_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Ulva) 
anova(model_total_Ulva_prop)
emmeans(model_total_Ulva_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=less_Ulva) %>% emmeans("Time") %>% plot()

#Bleaching
model2bleached_Ulva_prop<- lme(bleached_Ulva_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Ulva) 
anova(model2bleached_Ulva_prop)
emmeans(model2bleached_Ulva_prop, list(pairwise ~ Time), adjust = "tukey")

#Healthy vegetation
model2Ulva_prop<- lme(Ulva_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Ulva) 
anova(model2Ulva_prop)
emmeans(model2Ulva_prop, list(pairwise ~ Time), adjust = "tukey")

less_Ulva %>% group_by(Time) %>%
  dplyr::summarize(bleached_Ulva = mean(bleached_Ulva))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_Ulva_sum <- summarySE(data = less_Ulva, measurevar = "bleached_Ulva_prop", groupvars = "Time")
bleached_Ulva_sum$group = "bleached_Ulva"
colnames(bleached_Ulva_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_Ulva_sum <- summarySE(data = less_Ulva, measurevar = "total_Ulva_prop", groupvars = "Time")
total_Ulva_sum$group = "Ulva"
colnames(total_Ulva_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
Ulva_sum <- summarySE(data = less_Ulva, measurevar = "Ulva_prop", groupvars = "Time")
Ulva_sum$group = "healthy"
colnames(Ulva_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_Ulvasum2<-rbind(total_Ulva_sum, Ulva_sum, bleached_Ulva_sum)
total_Ulvasum2$Prop2 <- total_Ulvasum2$Prop*100
total_Ulvasum2$CI2 <- total_Ulvasum2$CI*100

#Rename grouping variables
total_Ulvasum2$group <- gsub("Ulva", "Total", total_Ulvasum2$group )
total_Ulvasum2$group <- gsub("bleached_Total", "Bleached/Dead", total_Ulvasum2$group )
total_Ulvasum2$group <- gsub("healthy", "Healthy", total_Ulvasum2$group )

#######################################################
#####THIS IS THE CODE FOR THE Ulva algae PLOT###########
#######################################################

p5<-ggplot(total_Ulvasum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(-1,25))+
  ggtitle("Sea Lettuce (n = 25)")+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  geom_errorbar(aes(ymin = Prop2 - CI2, ymax = Prop2 + CI2),
                width = 0.2, position = position_dodge(0))+
  xlab("")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  scale_x_discrete(labels=c("before heatdome" = "Before", "early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))


#####################################################
##################   Acrosiphonia   #######################
######################################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_Acrosiphonia_prop <- data3$bleached_Acrosiphonia_prop + data3$Acrosiphonia_prop
# Select only relevant columns  
Acrosiphonia_props <-data3%>%
  select(Plot, Time, total_Acrosiphonia_prop,bleached_Acrosiphonia_prop,Acrosiphonia_prop)
# remove all plots where Acrosiphonia was never seen
less_Acrosiphonia<- Acrosiphonia_props%>% group_by(Plot) %>% filter(sum(total_Acrosiphonia_prop,Acrosiphonia_prop,bleached_Acrosiphonia_prop) > 0)

# check remaining # of plots (n=?)
count(unique(less_Acrosiphonia$Plot))
#Create repeated measures plot to see plot-level effects
g1 <- ggplot(less_Acrosiphonia, aes(x = Time, y = total_Acrosiphonia_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total Acrosiphonia cover")

g2 <- ggplot(less_Acrosiphonia, aes(x = Time, y = bleached_Acrosiphonia_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_Acrosiphonia_prop<- lme(total_Acrosiphonia_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Acrosiphonia) 
anova(model_total_Acrosiphonia_prop)
emmeans(model_total_Acrosiphonia_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=less_Acrosiphonia) %>% emmeans("Time") %>% plot()

#Bleaching
model2bleached_Acrosiphonia_prop<- lme(bleached_Acrosiphonia_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Acrosiphonia) 
anova(model2bleached_Acrosiphonia_prop)
emmeans(model2bleached_Acrosiphonia_prop, list(pairwise ~ Time), adjust = "tukey")

#Healthy vegetation
model2Acrosiphonia_prop<- lme(Acrosiphonia_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=less_Acrosiphonia) 
anova(model2Acrosiphonia_prop)
emmeans(model2Acrosiphonia_prop, list(pairwise ~ Time), adjust = "tukey")

less_Acrosiphonia %>% group_by(Time) %>%
  dplyr::summarize(bleached_Acrosiphonia = mean(bleached_Acrosiphonia))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_Acrosiphonia_sum <- summarySE(data = less_Acrosiphonia, measurevar = "bleached_Acrosiphonia_prop", groupvars = "Time")
bleached_Acrosiphonia_sum$group = "bleached_Acrosiphonia"
colnames(bleached_Acrosiphonia_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_Acrosiphonia_sum <- summarySE(data = less_Acrosiphonia, measurevar = "total_Acrosiphonia_prop", groupvars = "Time")
total_Acrosiphonia_sum$group = "Acrosiphonia"
colnames(total_Acrosiphonia_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
Acrosiphonia_sum <- summarySE(data = less_Acrosiphonia, measurevar = "Acrosiphonia_prop", groupvars = "Time")
Acrosiphonia_sum$group = "healthy"
colnames(Acrosiphonia_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_Acrosiphoniasum2<-rbind(total_Acrosiphonia_sum, Acrosiphonia_sum, bleached_Acrosiphonia_sum)
total_Acrosiphoniasum2$Prop2 <- total_Acrosiphoniasum2$Prop*100
total_Acrosiphoniasum2$CI2 <- total_Acrosiphoniasum2$CI*100

#Rename grouping variables
total_Acrosiphoniasum2$group <- gsub("Acrosiphonia", "Total", total_Acrosiphoniasum2$group )
total_Acrosiphoniasum2$group <- gsub("bleached_Total", "Bleached/Dead", total_Acrosiphoniasum2$group )
total_Acrosiphoniasum2$group <- gsub("healthy", "Healthy", total_Acrosiphoniasum2$group )

#######################################################
#####THIS IS THE CODE FOR THE Acrosiphonia algae PLOT###########
#######################################################

p6<-ggplot(total_Acrosiphoniasum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(-1,5))+
  ggtitle("Green Rope Seaweed (n = 14)")+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  geom_errorbar(aes(ymin = Prop2 - CI2, ymax = Prop2 + CI2),
                width = 0.2, position = position_dodge(0))+
  xlab("")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold"))+
  scale_x_discrete(labels=c("before heatdome" = "Before","early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))


library(patchwork)



p1 + p2 + p3 + p4 + p5 + p6 + plot_layout(ncol = 3, guides = "collect") & theme(legend.position = "right")

ggsave("supplemental_algae.jpg", width = 16, height = 10, dpi=300)




# Write out tidy csv file (SE) -------------------------------------------------
# August 10, 2023

total_Fucussum2$species <- "fucus_distichus"
total_kelpsum2$species <- "kelp"
total_surfgrasssum2$species <- "surfgrass"
total_redsum2$species <- "reds"
total_Ulvasum2$species <- "ulva_spp"
total_Acrosiphoniasum2$species <- "acrosiphonia_spp"

all_species_dat <- bind_rows(total_Fucussum2, total_kelpsum2, total_surfgrasssum2, 
      total_redsum2, total_Ulvasum2, total_Acrosiphoniasum2) %>% 
  relocate(species, .before = Time)

all_species_dat <- all_species_dat %>% 
  select(species, Time, N, Prop, group) %>% 
  filter(group == "Healthy")


