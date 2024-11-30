##This is the code for the analysis of intertidal vegetation plot data
##Code orginally written by Sam Starko

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


#read in summarized data
data<-read.csv("raw_data/Victoria_intertidal_seaweeds/heatdome_data.csv")
colnames(data)[2] <- "Plot"
data2  <- data %>% spread(value = Count, key = Classification)

data3 <- data2 %>% group_by(Plot, Time) %>%
  dplyr::summarize(bleached = mean(bleached, na.rm = TRUE),
                   vegetation = mean(vegetation, na.rm = TRUE),
                   substrate = mean(substrate, na.rm = TRUE))

#check that data are out of 100 total points
data3[,3:5] %>% rowSums()
data3 <- data3[-1,]

#Convert data to proportion
data3$bleached_prop <- data3$bleached/100
data3$vegetation_prop <- data3$vegetation/100
data3$substrate_prop <- data3$substrate/100

#Create total vegetation variable (sum of bleached and healthy)
data3$total_veg_prop <- data3$bleached_prop + data3$vegetation_prop

#Fit models comparing percent cover of each vegetation category through time
#Total vegetation
model<- lme(total_veg_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model)
emmeans(model, list(pairwise ~ Time), adjust = "tukey")

#Bleaching
model2<- lme(bleached_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2)
emmeans(model2, list(pairwise ~ Time), adjust = "tukey")

#Healthy vegetation
model2<- lme(vegetation_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2)
emmeans(model2, list(pairwise ~ Time), adjust = "tukey")

data3 %>% group_by(Time) %>%
  dplyr::summarize(bleached = mean(bleached))

#Note package "Rmisc" required below but not loaded because it masks some tidyverse functions

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_sum <- Rmisc::summarySE(data = data3, measurevar = "bleached_prop", groupvars = "Time")
bleached_sum$group = "bleached"
colnames(bleached_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_sum <- Rmisc::summarySE(data = data3, measurevar = "total_veg_prop", groupvars = "Time")
total_sum$group = "veg"
colnames(total_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
veg_sum <- Rmisc::summarySE(data = data3, measurevar = "vegetation_prop", groupvars = "Time")
veg_sum$group = "healthy"
colnames(veg_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_sum2<-rbind(total_sum, veg_sum, bleached_sum)
total_sum2$Prop2 <- total_sum2$Prop*100
total_sum2$CI2 <- total_sum2$CI*100

#Rename grouping variables
total_sum2$group <- gsub("veg", "Total", total_sum2$group )
total_sum2$group <- gsub("bleached", "Bleached/Dead", total_sum2$group )
total_sum2$group <- gsub("healthy", "Healthy", total_sum2$group )

#######################################################
#####THIS IS THE CODE FOR THE MAIN TEXT PLOT###########
#######################################################

ggplot(total_sum2, aes(x = Time, y = Prop2, color = Time, group = group, shape=group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(0,75))+
  scale_color_manual(values = c("#FFCCBC","#B71C1C", "#FF7043"))+
  #scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  geom_errorbar(aes(ymin = Prop2 - CI2, ymax = Prop2 + CI2),
                width = 0.2, position = position_dodge(0))+
  xlab("")+guides(color="none")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                     size = rel(1)), legend.key = element_blank(), legend.title=element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = 'white', colour = 'white'),
        strip.background = element_rect(fill = "white", colour = "black", 
                                        size = rel(2)), complete = TRUE,
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))+
  scale_x_discrete(labels=c("early heatdome" = "Pre", "late heatdome" = "During",
                            "post heatdome" = "Post"))#+
  geom_text(label = c("A", "AB", "B", "A", "B", "B", "A", "B", "C"), position = position_dodge(-1), vjust = -2)

