##This is the code for the analysis of intertidal vegetation plot data
##Code orginally written by Sam Starko
install.packages('lubridate')
install.packages("tidyverse")
install.packages("lme4")
install.packages("readxl")
install.packages("cowplot")
install.packages("glmmTMB")
install.packages("nlme")
install.packages("lsmeans")
install.packages("rstatix")
install.packages("PMCMR")
install.packages('Rmisc')

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
data<-read.csv("./data/intertidal_veg_grouped.csv")
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

#Create repeated measures plot to see plot-level effects
g1 <- ggplot(data3, aes(x = Time, y = total_veg_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total vegetation cover")

g2 <- ggplot(data3, aes(x = Time, y = bleached_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model<- lme(total_veg_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model)
emmeans(model, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=data3) %>% emmeans("Time") %>% plot()

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
?summarySE()
##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_sum <- summarySE(data = data3, measurevar = "bleached_prop", groupvars = "Time")
bleached_sum$group = "bleached"
colnames(bleached_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_sum <- summarySE(data = data3, measurevar = "total_veg_prop", groupvars = "Time")
total_sum$group = "veg"
colnames(total_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
veg_sum <- summarySE(data = data3, measurevar = "vegetation_prop", groupvars = "Time")
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

ggplot(total_sum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(0,100))+
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
  scale_x_discrete(labels=c("early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))#+
geom_text(label = c("A", "AB", "B", "A", "B", "B", "A", "B", "C"), position = position_dodge(-1), vjust = -2)


ggsave("intertidal_veg_plot.jpg", width = 6, height = 4, dpi = 300 )
#################################################
##########SUPPLEMENTARY PLOTS####################
#################################################


#read in summarized data
data<-read.csv("./data/intertidal_veg_by_taxa.csv")
colnames(data)[3] <- "Plot"
data2  <- data %>% spread(value = Count, key = Classification)
colnames(data2) <- c("X", "Plot", "Date", "Time", "bleached_brown",
                     "bleached_green", "bleached_kelp", "bleached_red",
                     "bleached_surfgrass", "brown", "green", "kelp",
                     "red", "substrate", "surfgrass")


#not sure why this was here but it messes up the means by adding a bunch of zeros
#data2$bleached_brown[is.na(data2$bleached_brown)] <- 0
#data2$bleached_green[is.na(data2$bleached_green)] <- 0
#data2$bleached_kelp[is.na(data2$bleached_kelp)] <- 0
#data2$bleached_red[is.na(data2$bleached_red)] <- 0
#data2$bleached_surfgrass[is.na(data2$bleached_surfgrass)] <- 0
#data2$brown[is.na(data2$brown)] <- 0
#data2$green[is.na(data2$green)] <- 0
#data2$red[is.na(data2$red)] <- 0
#data2$surfgrass[is.na(data2$surfgrass)] <- 0
#data2$kelp[is.na(data2$kelp)] <- 0

data3 <- data2 %>% group_by(Plot, Time) %>%
  dplyr::summarize(bleached_brown = mean(bleached_brown, na.rm = TRUE),
                   bleached_green = mean(bleached_green, na.rm = TRUE),
                   bleached_kelp = mean(bleached_kelp, na.rm = TRUE),
                   bleached_red = mean(bleached_red, na.rm = TRUE),
                   bleached_surfgrass = mean(bleached_surfgrass, na.rm = TRUE),
                   brown = mean(brown, na.rm = TRUE),
                   green = mean(green, na.rm = TRUE),
                   kelp = mean(kelp, na.rm = TRUE),
                   red = mean(red, na.rm = TRUE),
                   surfgrass = mean(surfgrass, na.rm = TRUE),
                   substrate = mean(substrate, na.rm = TRUE))

#check that data are out of 100 total points
data3[,3:13] %>% rowSums()
#data3 <- data3[-1,]

#Convert data to proportion
data3$bleached_brown_prop <- data3$bleached_brown/100
data3$bleached_green_prop <-  data3$bleached_green/100
data3$bleached_kelp_prop <-  data3$bleached_kelp/100
data3$bleached_red_prop <-  data3$bleached_red/100
data3$bleached_surfgrass_prop <-  data3$bleached_surfgrass/100
data3$brown_prop <- data3$brown/100
data3$green_prop <- data3$green/100
data3$kelp_prop <- data3$kelp/100
data3$red_prop <-  data3$red/100
data3$surfgrass_prop <-  data3$surfgrass/100
data3$substrate_prop <-  data3$substrate/100
#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_brown_prop <- data3$bleached_brown_prop + data3$brown_prop

#Create repeated measures plot to see plot-level effects
g1 <- ggplot(data3, aes(x = Time, y = total_brown_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total brown cover")

g2 <- ggplot(data3, aes(x = Time, y = bleached_brown_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_brown_prop<- lme(total_brown_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model_total_brown_prop)
emmeans(model_total_brown_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=data3) %>% emmeans("Time") %>% plot()

#Bleaching
model2bleached_brown_prop<- lme(bleached_brown_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2bleached_brown_prop)
emmeans(model2bleached_brown_prop, list(pairwise ~ Time), adjust = "tukey")

#Healthy vegetation
model2brown_prop<- lme(brown_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2brown_prop)
emmeans(model2brown_prop, list(pairwise ~ Time), adjust = "tukey")

data3 %>% group_by(Time) %>%
  dplyr::summarize(bleached_brown = mean(bleached_brown))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_brown_sum <- summarySE(data = data3, measurevar = "bleached_brown_prop", groupvars = "Time")
bleached_brown_sum$group = "bleached_brown"
colnames(bleached_brown_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_brown_sum <- summarySE(data = data3, measurevar = "total_brown_prop", groupvars = "Time")
total_brown_sum$group = "brown"
colnames(total_brown_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
brown_sum <- summarySE(data = data3, measurevar = "brown_prop", groupvars = "Time")
brown_sum$group = "healthy"
colnames(brown_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_brownsum2<-rbind(total_brown_sum, brown_sum, bleached_brown_sum)
total_brownsum2$Prop2 <- total_brownsum2$Prop*100
total_brownsum2$CI2 <- total_brownsum2$CI*100

#Rename grouping variables
total_brownsum2$group <- gsub("brown", "Total", total_brownsum2$group )
total_brownsum2$group <- gsub("bleached_Total", "Bleached/Dead", total_brownsum2$group )
total_brownsum2$group <- gsub("healthy", "Healthy", total_brownsum2$group )

#######################################################
#####THIS IS THE CODE FOR THE MAIN TEXT PLOT###########
#######################################################

p1<-ggplot(total_brownsum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(0,20))+
  ggtitle("Brown")+
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
  theme(plot.title = element_text(size = 20, face = "bold"))+
  scale_x_discrete(labels=c("early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))#+
  geom_text(label = c("A", "AB", "B", "A", "AB", "B", "A", "AB", "B"),  vjust = -1, hjust = 2)
#position = position_dodge(-.5),


##################################
###### Now for kelp ##############
##################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_kelp_prop <- data3$bleached_kelp_prop + data3$kelp_prop

#Create repeated measures plot to see plot-level effects
g1 <- ggplot(data3, aes(x = Time, y = total_kelp_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total kelp cover")

g2 <- ggplot(data3, aes(x = Time, y = bleached_kelp_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_kelp_prop<- lme(total_kelp_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model_total_kelp_prop)
emmeans(model_total_kelp_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=data3) %>% emmeans("Time") %>% plot()

#Healthy vegetation
model2kelp_prop<- lme(kelp_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2kelp_prop)
emmeans(model2kelp_prop, list(pairwise ~ Time), adjust = "tukey")

#Bleaching
model2bleached_kelp_prop<- lme(bleached_kelp_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2bleached_kelp_prop)
emmeans(model2bleached_kelp_prop, list(pairwise ~ Time), adjust = "tukey")



data3 %>% group_by(Time) %>%
  dplyr::summarize(bleached_kelp = mean(bleached_kelp))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_kelp_sum <- summarySE(data = data3, measurevar = "bleached_kelp_prop", groupvars = "Time")
bleached_kelp_sum$group = "bleached_kelp"
colnames(bleached_kelp_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_kelp_sum <- summarySE(data = data3, measurevar = "total_kelp_prop", groupvars = "Time")
total_kelp_sum$group = "kelp"
colnames(total_kelp_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
kelp_sum <- summarySE(data = data3, measurevar = "kelp_prop", groupvars = "Time")
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
#####THIS IS THE CODE FOR THE MAIN TEXT PLOT###########
#######################################################

p2<- ggplot(total_kelpsum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(0,15))+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  ggtitle("Kelp")+
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
  scale_x_discrete(labels=c("early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))+
  theme(plot.title = element_text(size = 20, face = "bold"))#+
  geom_text(label = c("A", "AB", "B", "A", "B", "AB", "A", "B", "A"),position = position_dodge(-.2),  vjust = -1.75, hjust = 3)


##################################
###### Now for red ##############
##################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_red_prop <- data3$bleached_red_prop + data3$red_prop

#Create repeated measures plot to see plot-level effects
g1 <- ggplot(data3, aes(x = Time, y = total_red_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total red cover")

g2 <- ggplot(data3, aes(x = Time, y = bleached_red_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_red_prop<- lme(total_red_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model_total_red_prop)
emmeans(model_total_red_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=data3) %>% emmeans("Time") %>% plot()

#Healthy vegetation
model2red_prop<- lme(red_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2red_prop)
emmeans(model2red_prop, list(pairwise ~ Time), adjust = "tukey")

#Bleaching
model2bleached_red_prop<- lme(bleached_red_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2bleached_red_prop)
emmeans(model2bleached_red_prop, list(pairwise ~ Time), adjust = "tukey")



data3 %>% group_by(Time) %>%
  dplyr::summarize(bleached_red = mean(bleached_red))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_red_sum <- summarySE(data = data3, measurevar = "bleached_red_prop", groupvars = "Time")
bleached_red_sum$group = "bleached_red"
colnames(bleached_red_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_red_sum <- summarySE(data = data3, measurevar = "total_red_prop", groupvars = "Time")
total_red_sum$group = "red"
colnames(total_red_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
red_sum <- summarySE(data = data3, measurevar = "red_prop", groupvars = "Time")
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
#####THIS IS THE CODE FOR THE MAIN TEXT PLOT###########
#######################################################

p3<- ggplot(total_redsum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(0,30))+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  ggtitle("Red")+
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
  scale_x_discrete(labels=c("early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))+
  theme(plot.title = element_text(size = 20, face = "bold"))#+
  geom_text(label = c("A", "A", "A", "A", "B", "B", "A", "B", "A"),position = position_dodge(-.2),  vjust = -1.75, hjust = 3)

##################################
###### Now for green ##############
##################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_green_prop <- data3$bleached_green_prop + data3$green_prop

#Create repeated measures plot to see plot-level effects
g1 <- ggplot(data3, aes(x = Time, y = total_green_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total green cover")

g2 <- ggplot(data3, aes(x = Time, y = bleached_green_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_green_prop<- lme(total_green_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model_total_green_prop)
emmeans(model_total_green_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=data3) %>% emmeans("Time") %>% plot()

#Healthy vegetation
model2green_prop<- lme(green_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2green_prop)
emmeans(model2green_prop, list(pairwise ~ Time), adjust = "tukey")

#Bleaching
model2bleached_green_prop<- lme(bleached_green_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2bleached_green_prop)
emmeans(model2bleached_green_prop, list(pairwise ~ Time), adjust = "tukey")



data3 %>% group_by(Time) %>%
  dplyr::summarize(bleached_green = mean(bleached_green))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_green_sum <- summarySE(data = data3, measurevar = "bleached_green_prop", groupvars = "Time")
bleached_green_sum$group = "bleached_green"
colnames(bleached_green_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_green_sum <- summarySE(data = data3, measurevar = "total_green_prop", groupvars = "Time")
total_green_sum$group = "green"
colnames(total_green_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
green_sum <- summarySE(data = data3, measurevar = "green_prop", groupvars = "Time")
green_sum$group = "healthy"
colnames(green_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")

#Convert to percentage 
total_greensum2<-rbind(total_green_sum, green_sum, bleached_green_sum)
total_greensum2$Prop2 <- total_greensum2$Prop*100
total_greensum2$CI2 <- total_greensum2$CI*100

#Rename grouping variables
total_greensum2$group <- gsub("green", "Total", total_greensum2$group )
total_greensum2$group <- gsub("bleached_Total", "Bleached/Dead", total_greensum2$group )
total_greensum2$group <- gsub("healthy", "Healthy", total_greensum2$group )

#######################################################
#####THIS IS THE CODE FOR THE MAIN TEXT PLOT###########
#######################################################

p4<- ggplot(total_greensum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(0,30))+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  ggtitle("Green")+
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
  scale_x_discrete(labels=c("early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))+
  theme(plot.title = element_text(size = 20, face = "bold"))#+
  geom_text(label = c("A", "A", "A", "A", "A", "A", "A", "B", "A"),position = position_dodge(-.2),  vjust = -1.75, hjust = 3)

##################################
###### Now for surfgrass ##############
##################################

#Create total vegetation variable for each group (sum of bleached and healthy)
data3$total_surfgrass_prop <- data3$bleached_surfgrass_prop + data3$surfgrass_prop

#Create repeated measures plot to see plot-level effects
g1 <- ggplot(data3, aes(x = Time, y = total_surfgrass_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Total surfgrass cover")

g2 <- ggplot(data3, aes(x = Time, y = bleached_surfgrass_prop, group = Plot))+
  geom_point()+
  geom_line()+
  ylab("Percentage bleached/dead vegetation")

plot_grid(g1, g2)


#Fit models
#Total vegetation
model_total_surfgrass_prop<- lme(total_surfgrass_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model_total_surfgrass_prop)
emmeans(model_total_surfgrass_prop, list(pairwise ~ Time), adjust = "tukey")
#lme(total_veg_prop ~ as.factor(Time), random = ~1|Plot, data=data3) %>% emmeans("Time") %>% plot()

#Healthy vegetation
model2surfgrass_prop<- lme(surfgrass_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2surfgrass_prop)
emmeans(model2surfgrass_prop, list(pairwise ~ Time), adjust = "tukey")

#Bleaching
model2bleached_surfgrass_prop<- lme(bleached_surfgrass_prop ~ as.factor(Time), random = ~1|as.factor(Plot), data=data3) 
anova(model2bleached_surfgrass_prop)
emmeans(model2bleached_surfgrass_prop, list(pairwise ~ Time), adjust = "tukey")



data3 %>% group_by(Time) %>%
  dplyr::summarize(bleached_surfgrass = mean(bleached_surfgrass))

##Create summary datafiles using the summarySE function for each of the three variables (bleached, healthy and total)
bleached_surfgrass_sum <- summarySE(data = data3, measurevar = "bleached_surfgrass_prop", groupvars = "Time")
bleached_surfgrass_sum$group = "bleached_surfgrass"
colnames(bleached_surfgrass_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
total_surfgrass_sum <- summarySE(data = data3, measurevar = "total_surfgrass_prop", groupvars = "Time")
total_surfgrass_sum$group = "surfgrass"
colnames(total_surfgrass_sum) <- c("Time", "N", "Prop", "SD", "SE", "CI", "group")
surfgrass_sum <- summarySE(data = data3, measurevar = "surfgrass_prop", groupvars = "Time")
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
#####THIS IS THE CODE FOR THE MAIN TEXT PLOT###########
#######################################################

p5<- ggplot(total_surfgrasssum2, aes(x = Time, y = Prop2, color = group, group = group))+
  geom_point(cex = 3)+
  geom_line()+
  ylim(c(0,15))+
  scale_color_manual(values = c("Orange", "Darkgreen", "Black"))+
  ylab("Percent cover (%)")+
  ggtitle("Surfgrass")+
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
  scale_x_discrete(labels=c("early heatdome" = "Early", "late heatdome" = "Late",
                            "post heatdome" = "After"))+
  theme(plot.title = element_text(size = 20, face = "bold"))#+
  geom_text(label = c( "A", "A", "A", "A", "B", "AB","A", "B", "AB"),position = position_dodge(-.3),  vjust = -2, hjust = 3)

##############################################
#### Now turn supp plots into a panel#########
###############################################
install.packages("patchwork")
library(patchwork)

supps <- p1 + p2 + p3 + p4 + p5 + guide_area() + plot_layout(ncol = 3, guides = "collect")

ggsave("supplemental_algae.jpg", width = 8, height = 5, dpi=300)
