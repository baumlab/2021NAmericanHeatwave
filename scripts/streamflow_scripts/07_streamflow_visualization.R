# Load packages
library(tidyverse) 
library(lubridate) ## Deal with dates
library(visreg)
library(gridExtra)

all_dats<-read_csv("data/streamflow_wildfires/processed_data/waterflow2021_dailymean.csv")

#flow_all<- read_csv("flow_all.csv") 
# this file is too large for github, so create it in 06 script, store outside github, 
#and import with a modification of this line

xvar<-read_csv("data/streamflow_wildfires/processed_data/xvar_streamflow.csv") 

#==== find a stream that demonstrates mean behaviour ======

mean(xvar$june_ratio, na.rm=TRUE) #1.444229
mean(xvar$aug_ratio, na.rm=TRUE) #0.8627628

xvar %>% filter(june_ratio >1.4 & june_ratio< 1.6) %>% 
  filter(aug_ratio >0.87 & aug_ratio < 0.93) %>%
  filter(temp_change >8 & temp_change <12) %>% 
  filter(cat0.0005g0.25s == "glacier, low snow") %>% 
  select(station, cat0.0005g0.25s, june_ratio, aug_ratio, surface_anom_max, temp_change) %>% 
  view()

#final selection 05BB001

# 05BB001 glacier, low snow       1.52     0.919             13.6        9.93
# 08MA002 glacier, low snow       1.44     0.925             15.6       11.1 
# 08MC018 glacier, low snow       1.43     0.911             11.0       10.5 
# 08NB005 glacier, low snow       1.57     0.907             12.0        8.24

#potential example stations: 05BB001,08MA002, 08MC018, 08NB005

chosen_station<-"08NB005"

flow21 <- all_dats %>% 
  filter(station%in% chosen_station) %>% 
  mutate(MonthDay = format(as.Date(date), "%m-%d"),
         Month = format (as.Date(date),"%m"),
         Block = "2021") %>% 
  rename(Date = date) %>%
  mutate(
    mean = daily_flow,
    sd1 = daily_flow,
    sd1low = daily_flow,
    sd2 = daily_flow,
    sd2low = daily_flow
    ) %>% 
  filter(MonthDay!="09-01") %>% 
  dplyr::select(MonthDay, mean, sd1, sd1low, sd2, sd2low, Block)

flow_prior <-flow_all %>% 
  filter(STATION_NUMBER%in% chosen_station) %>% 
  mutate(MonthDay = format(as.Date(Date), "%m-%d"),
         Month = format (as.Date(Date),"%m"),
         Block = "2000-2020") %>% 
  dplyr::rename(daily_flow = Value) %>% 
  filter(Month %in%c("06","07","08")) %>% 
  dplyr::select(-Symbol,-STATION_NUMBER, -Parameter, -Date)

prior_mean <- flow_prior %>% 
  group_by(MonthDay) %>% 
  summarize(
    mean = mean(daily_flow, na.rm=TRUE),
    sd1 = mean+sd(daily_flow, na.rm=TRUE),
    sd1low = mean-sd(daily_flow, na.rm=TRUE),
    sd2 = mean+2*sd(daily_flow, na.rm=TRUE),
    sd2low = mean-2*sd(daily_flow, na.rm=TRUE),
    Block = first(Block))

combine_flow <- prior_mean%>% 
  rbind(flow21) %>% 
  mutate(MonthDayYear = mdy(paste(MonthDay, "2000", sep="-")))



#graph after combining two datasets as two blocks
    
p<-ggplot(data = combine_flow, aes(x = MonthDayYear, y = mean, group = Block))+
  geom_ribbon(aes(y = mean, ymin = sd1low, ymax = sd1, fill = Block), alpha = 0.5, fill = "darkgrey")+
  geom_ribbon(aes(y = mean, ymin = sd2low, ymax = sd2, fill = Block), alpha = 0.3, fill = "darkgrey")+
  geom_line(aes(y = mean, colour = Block), size = 1)+
  #geom_line(aes(group = MonthDayYear))+ vertical lines for every day
  scale_color_manual(name = "Stream 08NB005", values = c("#0000FF", "#FF0000"))+
  geom_vline(xintercept = as.numeric(as.Date("2000-06-25")), linetype="dotted") +
  geom_vline(xintercept = as.numeric(as.Date("2000-07-02")), linetype="dotted") +
  ylab("Mean daily streamflow (m/s)")+
  xlab("Day") +
  scale_x_date(date_labels = "%b")+  
  theme_bw()+
  theme(legend.key = element_blank(), 
        legend.title = element_text(),
        legend.position = c(0.8, 0.8),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(5.5,30,20,5.5),"pt"))+
ggtitle("b   Streamflow changes") +
  theme(plot.title = element_text(face="bold", hjust=0)) 

library(grid)
grob <- grobTree(textGrob("(i)", x=0.01,  y=0.975, hjust=0))
p <- p + annotation_custom(grob)

p

### ==== fit models with snow-glacier categories ====

# ==== june models and plots =====

mj3b<-glm(sqrt(june_ratio) ~ temp_change*cat0.0005g0.25s, family = gaussian, data= xvar)
Anova(mj3b)
summary(mj3b) #181 df (1 obs deleted due to missing data)

square<-function(x){
  return(x**2)
}

pj1<-visreg(mj3b, "temp_change", by = "cat0.0005g0.25s", overlay = TRUE,
            #trans = square, 
            partial = TRUE, 
            ylab= "Streamflow during heatdome period\n2021 / (2000-2020)\n",
            xlab = "Maximum temperature during heat dome:\n change from previous week (°C)",
            gg = TRUE)+
  geom_point(aes(x = 9.93, y = sqrt(1.52)), shape = 1 , size = 2, colour = "black")+ #highlighting the example stream
  scale_x_continuous(breaks=c(5,7.5,10,12.5))+
  scale_y_continuous(breaks=c(0.5, 1.0, 1.581139), limits = c(0.316, 1.73), labels = c("0.25", "1", "2.5")) +
scale_color_manual(name = "", values = c("#9ecae1", "#4292c6", "#08519c")) +
  scale_fill_manual(name = "", values = c("#deebf7", "#9ecae1", "#6baed6")) +
  geom_hline(yintercept = 1)+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

grob2 <- grobTree(textGrob("(ii)", x=0.01,  y=0.95, hjust=0))
pj1 <- pj1 + annotation_custom(grob2)

pj1 #note: tick marks are 0.5, 1.0, 1.5 in sqrt(ratio) units = 0.25, 1, 2.25 original units

#scale_colour_manual would change colour of three lines, awkward integratino with visreg
#pj2<-ggMarginal(pj1, margins = 'y', type="histogram", fill = "grey", xparams = list(bins=10))

# ==== august models and plots ======

ma3b<-lm(sqrt(aug_ratio) ~ surface_anom_max *cat0.0005g0.25s, data= xvar)
Anova(ma3b)
summary(ma3b) #177 df (29 deleted due to missing data)

pa1<-visreg(ma3b, "surface_anom_max", by = "cat0.0005g0.25s", overlay = TRUE, 
            #trans = square, 
            partial = TRUE, ylab= "August streamflow\n2021 / (2000-2020)\n",
            xlab = "Maximum surface temperature\nanomaly during heatwave (°C)", gg = TRUE)+
  geom_point(aes(x = 13.6, y = sqrt(0.919)), shape = 1 , size = 2, colour = "black")+ #highlighting the example stream
  scale_x_continuous(breaks=c(6,10,14,18))+
  scale_y_continuous(breaks=c(0.5, 1.0, 1.581139), limits = c(0.316, 1.73), labels = c("0.25", "1", "2.5"))+
scale_color_manual(name = "", values = c("#9ecae1", "#4292c6", "#08519c")) +
  scale_fill_manual(name = "", values = c("#deebf7", "#9ecae1", "#6baed6")) +
  geom_hline(yintercept = 1)+
  theme_bw()+
  theme(legend.title = element_blank(),
        #legend.position = c(0.5, 0.8),
        legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

grob3 <- grobTree(textGrob("(iii)", x=0.01,  y=0.95, hjust=0))
pa1 <- pa1 + annotation_custom(grob3)

pa1 #tick marks are 0.6, 0.8, 1.0, 1.2 in sqrt(ratio) units

#pa2<-ggMarginal(pa1, margins = 'y', type="histogram", fill = "grey", xparams = list(bins=10))

#to do: same y axis, add ratio = 1 line, combine in panel.

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

pa0<-visreg(ma3b, "surface_anom_max", by = "cat0.0005g0.25s", overlay = TRUE, 
            #trans = square, 
            partial = TRUE, ylab= "August streamflow\n2021 / (2000-2020)\n",
            xlab = "Maximum surface temperature\nanomaly during heatwave (C)", gg = TRUE)+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.direction = "horizontal") +
  scale_color_manual(name = "", values = c("#9ecae1", "#4292c6", "#08519c")) +
  scale_fill_manual(name = "", values = c("#deebf7", "#9ecae1", "#6baed6")) +

pa0

legend <- get_legend(pa0)


b2<-grid.arrange(p, arrangeGrob(legend, pj1, pa1, nrow=3, padding = 1, heights = c(0.8,4,4)), nrow = 1)
b2


ggsave(b2, filename = "figures_tables/raw figures/regression.tiff", width = 10 , height = 7,device = tiff, dpi = 300)
ggsave(pj1, filename = "figures_tables/raw figures/juneregression.tiff", device = tiff, dpi = 300)
ggsave(pa1, filename = "figures_tables/raw figures/augregression.tiff", device = tiff, dpi = 300)

ggsave(p, dpi = 300, device = tiff, width = 20 , height = 15, filename = "figures_tables/raw figures/Stream_08NB005.tiff")

