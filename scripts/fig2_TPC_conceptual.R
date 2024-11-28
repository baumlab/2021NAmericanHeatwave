# Fig 2 - Conceptual TPC figure

# Libraries
library(plantecophys)
library(rTPC)
library(ggpubr)
library(ggplot2)
library(ggridges)
library(raster)
library(tidyverse)
library(data.table)
library(gridExtra)
library(cowplot)
library(grid)
library("rnaturalearth")

# Function to compute fitness distribution, given temp dist. and TPC
compute_fitdist = function(tpc, tdist) {
  
  tmin = min(tpc$temperature)
  tmax = max(tpc$temperature)
  
  tdist = tdist[tdist <= tmax]
  tdist = tdist[tdist >= tmin]
  
  fitdist = c()
  for (i in 1:length(tdist)) {
    ind = which.min(abs(tdist[i]-tpc$temperature))
    fitdist[i] = tpc$performance[ind]
  }
  
  return(fitdist)
}

# Set up plotting theme
my_theme = theme_bw() + 
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

set.seed(1)


# Load source data

# 10-day mean air temperature leading up to heat dome period
Tair10_hist = raster("fig2_raster_data/pre_heatdome_norm_avg_Tavg.tif") # Historical
Tair10_2021 = raster("fig2_raster_data/pre_2021heatdome_avg_Tavg.tif") # 2021; Still need this?

# Avg surface temperature during period of heat dome; assumed = leaf temperature
Tleaf_hist = raster("fig2_raster_data/heatdome_norm_avg_LST_proj.tif") # Historical
Tleaf_2021 = raster("fig2_raster_data/heatdome_2021_avg_LST_proj.tif") # 2021

# Avg VPD during period of heat dome
VPD_hist = raster("fig2_raster_data/heatdome_norm_avg_VPD.tif")
VPD_2021 = raster("fig2_raster_data/heatdome_2021_avg_VPD.tif")

# Crop rasters
b = as(extent(-2.1e6,1e5, -5e5,2.7e6), 'SpatialPolygons')
crs(b) = crs(Tleaf_hist)
rb = crop(Tleaf_hist, b)

Tleaf_2021 = crop(Tleaf_2021,b)
VPD_2021 = crop(VPD_2021,b)/1000
Tair10_2021 = crop(Tair10_2021,b)
Tleaf_hist = crop(Tleaf_hist,b)
VPD_hist = crop(VPD_hist,b)/1000
Tair10_hist = crop(Tair10_hist,b)

# Generate temperature distributions for historical and 2021 heatwaves
Tleaf.dist.hist = Tleaf_hist %>% values() %>% na.omit()
Tleaf.dist.2021 = Tleaf_2021 %>% values() %>% na.omit()


# Generate Heatmap

tpc = data.frame(temperature = seq(0,50,0.1),
                 performance = briere2_1999(seq(0,50,0.1),1,40,1,1.2))

# Hold a and b constant
a = 1
b = 1.3

results = data.frame()
i=0

Tdist.hist.small = sample(Tleaf.dist.hist, 10000)
Tdist.2021.small = sample(Tleaf.dist.2021, 10000)

# Vary over xmin and xmax over some ranges
for(xmin in -30:40) {
  for(xmax in 10:50) {
    
    print(i)
    if(xmax-xmin <= 1) next
    i=i+1
    tpc = data.frame(temperature = seq(xmin,xmax,0.1),
                     performance = briere2_1999(seq(xmin,xmax,0.1),xmin,xmax,a,b))
    tpc$performance[tpc$performance<1] = 0
    tpc$performance[is.nan(tpc$performance)] = 0
    tpc = tpc %>% mutate(performance = performance/sum(performance))
    
    topt = tpc$temperature[which(tpc$performance == max(tpc$performance))]
    breadth = xmax-xmin
    
    Fit.dist.hist = compute_fitdist(tpc, Tdist.hist.small)
    Fit.dist.2021 = compute_fitdist(tpc, Tdist.2021.small)
    
    fit.2021 = mean(Fit.dist.2021)
    fit.hist = mean(Fit.dist.hist)
    
    pct_chg = 100*(fit.2021-fit.hist)/fit.hist
    
    results = bind_rows(results,
                        data.frame(xmin,xmax,topt,breadth,pct_chg))
  }
}

res = results %>% mutate(topt_floor = floor(topt), pct_clip = ifelse(pct_chg>100,100,pct_chg))
zeroes = subset(res, abs(pct_chg) < 5)


###### Make TPC plot #####


# One that's sharply declining with Topt ~25
tpc1 = data.frame(temperature = seq(0,50,0.1),
                  performance = pawar_2018(seq(0,50,0.1),1, 0.66, 3.5, 25, 10),
                  name = "TPC1")
tpc1 = tpc1 %>% mutate(performance = performance/max(performance))

# One that's broader with a higher Topt
tpc2 = data.frame(temperature = seq(0,50,0.1),
                  performance = pawar_2018(seq(0,50,0.1),1, 0.5, 2, 33.5, 10),
                  name = "TPC2")
norm = sum(tpc1$performance)/sum(tpc2$performance)
tpc2 = tpc2 %>% mutate(performance = performance/max(performance))

tpcs = bind_rows(tpc1, tpc2)

# Compute new fitness distributions for these two
fitdist.2021.tpc1 = compute_fitdist(tpc1, Tleaf.dist.2021)
fitdist.2021.tpc2 = compute_fitdist(tpc2, Tleaf.dist.2021)
fitdist.hist.tpc1 = compute_fitdist(tpc1, Tleaf.dist.hist)
fitdist.hist.tpc2 = compute_fitdist(tpc2, Tleaf.dist.hist)

# Build panels for conceptual TPC figure
p1 = ggplot(tpcs, aes(x = temperature, y = performance, lty=name)) +
  geom_line() +
  xlim(0,50) +
  xlab(NULL) +
  ylab("Relative performance") +
  #theme_classic() +
  
  my_theme +
  labs(tag = "a") +
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6)) #+
#annotate("text", x = 0, y = 1, label = "(a)", size=2.5)

# LST
p2 = ggplot() +
  geom_density(data = data.frame(Tleaf.dist.2021), aes(x = Tleaf.dist.2021), color = "red", fill = "red", alpha = 0.5) +
  geom_density(data = data.frame(Tleaf.dist.hist), aes(x = Tleaf.dist.hist), color = "blue", fill = "blue", alpha = 0.5) +
  xlim(0,50) +
  xlab("Temperature (ºC)") +
  ylab("Density") +
  my_theme +
  labs(tag = "b") +
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6)) #+

# legend panel
a = data.frame(data = c(1,2,3,4), name = rep(c("Historical average", "2021 Heat Dome"),2))
p3a = ggplot(data = a) + 
  geom_density(aes(x=data, color = name, fill=name), alpha=0.5) +
  scale_colour_manual( values = c("red","blue")) +
  scale_fill_manual( values = c("red","blue")) +
  #my_theme +
  theme(legend.title = element_blank()) +
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6)) +
  xlim(0,7.5) +
  ylim(0,0.09) +
  theme(legend.position = c(0.3,0.5))+
  theme(legend.key.size = unit(0.35, 'cm'),
        legend.text = element_text(size=8))+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.margin = margin(16,12,16,12))

p3b = ggplot(data = res, aes(x=breadth, y=topt_floor,fill=pct_clip)) + 
  geom_tile() +
  scale_fill_gradient2(low = "chocolate4", high = "forestgreen", mid = "lightgoldenrodyellow", 
                       na.value = "transparent", limits=c(-40,80))+
  labs(fill = "% change in  \nperformance  ") +
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6)) +
  theme(legend.position = c(0.375,0.45),
        legend.direction = "horizontal")+
  theme(legend.key.size = unit(0.35, 'cm'),
        legend.text = element_text(size=6),
        legend.title = element_text(size=8))+
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.margin = margin(22,14,22,14))

# Fitness dist - tpc1
p4 = ggplot() +
  geom_density(data = data.frame(fitdist.2021.tpc1), aes(y = fitdist.2021.tpc1), color = "red", fill = "red", alpha = 0.5) +
  geom_density(data = data.frame(fitdist.hist.tpc1), aes(y = fitdist.hist.tpc1), color = "blue", fill = "blue", alpha = 0.5) +
  xlab("Density") +
  ylab(NULL) + 
  my_theme +
  labs(tag = "c") +
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6))+
  theme(legend.key.size = unit(0.35, 'cm'),
        legend.text = element_text(size=6))+
  theme(legend.position = c(0.5,0.5),
  ) #+

# Fitness dist tpc2
p5 = ggplot() +
  geom_density(data = data.frame(fitdist.2021.tpc2), aes(y = fitdist.2021.tpc2), color = "red", fill = "red", alpha = 0.5,lty=2) +
  geom_density(data = data.frame(fitdist.hist.tpc2), aes(y = fitdist.hist.tpc2), color = "blue", fill = "blue", alpha = 0.5,lty=2) +
  xlab("Density") +
  ylab(NULL) + 
  ylim(0,1) +
  my_theme +
  labs(tag = "d") +
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6))+
  theme(legend.key.size = unit(0.35, 'cm'),
        legend.text = element_text(size=6))+
  theme(legend.position = c(0.5,0.5),
  ) #+


pf = ggplot(data = res, aes(x=breadth, y=topt_floor,fill=pct_clip)) +
  geom_tile() +
  scale_fill_gradient2(low = "chocolate4", high = "forestgreen", mid = "lightgoldenrodyellow", 
                       na.value = "transparent", limits=c(-42,82))+
  xlim(22,50) +
  ylim(15,35) +
  xlab("Thermal performance breadth (ºC)") +
  ylab("Optimum temperature (ºC)") +
  my_theme +
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6)) +
  labs(tag = "e", fill = "Percent\nchange in\nperformance") +
  theme(
  )

# Assemble panels into plot
pa = p1
legends = ggarrange(p3a,p3b, ncol=2)
pbc = ggarrange(p4,p5,ncol=2,align="hv")
pde = ggarrange(p2,legends,nrow=2,align="hv")
pf = pf
a = ggarrange(p1,p4,p5,ncol=3,align="h",widths = c(1,0.5,0.5))
b = ggarrange(pde,pf,ncol=2)
plot_all = ggarrange(a,b,ncol=1)

# Plot to file
png("fig2.png", width = 6.92, height = 5.5, units="in", res=300)
plot_all +
  annotate(geom = "segment", x = 0.267, xend = 0.267, y = 0.324, yend = 0.91, color = "blue") +
  annotate(geom = "segment", x = 0.267, xend = 0.65, y = 0.91, yend = 0.91, color = "blue") +
  annotate(geom = "segment", x = 0.267, xend = 0.867, y = 0.815, yend = 0.815, color = "blue", lty = 2) +
  annotate(geom = "segment", x = 0.321, xend = 0.321, y = 0.324, yend = 0.915, color = "red") +
  annotate(geom = "segment", x = 0.321, xend = 0.897, y = 0.915, yend = 0.915, color = "red", lty = 2) +
  annotate(geom = "segment", x = 0.321, xend = 0.598, y = 0.795, yend = 0.795, color = "red") +
  
  annotate(geom = "segment", x = 0.598, xend = 0.598, y = 0.91, yend = 0.795, arrow = arrow(type = "closed", length = unit(0.01, "npc"))) +
  annotate(geom = "segment", x = 0.865, xend = 0.865, y = 0.815, yend = 0.915, arrow = arrow(type = "closed", length = unit(0.01, "npc")))
dev.off()
