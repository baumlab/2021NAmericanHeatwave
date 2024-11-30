# Fig 5 - GPP map

# Libraries
library(plantecophys)
library(rTPC)
library(ggpubr)
library(ggplot2)
library(ggridges)
library(raster)
library(tidyverse)
library(data.table)
library(cowplot)
library(grid)
library(gridExtra)
library("rnaturalearth")

# Arrhenius-style temperature response
fT = function(Tv, Ea) {
  Tv = Tv+273.15
  R = (6.02214e26)*(1.38065e-23)
  return( exp( (Ea/(298.15*0.001*R))*(1-298.15/Tv) ))
}

# Modified Arrhenius
fTH = function(Tv, Ed, dS) {
  Tv = Tv+273.15
  R = (6.02214e26)*(1.38065e-23)
  
  num = 1+exp( (298.15*dS-Ed)/(298.15*0.001*R) )
  denom = 1+exp( (dS*Tv-Ed)/(0.001*R*Tv)  )
  return(num/denom)
}

# Function to generate GPP using Farquhar-Medlyn coupled model 
# parameterized with CLM5 data
# Tleaf = range of Tleaf values to consider (C)
# VPD = vapour pressure deficit (kPa) at each Tleaf
# T10 = 10 day mean AIR temperature (C)
make_TPC_farquhar = function(Tleaf, VPD, T10) {
  
  # Parameter values from CLM5 documentation, Lawrence et al. 2018
  # Also Medlyn et al. 2002
  
  T10 = T10 + 273.15 # 10-day mean temperature
  Tf = 0 + 273.15 # Freezing temp of water
  
  # Stomatal conductance parameters for Medlyn et al 2011 model
  g0 = 100 # umol/m2s; const. for all PFTs. p104
  g1 = 2.35 # for NET temperate; p104
  
  # Vcmax temperature response parameters
  V25 = 42 # umol/m2s; NET temperate avg.; Table 3 from Lawrence 2019
  VEa = 72000 # J/mol; table 9.2
  VEd = 200000 # J/mol; Table 9.2
  VS = 668.39 - 1.07*(T10 - Tf) # J/molK; Eqn 9.15
  
  # Jmax temperature response parameters
  J25 = V25*(2.59 - 0.035*(T10 - Tf)) # umol/m2s; Eqn 9.16
  JEa = 50000 # J/mol; table 9.2
  JEd = 200000 # J/mol; Table 9.2
  JS = 659.70 - 0.75*(T10 - Tf) # J/molK; Eqn 9.15
  
  # TPU temperature response parameters; p. 106
  TPU25 = 0.167*V25
  TPUEa = VEa
  TPUEd = VEd
  TPUS = VS
  
  # Respiration temperature response parameters; p. 106
  R25 = 0.015*V25
  REa = 46390 # J/mol
  REd = 150650 # J/mol
  RS = 490 # J/molK
  
  # Light response parameters
  theta = 0.7 # Curvature parameter; unitless; p. 105
  alpha = 0.85 # Quantum yield of PSII; unitless; p. 105
  
  # Michaelis Menton temperature response; These are the same as Bernacchi 2001
  KC25 = 404.9 # mmol/mol, following Medlyn e-6 # atm; p. 105
  KCEa = 79430 # J/mol
  KO25 = 278.4 # mmol/mol, following Medlyn e-3 # atm
  KOEa = 36380 # J/mol
  
  # Gamma (star?)
  G25 = 42.75 # mmol/mol following Medlyn e-6 # atm; p. 105
  GEa = 37830 # J/mol
  
  # Build data frame
  df = c()
  df$Tleaf = Tleaf
  df$VPD = VPD
  
  # Compute other temperature dependent parameters following Lawrence 2018
  df$TPU = TPU25*fT(df$Tleaf, TPUEa)*fTH(df$Tleaf, TPUEd, TPUS)
  df$Rd = R25*fT(df$Tleaf, REa)*fTH(df$Tleaf, REd, RS)
  df$KC = KC25*fT(df$Tleaf, KCEa)
  df$KO = KO25*fT(df$Tleaf, KOEa)
  df$Gamma = G25*fT(df$Tleaf, GEa)
  
  # Compute "Effective" Michaelis-Menton constant following Medlyn 2002
  Oi = 210 # mmol/mol; intercellular o2 concentration; Medlyn 2002; 
  df$KM = df$KC*(1 + Oi/df$KO)
  
  # Simulate photosynthesis with Farquhar model
  photo.df = Photosyn(
    VPD = df$VPD,          # OK; add data
    Ca = 410,              # OK 
    PPFD = 1500,           # OK; appears to saturate A
    Tleaf = df$Tleaf,      # OK; add data
    Patm = 100,            # OK; computing relative diff only
    gsmodel = "BBOpti",    # OK
    g1 = g1,               # OK
    g0 = g0,               # OK
    gk = 0.5,              # OK
    alpha = alpha,        # OK
    theta = theta,        # OK
    Jmax = J25,           # OK
    Vcmax = V25,          # OK
    TPU = df$TPU,         # OK
    Rd = df$Rd,           # OK
    EaV = VEa,            # OK
    EdVC = VEd,           # OK
    delsC = VS,           # OK
    EaJ = JEa,            # OK
    EdVJ = JEd,           # OK
    delsJ = JS,           # OK
    GammaStar = df$Gamma, # OK
    Km = df$KM,           # OK
    Tcorrect = T,         # OK
  )
  
  return(data.frame(Tleaf = photo.df$Tleaf, Photo = photo.df$ALEAF, GPP = photo.df$ALEAF + photo.df$Rd))
  
}


# Set up plotting theme
my_theme = theme_bw() + 
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

set.seed(1)

# Load source data

# 10-day mean air temperature leading up to heat dome period
Tair10_hist = raster("data/fig2_raster_data/pre_heatdome_norm_avg_Tavg.tif") # Historical
Tair10_2021 = raster("data/fig2_raster_data/pre_2021heatdome_avg_Tavg.tif") # 2021

# Avg surface temperature during period of heat dome; assumed = leaf temperature
Tleaf_hist = raster("data/fig2_raster_data/heatdome_norm_avg_LST_proj.tif") # Historical
Tleaf_2021 = raster("data/fig2_raster_data/heatdome_2021_avg_LST_proj.tif") # 2021

# Avg VPD during period of heat dome
VPD_hist = raster("data/fig2_raster_data/heatdome_norm_avg_VPD.tif")
VPD_2021 = raster("data/fig2_raster_data/heatdome_2021_avg_VPD.tif")

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


###### Make Raster plot ######
df.2021 = data.table(Tleaf = values(Tleaf_2021), VPD = values(VPD_2021), Tair10 = values(Tair10_2021))
df.2021 = df.2021 %>% mutate(na = is.na(Tleaf) | is.na(VPD) | is.na(Tair10))
df.2021$rowname = 1:dim(df.2021)[1]
df.2021.cut = subset(df.2021, na == F)
df.2021.cut$Photo = make_TPC_farquhar(df.2021.cut$Tleaf, df.2021.cut$VPD, df.2021.cut$Tair10)$Photo
df.2021.cut$GPP = make_TPC_farquhar(df.2021.cut$Tleaf, df.2021.cut$VPD, df.2021.cut$Tair10)$GPP
df.2021.full = df.2021.cut %>% select(GPP, Photo, rowname) %>% merge(df.2021, by="rowname", all.y = T)


gpp_rast_2021 = Tleaf_2021
values(gpp_rast_2021) = df.2021.full$GPP

df.hist = data.table(Tleaf = values(Tleaf_hist), VPD = values(VPD_hist), Tair10 = values(Tair10_hist))
df.hist = df.hist %>% mutate(na = is.na(Tleaf) | is.na(VPD) | is.na(Tair10))
df.hist$rowname = 1:dim(df.hist)[1]
df.hist.cut = subset(df.hist, na == F)
df.hist.cut$Photo = make_TPC_farquhar(df.hist.cut$Tleaf, df.hist.cut$VPD, df.hist.cut$Tair10)$Photo
df.hist.cut$GPP = make_TPC_farquhar(df.hist.cut$Tleaf, df.hist.cut$VPD, df.hist.cut$Tair10)$GPP
df.hist.full = df.hist.cut %>% select(GPP, Photo, rowname) %>% merge(df.hist, by="rowname", all.y = T)


gpp_rast_hist = Tleaf_hist
values(gpp_rast_hist) = df.hist.full$GPP

# Then calculate percent change relative to historical
gpp_pct_change = (gpp_rast_2021-gpp_rast_hist)/gpp_rast_hist
gpp_pct_clip = clamp(gpp_pct_change,-1, 1)

# load up map data
state_prov <- ne_states(c("united states of america", "canada"), returnclass = "sf") #states and provinces

# reproject our data
gpp_pct_clip_trans = projectRaster(gpp_pct_clip, crs = crs(state_prov))
gpp_pct_clip_trans = gpp_pct_clip_trans*100

plotdf2 = as.data.frame(gpp_pct_clip_trans, xy=T)

# Make main plot figure 6
fig6_main = ggplot() +
  geom_raster(data = plotdf2, aes(x=x, y=y, fill= layer)) +
  geom_sf(data = state_prov, fill=NA) +
  scale_fill_gradient2(low = "chocolate4", high = "forestgreen", mid = "lightgoldenrodyellow", 
                       na.value = "transparent", limits=c(-100,100),
                       breaks = c(-100,-50,0,50,100),
                       labels = c("-100", "-50", "0", "50", "100+")) +
  my_theme +
  xlim(-150,-99) +
  ylim(39,65) +
  labs(fill = "Change in\nGPP (%)") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  theme(legend.position = c(0.16, 0.54), legend.title=element_text(size=6)) +
  theme(legend.box.background = element_rect(colour = "black")) +
  theme(legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank()) +
  theme(legend.key.size = unit(0.35, 'cm'),
        legend.text = element_text(size=6)) +
  theme(axis.text = element_text(size=6))

# Make inset plot for 2d
fig6_inset = ggplotGrob(
  ggplot(data = plotdf2, aes(x = layer, y=1,fill = stat(x))) + 
    geom_density_ridges_gradient(lwd=0.2) + 
    scale_fill_gradient2(low = "chocolate4", high = "forestgreen", mid = "lightgoldenrodyellow", 
                         na.value = "transparent", limits=c(-100,100),
                         breaks = c(-100,-50,0,50,100),
                         labels = c("-100", "-50", "0", "50", "100+"))  +
    theme_classic() +
    my_theme +
    theme(axis.ticks=element_blank(),
          axis.text.y =element_blank(),
          axis.text.x = element_text(size=6),
          axis.title = element_text(size=6)) +
    xlab("Change in GPP (%)") +
    ylab("Density") +
    geom_vline(xintercept = 0, lty=2) +
    scale_y_continuous(position = "right") +
    #scale_fill_gradient(low="red", high="green") +
    theme(rect = element_rect(fill = "transparent"),
          plot.background = element_rect(colour = "transparent") )
)

# Draw second half of figure 2 - part d with inset
fig6 = fig6_main +
  annotation_custom(grob = fig6_inset, xmin = -152, xmax = -125, ymin = 38, ymax = 48)
#fig6

png("fig5.png", width=3.46, height = 2.77, units="in", res=300)
grid.arrange(fig6)
dev.off()

# Statistics

# What percent of the heat dome area experienced a decline in GPP?
sum(plotdf2$layer<0, na.rm=T)/sum(!is.na(plotdf2$layer))

# Percentiles
quantile(plotdf2$layer, na.rm=T, probs = c(0,0.05,0.5,0.95,1))

# Mean cellwise change
mean(plotdf2$layer, na.rm=T)
