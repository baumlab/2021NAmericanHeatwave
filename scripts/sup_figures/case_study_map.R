################################################################################
################################################################################
###                                                                          ###
###                         HEAT DOME WORKING GROUP                          ###
###                     MAP OF STUDY LOCATIONS FOR FIG 3                     ###
###                                                                          ###
################################################################################
################################################################################

# Created May 6, 2022
# Last Updated Oct. 6, 2022
# Authors: Katie Goodwin, ....

# Map showing the heat dome and the locations of all ecological case studies for figure 3


################################################################################
##### INPUT DATA AND CALL PACKAGES ---------------------------------------------
################################################################################

library(sf) 
library(maps)
library(ggplot2)
library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")

### Shapefile of heat dome (Fig1 5day Tmax 90 created by Zihaohan Sang)
heatdome <- st_read('tidy_data/fig1_tmax90_5days_shape/tmax90_5days_update.shp')
heatdome_poly <- st_as_sf(heatdome) # Transform to sf object
st_crs(heatdome_poly) #"Canada_Albers_Equal_Area_Conic"

### Case study site coordinates from metadata 
sites <- read.csv("tidy_data/casestudy_metadata.csv")
str(sites)

sites$using.dataset <- as.factor(sites$using.dataset)

# subset for data confirmed we are using (as of Oct. 6, 2022 still a few TBD datasets)
sites1 <- subset(sites, using.dataset == "y")

sites_pt <- st_as_sf(x = sites1, coords = c("longitude", "latitude"), 
                     crs = st_crs(heatdome_poly)) # transform coordinates to sf object with same coordinate ref. system as heat dome shape
st_crs(sites_pt)

### Map of Western north america
world <- ne_countries(scale = "medium", returnclass = "sf") # countries
state_prov1 <- ne_states(c("united states of america", "canada"), returnclass = "sf") #states and provinces
st_crs(state_prov1)
state_prov <- st_transform(state_prov1, crs = st_crs(heatdome_poly)) # convert coordinate ref. system to heat dome
st_crs(state_prov)


################################################################################
##### MAP OF CASE STUDIES ------------------------------------------------------
################################################################################

ggplot() + # cannot get projections to align. heatdome file wont convert to state_province or vice versa
  geom_sf(data = state_prov1) + # provinces and states
  geom_sf(data = heatdome_poly1)  + 
  geom_sf(data = sites_pt, size = 2, shape = 16) + # site coordinates
  coord_sf(xlim = c(-140, -99), ylim = c(40, 65), expand = FALSE)

