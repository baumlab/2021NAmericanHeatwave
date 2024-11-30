## Map of study sites 

pkgs <- c("sf", "tidyverse", "cowplot", "ggspatial", "janitor")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

data <- read_csv("./raw_data/case_study_locations.csv") %>% 
  clean_names()

sites <- data %>% 
  dplyr::select(study_organism, latitude, longitude) %>% filter(!is.na(latitude))

df <- st_read("/Users/sandraemry/Documents/Chapter1/fucus-temp/data/raw_data/map_data/CAN_US_BCalbers")

disp_win <- st_sfc(st_point(c(-133, 46.25)), st_point(c(-116, 57)),
                   crs = 4326)
disp_win_trans <- st_transform(disp_win, crs = target_crs) 
disp_win_coords <- st_coordinates(disp_win_trans)

b2 <- brick("/Users/sandraemry/Documents/data.nc")
b1 <- brick("/Users/sandraemry/Documents/data_2.nc")

b <- stack(b1, b2)

b <- brick(b)
max_b <- stackApply(b, indices = rep(1, nlayers(b)), fun = max)

target_crs <- '+init=epsg:3005'

projected_raster <- projectRaster(max_b, crs = target_crs)

box <- st_sfc(st_point(c(-133, 48.25)), st_point(c(-116, 57)),
                   crs = 4326)
box <- st_transform(box, crs = target_crs) 
box <- st_coordinates(box)

display_box <-  c(xmin = disp_win_coords[1,1], 
          ymin = disp_win_coords[1,2], 
          xmax = disp_win_coords[2,1], 
          ymax = disp_win_coords[2,2])

border <- st_crop(df, (box))

projected_raster_cropped <- crop(projected_raster, c(455168.2, 1604668.3, 137278.6, 1377209.3))
data_spdf <- as(projected_raster_cropped, "SpatialPixelsDataFrame")
temp_df_proj <- as.data.frame(data_spdf)
colnames(temp_df_proj) <- c("value", "x", "y")

temp_df_proj <- temp_df_proj %>% 
  mutate(value = value - 273.15)

sites <- st_as_sf(sites, coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326)

target_crs <- '+init=epsg:3005'
sites_transformed <- st_transform(sites, 3005)

border <- st_crop(df, box)
border_2 <- st_union(border)

mask <- border_2 %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_buffer(5) %>%
  st_difference(border_2)

pal <- rev(brewer.pal("RdYlBu", n = 11))  

ggplot() +  
  geom_sf(data = border, fill = "grey87", color = "grey34"
  ) +
  geom_tile(data = temp_df_proj, aes(x = x, y = y, fill = value)) + 
  geom_sf(data = mask, fill = "white", color = "NA"
  ) +
  geom_sf(data = border, fill = "NA", color = "grey34"
  ) +
  geom_sf(data = sites_transformed, size = 1) +
  coord_sf(xlim = box[,'X'], ylim = box[,'Y']) +
  scale_fill_gradientn(colours = pal, limits = c(20, 45)) + 
  theme(legend.position="right") + 
  labs(fill = "Temperature (°C)", x = "", y = "") + 
  theme_classic()

