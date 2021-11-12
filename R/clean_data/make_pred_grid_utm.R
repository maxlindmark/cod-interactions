#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2020.06.16: Max Lindmark
#
# - Make a prediction grid that is cropped to predict only on coordinates in the Baltic.
#   Covariates are added later
# 
# A. Load libraries
# 
# B. Basic grid with depth
# 
# C. Add lat long
# 
# D. Add ICES areas
# 
# E. Save
# 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# A. LOAD LIBRARIES ================================================================
rm(list = ls())

# Load libraries, install if needed
library(tidyverse); theme_set(theme_light(base_size = 12))
library(tidylog)
library(viridis)
library(mapdata)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(ncdf4)
library(chron)
library(gganimate)
library(gifski)
library(png)
library(RCurl)
library(sdmTMB)
library(RColorBrewer)
library(terra)

# Print package versions
# sessionInfo()
# other attached packages:
# [1] forcats_0.5.0    stringr_1.4.0    dplyr_1.0.0      purrr_0.3.4      readr_1.3.1
# tidyr_1.1.0      tibble_3.0.3    [8] ggplot2_3.3.2    tidyverse_1.3.0  glmmfields_0.1.4
# Rcpp_1.0.5.1    

# B. BASIC GRID WITH DEPTH =========================================================
# Read data
dat <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/mdat_cpue.csv")

# These are the ranges I'm thinking of using. Convert these to UTM!
ymin = 54
ymax = 58
xmin = 12
xmax = 22

# Function to go from lat long to UTM
LongLatToUTM <- function(x, y, zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

LongLatToUTM(11, 53, 33)
LongLatToUTM(11, 58, 33)
LongLatToUTM(22, 53, 33)
LongLatToUTM(22, 58, 33)

# Round values based on above
utm_x_min <- 230000
utm_x_max <- 960000
utm_y_min <- 5900000
utm_y_max <- 6450000

# Make the evenly spaced (on UTM) grid 
pred_grid <- expand.grid(
  X = seq(utm_x_min, utm_x_max, by = 4000),
  Y = seq(utm_y_min, utm_y_max, by = 4000)) # 4x4 km

# For adding maps to plots
world <- ne_countries(scale = "medium", returnclass = "sf")

map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", continent = "europe")

swe_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

# Transform our map into UTM 9 coordinates, which is the equal-area projection we fit in:
utm_zone33 <- 32633
swe_coast_proj <- sf::st_transform(swe_coast, crs = utm_zone33)

ggplot(swe_coast_proj) + geom_sf()

ggplot(swe_coast_proj) + geom_sf() +
  geom_point(data = pred_grid, aes(x = X, y = Y), alpha = 0.1, shape = 21, fill = NA) +
  theme_light() +
  labs(x = "Longitude", y = "Latitude")

# Looks OK!

# Now we need to add depth
west <- raster("data/depth_geo_tif/D5_2018_rgb-1.tif")
plot(west)

east <- raster("data/depth_geo_tif/D6_2018_rgb-1.tif")
plot(east)

dep_rast <- raster::merge(west, east)

plot(dep_rast)

# Do that by extracting depths of the pred grid
utm_coords <- pred_grid %>% dplyr::select(X, Y)

# Reproject the raster to fit the UTM pred grid...
# Define spatial reference 
sr <- "+proj=utm +zone=33  +datum=WGS84 +units=m " 

# Project Raster... This takes some time
projected_raster <- projectRaster(dep_rast, crs = sr)

utm_coords$depth <- extract(projected_raster, utm_coords[, 1:2])
max(utm_coords$depth)
min(utm_coords$depth)

# Convert to depth (instead of elevation)
ggplot(utm_coords, aes(depth)) + geom_histogram()
utm_coords$depth <- utm_coords$depth - max(utm_coords$depth)
ggplot(utm_coords, aes(depth)) + geom_histogram()

ggplot(swe_coast_proj) + geom_sf() +
  geom_point(data = filter(utm_coords, depth < 0), aes(x = X, y = Y, color = depth)) +
  theme_light() +
  scale_colour_viridis() + 
  labs(x = "Longitude", y = "Latitude")

df <- utm_coords %>% filter(depth < 0) %>% mutate(depth = depth*-1)

# Now make a new grid
pred_grid <- data.frame(X = rep(df$X, length(unique(dat$year))),
                        Y = rep(df$Y, length(unique(dat$year))),
                        depth = rep(df$depth, length(unique(dat$year))),
                        year = rep(sort(unique(dat$year)), each = nrow(df)))

pred_grid <- pred_grid %>% mutate(deep = ifelse(depth > 135, "Y", "N"))

ggplot(swe_coast_proj) + 
  geom_raster(data = filter(pred_grid, year == "1999"), aes(x = X, y = Y, fill = deep)) +
  geom_sf() +
  theme_light() +
  labs(x = "Longitude", y = "Latitude")

ggplot(swe_coast_proj) + 
  geom_raster(data = filter(pred_grid, year == "1999"), aes(x = X, y = Y, fill = depth)) +
  geom_sf() +
  theme_light() +
  labs(x = "Longitude", y = "Latitude")

hist(pred_grid$depth)


# C. ADD LATLONG ===================================================================

# Need to go from UTM to lat long for this one... 
# https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r
xy <- as.matrix(pred_grid[, 1:2])
v <- vect(xy, crs="+proj=utm +zone=33 +datum=WGS84  +units=m")
y <- project(v, "+proj=longlat +datum=WGS84")
lonlat <- geom(y)[, c("x", "y")]

pred_grid$lon <- lonlat[, 1]
pred_grid$lat <- lonlat[, 2]

pred_grid$X <- pred_grid$X/1000
pred_grid$Y <- pred_grid$Y/1000

# D. ADD ICES AREAS VIA SHAPEFILE ==================================================
# https://stackoverflow.com/questions/34272309/extract-shapefile-value-to-point-with-r
# https://gis.ices.dk/sf/
shape <- shapefile("data/ICES_StatRec_mapto_ICES_Areas/StatRec_map_Areas_Full_20170124.shp")
head(shape)

plot(shape, axes = TRUE)

pts <- SpatialPoints(cbind(pred_grid$lon, pred_grid$lat), 
                     proj4string = CRS(proj4string(shape)))

pred_grid$subdiv <- over(pts, shape)$Area_27
pred_grid$subdiv2 <- over(pts, shape)$AreasList

# Rename subdivisions to the more common names and do some more filtering (by sub div and area)
sort(unique(pred_grid$subdiv))

pred_grid2 <- pred_grid %>% 
  mutate(SubDiv = factor(subdiv),
         SubDiv = fct_recode(subdiv,
                             "24" = "3.d.24",
                             "25" = "3.d.25",
                             "26" = "3.d.26",
                             "27" = "3.d.27",
                             "28" = "3.d.28.1",
                             "28" = "3.d.28.2"),
         SubDiv = as.character(SubDiv)) %>% 
  filter(SubDiv %in% c("24", "25", "26", "27", "28")) %>% 
  filter(lat > 54 & lat < 58 & lon < 22)
  

# E. SAVE ==========================================================================
# Save
write.csv(pred_grid2, file = "data/pred_grid2.csv", row.names = FALSE)

