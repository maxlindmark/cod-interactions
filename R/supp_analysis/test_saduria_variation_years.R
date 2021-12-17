rm(list = ls())

## Add Saduria from raster covariates
library(tidyverse)
library(raster)
library(sf)
library(viridis)
library(tidylog)

saduria_93_19 <- raster("data/saduria_tif/FWBiomassm_raster_19812019presHighweightcor_no0_newZi.tif")
saduria_10_19 <- raster("data/saduria_tif/FWBiomassm_19932019presHighweightcor_no0_bpprf5_2010-2019_prediction_newZi.tif")
saduria_93_09 <- raster("data/saduria_tif/FWBiomassm_19932019presHighweightcor_no0_bpprf5_1993-2009_prediction_newZi.tif")
saduria_81_92 <- raster("data/saduria_tif/FWBiomassm_19932019presHighweightcor_no0_bpprf5_1981-1992_prediction_newZi.tif")

saduria_93_19_longlat = projectRaster(saduria_93_19, crs = ('+proj=longlat'))
saduria_10_19_longlat = projectRaster(saduria_10_19, crs = ('+proj=longlat'))
saduria_93_09_longlat = projectRaster(saduria_93_09, crs = ('+proj=longlat'))
saduria_81_92_longlat = projectRaster(saduria_81_92, crs = ('+proj=longlat'))

plot(saduria_93_19_longlat)

crs(saduria_all)
extent(saduria_all)

# Now extract the values from the saduria raster to the prediction grid
pred_grid2 <- readr::read_csv("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/for_analysis/pred_grid2.csv")
  
df_saduria_93_19 <- pred_grid2 %>% mutate(time_period = "1993-2019")
df_saduria_10_19 <- pred_grid2 %>% mutate(time_period = "2010-2019")
df_saduria_93_09 <- pred_grid2 %>% mutate(time_period = "1993-2009")
df_saduria_81_92 <- pred_grid2 %>% mutate(time_period = "1981-1992")

df_saduria_93_19$biomass_saduria <- extract(saduria_93_19_longlat, df_saduria_93_19[, 8:9])
df_saduria_10_19$biomass_saduria <- extract(saduria_10_19_longlat, df_saduria_10_19[, 8:9])
df_saduria_93_09$biomass_saduria <- extract(saduria_93_09_longlat, df_saduria_93_09[, 8:9])
df_saduria_81_92$biomass_saduria <- extract(saduria_81_92_longlat, df_saduria_81_92[, 8:9])

# Bind rows and plot
ymin = 54; ymax = 58; xmin = 12; xmax = 22

map_data <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf", continent = "europe")

# Crop the polygon for plotting and efficiency:
# st_bbox(map_data) # find the rough coordinates
swe_coast <- suppressWarnings(suppressMessages(
  st_crop(map_data,
          c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax))))

# Transform our map into UTM 33 coordinates, which is the equal-area projection we fit in:
utm_zone33 <- 32633
swe_coast_proj <- sf::st_transform(swe_coast, crs = utm_zone33)

plot_map_raster <- 
  ggplot(swe_coast_proj) + 
  geom_sf(size = 0.3) +
  labs(x = "Longitude", y = "Latitude")

d <- bind_rows(df_saduria_93_19, df_saduria_10_19, df_saduria_93_09, df_saduria_81_92)

# All time periods
plot_map_raster +
  geom_raster(data = d, aes(x = X * 1000, y = Y * 1000, fill = biomass_saduria)) +
  facet_wrap(~ time_period, ncol = 2) +
  scale_fill_viridis() + 
  theme_classic() +
  NULL

# Now calculate proportion
df_saduria_10_19 <- df_saduria_10_19 %>%
  rename("biomass_saduria_10_19" = "biomass_saduria") %>%
  mutate(id = paste(X, Y, time_period))

df_saduria_93_09 <- df_saduria_93_09 %>%
  rename("biomass_saduria_93_09" = "biomass_saduria") %>%
  mutate(id = paste(X, Y, time_period))

dd <- bind_cols(df_saduria_93_09, df_saduria_10_19 %>% dplyr::select(id, biomass_saduria_10_19))

dd <- dd %>% mutate(prop_change_sad = biomass_saduria_10_19 / biomass_saduria_93_09)

plot_map_raster +
  geom_raster(data = filter(dd), aes(x = X * 1000, y = Y * 1000, fill = prop_change_sad)) +
  scale_fill_viridis(trans = "sqrt") + 
  theme_classic() + 
  NULL

dd2 <- dd %>% drop_na(prop_change_sad)

plot_map_raster +
  geom_raster(data = filter(dd2, prop_change_sad < 2), aes(x = X * 1000, y = Y * 1000, fill = prop_change_sad)) +
  scale_fill_gradient2(midpoint = 1) +
  theme_classic() + 
  NULL

plot_map_raster +
  geom_raster(data = filter(dd2, prop_change_sad > 0.5 & prop_change_sad < 1.5), aes(x = X * 1000, y = Y * 1000, fill = prop_change_sad)) +
  scale_fill_gradient2(midpoint = 1) +
  theme_classic() + 
  NULL

  
  