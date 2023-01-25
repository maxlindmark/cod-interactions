library(sf)

sf::sf_use_s2(FALSE)

# Specify map ranges
ymin = 52; ymax = 60.5; xmin = 10; xmax = 24

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

# Define plotting theme for main plot
theme_plot <- function(base_size = 11, base_family = "") {
  theme_light(base_size = base_size, base_family = "") +
    theme(
      legend.position = "bottom",
      legend.key.height = unit(0.2, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, -5, -5, -5),
      strip.text = element_text(size = 9, colour = 'gray10', margin = margin(b = 1, t = 1)),
      strip.background = element_rect(fill = "grey95"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Define plotting theme for facet_wrap map with years
theme_facet_map <- function(base_size = 11, base_family = "") {
  theme_light(base_size = base_size, base_family = "") +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.text = element_text(size = 7),
      strip.text = element_text(size = 8, colour = 'gray10', margin = margin(b = 1, t = 1)),
      strip.background = element_rect(fill = "gray95"),
      legend.direction = "horizontal",
      legend.margin = margin(1, 1, 1, 1),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.key.height = unit(0.4, "line"),
      legend.key.width = unit(2, "line"),
      legend.spacing.x = unit(0.1, 'cm'),
      legend.position = "bottom",
    )
}

# Make default base map plot
xmin2 <- 303000
xmax2 <- 916000
xrange <- xmax2 - xmin2

ymin2 <- 5980000
ymax2 <- 6450000
yrange <- ymax2 - ymin2

plot_map <- 
  ggplot(swe_coast_proj) + 
  xlim(xmin2, xmax2) +
  ylim(ymin2, ymax2) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.3) + 
  theme_plot() +
  guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5),
         fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  NULL

# This is for the diet data
xmin3 <- 303000
xmax3 <- 916000
xrange <- xmax2 - xmin3

ymin3 <- 5980000
ymax3 <- 6580000
yrange <- ymax3 - ymin3

plot_map_d <- 
  ggplot(swe_coast_proj) + 
  xlim(xmin3*1.5, xmax3*0.9) +
  ylim(ymin3*1.025, ymax3*0.98268) + 
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.3) + 
  theme_plot() +
  guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5),
         fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  NULL

# small_cod_stomach <- read_csv("data/clean/small_cod_stomach.csv") 
# plot_map_d + geom_point(data = small_cod_stomach, aes(X*1000, Y*1000))

plot_map_fc <- 
  ggplot(swe_coast_proj) + 
  xlim(xmin2, xmax2) +
  ylim(ymin2, ymax2) +
  labs(x = "Longitude", y = "Latitude") +
  geom_sf(size = 0.3) + 
  theme_facet_map() +
  guides(colour = guide_colorbar(title.position = "top", title.hjust = 0.5),
         fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  NULL

plot_map_labels <- 
  plot_map + 
  annotate("text", label = "Sweden", x = xmin2 + 0.25*xrange, y = ymin2 + 0.75*yrange, color = "black", size = 1.9) +
  annotate("text", label = "Denmark", x = xmin2 + 0.029*xrange, y = ymin2 + 0.32*yrange, color = "black", size = 1.9, angle = 75) +
  annotate("text", label = "Germany", x = xmin2 + 0.07*xrange, y = ymin2 + 0.022*yrange, color = "black", size = 1.9) +
  annotate("text", label = "Poland", x = xmin2 + 0.55*xrange, y = ymin2 + 0.08*yrange, color = "black", size = 1.9) +
  annotate("text", label = "Russia", x = xmin2 + 0.95*xrange, y = ymin2 + 0.18*yrange, color = "black", size = 1.9) +
  annotate("text", label = "Lithuania", x = xmin2 + 1*xrange, y = ymin2 + 0.43*yrange, color = "black", size = 1.9, angle = 75) +
  annotate("text", label = "Latvia", x = xmin2 + 0.99*xrange, y = ymin2 + 0.65*yrange, color = "black", size = 1.9, angle = 75)

plot_map_labels_fc <- 
  plot_map_fc + 
  annotate("text", label = "Sweden", x = xmin2 + 0.25*xrange, y = ymin2 + 0.75*yrange, color = "black", size = 1.9) +
  annotate("text", label = "Denmark", x = xmin2 + 0.029*xrange, y = ymin2 + 0.32*yrange, color = "black", size = 1.9, angle = 75) +
  annotate("text", label = "Germany", x = xmin2 + 0.07*xrange, y = ymin2 + 0.022*yrange, color = "black", size = 1.9) +
  annotate("text", label = "Poland", x = xmin2 + 0.55*xrange, y = ymin2 + 0.08*yrange, color = "black", size = 1.9) +
  annotate("text", label = "Russia", x = xmin2 + 0.95*xrange, y = ymin2 + 0.18*yrange, color = "black", size = 1.9) +
  annotate("text", label = "Lithuania", x = xmin2 + 1*xrange, y = ymin2 + 0.43*yrange, color = "black", size = 1.9, angle = 75) +
  annotate("text", label = "Latvia", x = xmin2 + 0.99*xrange, y = ymin2 + 0.65*yrange, color = "black", size = 1.9, angle = 75)

# Diet map plot here!