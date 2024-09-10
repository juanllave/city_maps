# # Load libraries
# library(remotes)
# remotes::install_github("ropensci/osmdata") # this approach retrieves the latests version of osmdata

# Load libraries
library(tidyverse) # Clean, manipulate, and visualize data efficiently.
library(osmdata) # Extract geographic data from.
library(showtext) # To customize fonts
library(ggmap) # Create maps with geographic data.
library(rvest) # Scrape web pages for data.
library(sf) # To work  with simple features (geometry, attributes).
library(raster) # Handle raster data (images, grids).
library(lwgeom) # Perform advanced geometry operations.

# Define the place the map
place <- 'Guadalajara Mexico'

# Get bbbox
placebb <- getbb(place)

# Get the featurues to be mapped
big_streets <- placebb %>% 
  opq() %>%
  add_osm_feature(key = 'highway', 
                  value = c('motorway', 'primary', 'motorway_link', 'primary_link')) %>%
  osmdata_sf()

med_streets <- placebb %>%
  opq() %>%
  add_osm_feature(key = 'highway', 
                  value = c('secondary', 'tertiary', 'secondary_link', 'tertiary_link')) %>%
  osmdata_sf()

small_streets <- placebb %>% 
  opq() %>%
  add_osm_feature(key = 'highway', 
                  value = c('residential', 'living_street', 'unclassified', 'footway')) %>%
  osmdata_sf()

# First plot
ggplot() +
  geom_sf(data = big_streets$osm_lines, 
          inherit.aes = FALSE,
          color = 'black')

# Second plot
ggplot() +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#D9D9D9',
          size = 0.2,
          alpha = 0.5) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#999999',
          size = 0.3,
          alpha = 0.5) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#222222',
          size = 0.4,
          alpha = 0.6)

# Third plot with coord limits
ggplot() +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#999999',
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#D9D9D9',
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#222222',
          size = .4,
          alpha = .6) +
  coord_sf(xlim = c(-103.41, -103.28), 
           ylim = c(20.60, 20.75),
           expand = FALSE)

# Fourth plot
ggplot() +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#999999',
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#D9D9D9',
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = '#222222',
          size = .4,
          alpha = .6) +
  coord_sf(xlim = c(-103.41, -103.28), 
           ylim = c(20.60, 20.75),
           expand = FALSE)  +
  theme_void() + # get rid of background color, grid lines, etc.
  theme(plot.title = element_text(size = 20,
                                  family = 'Spline Sans',
                                  hjust = 0.5,
                                  color = '#222222'),
        plot.subtitle = element_text(size = 8,
                                     family = 'Spline Sans Mono',
                                     hjust = 0.5,
                                     color = '#222222',
                                     margin=margin(2, 0, 5, 0))) +
  labs(title = 'Guadalajara',
       subtitle = '20.690°N / 103.368°W')

# Municipal boundaries
all_boundaries <- opq(place) %>%
  add_osm_feature(key = 'boundary', 
                  value = c('administrative')) %>%
  osmdata_sf() %>% 
  unname_osmdata_sf() %>% 
  .$osm_multipolygons

ggplot(data = all_boundaries) + 
  geom_sf()

# Correct boundaries
boundary <- all_boundaries %>% 
  filter(osm_id == 5605820) %>% # This is the specific osm_id for the municipality of Guadalajara
  dplyr::select()

ggplot(data = boundary) + 
  geom_sf()

# Extract the lines from the streets objects
big_streets_lines <- big_streets$osm_lines
med_streets_lines <- med_streets$osm_lines
small_streets_lines <- small_streets$osm_lines

# Perform the intersection with the boundary
big_streets_cropped <- st_intersection(big_streets_lines, boundary)
med_streets_cropped <- st_intersection(med_streets_lines, boundary)
small_streets_cropped <- st_intersection(small_streets_lines, boundary)

# Use st_bbox to get the bounding box from the boundary
bounding_box <- st_bbox(boundary)

# Boundary-shaped map
gdl <- ggplot() +
  geom_sf(data = med_streets_cropped,
          inherit.aes = FALSE,
          color = '#F08080',
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets_cropped,
          inherit.aes = FALSE,
          color = '#F08080',
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets_cropped,
          inherit.aes = FALSE,
          color = '#F08080',
          size = .4,
          alpha = .6) +
  theme_void() +
  theme(plot.title = element_text(size = 20,
                                  family = 'Spline Sans',
                                  hjust = 0.5,
                                  color = '#F08080'),
        plot.subtitle = element_text(size = 8,
                                     family = 'Spline Sans Mono',
                                     hjust = 0.5,
                                     color = '#F08080',
                                     margin=margin(2, 0, 5, 0))) +
  labs(title = 'Guadalajara',
       subtitle = '20.690°N / 103.368°W')

ggsave(file='~/Desktop/test.svg', plot = gdl, width=10, height=8)

# Circle definition
crs2 <- 6362 # https://epsg.io/6362

center <- c(long = -103.350,
            lat = 20.659)
center_proj <- tibble(lat = center['lat'], long = center['long']) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs = crs2)

dist <-  6000
circle <- tibble(lat = center['lat'], long = center['long']) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4326) %>%
  st_transform(crs = crs2) %>% 
  st_buffer(dist = dist) %>% 
  st_transform(crs = 4326)

ggplot(data = circle) + 
  geom_sf()

# Circle cropping
# Perform the intersection with the circle
big_streets_circle <- st_intersection(big_streets_lines, circle)
med_streets_circle <- st_intersection(med_streets_lines, circle)
small_streets_circle <- st_intersection(small_streets_lines, circle)

# Use st_bbox to get the bounding box from the circle
bounding_box_circle <- st_bbox(circle)

# Circle map
ggplot() +
  geom_sf(data = med_streets_circle,
          inherit.aes = FALSE,
          color = '#999999',
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets_circle,
          inherit.aes = FALSE,
          color = '#D9D9D9',
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets_circle,
          inherit.aes = FALSE,
          color = '#222222',
          size = .4,
          alpha = .6) +
  theme_void() +
  theme(plot.title = element_text(size = 20,
                                  family = 'Spline Sans',
                                  hjust = 0.5,
                                  color = '#222222'),
        plot.subtitle = element_text(size = 8,
                                     family = 'Spline Sans Mono',
                                     hjust = 0.5,
                                     color = '#222222',
                                     margin=margin(2, 0, 5, 0))) +
  labs(title = 'Guadalajara',
       subtitle = '20.690°N / 103.368°W')