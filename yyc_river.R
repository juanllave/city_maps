# # Load libraries
# library(remotes)
# remotes::install_github('ropensci/osmdata') # this approach retrieves the latests version of osmdata

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
place <- 'Calgary Canada'

# Get bbbox
placebb <- getbb(place)

# Get water and river information
water_osm <- opq(place) %>%
  add_osm_feature(key = 'natural',
                  value = 'water') %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

river_osm <- opq(place) %>%
  add_osm_feature(key = 'waterway',
                  value = c('river', 'riverbank')) %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

# Combine water and river features
water <- c(water_osm, river_osm) %>% 
  .$osm_multipolygons %>%
  st_make_valid() %>%  # Fix invalid geometries
  dplyr::select(osm_id, name) %>% 
  mutate(area = st_area(.)) # %>% 
  # Filter to remove small isolated lakes, etc.
 # filter(area >= quantile(area, probs = 0.75))

# Get the roads to be mapped
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
                  value = c('residential', 'living_street')) %>%
  osmdata_sf()

# Place administrative boundaries
all_boundaries <- opq(place) %>%
  add_osm_feature(key = 'boundary', 
                  value = c('administrative')) %>%
  osmdata_sf() %>% 
  unname_osmdata_sf() %>% 
  .$osm_multipolygons

# Correct boundaries
boundary <- all_boundaries %>% 
  filter(osm_id == 3227127) %>% # This is the specific osm_id for the city of Calgary
  dplyr::select()

# Extract the lines from the streets objects
big_streets_lines <- big_streets$osm_lines
med_streets_lines <- med_streets$osm_lines
small_streets_lines <- small_streets$osm_lines

# Perform the intersection with the boundary
water_cropped <- st_intersection(water, boundary)
big_streets_cropped <- st_intersection(big_streets_lines, boundary)
med_streets_cropped <- st_intersection(med_streets_lines, boundary)
small_streets_cropped <- st_intersection(small_streets_lines, boundary)

# Use st_bbox to get the bounding box from the boundary
bounding_box <- st_bbox(boundary)


# Boundary-shaped map
yyc <- ggplot() +
  geom_sf(data = water_cropped,
         fill = '#4A90E2',
         # size = 0.8,
         lwd = 0,
         alpha = 0.3) +
    geom_sf(data = med_streets_cropped,
          inherit.aes = FALSE,
          color = '#999999',
          size = 0.3,
          alpha = 0.5) +
  geom_sf(data = small_streets_cropped,
          inherit.aes = FALSE,
          color = '#D9D9D9',
          size = 0.2,
          alpha = 0.3) +
  geom_sf(data = big_streets_cropped,
          inherit.aes = FALSE,
          color = '#222222',
          size = 0.4,
          alpha = 0.6) +
  theme_void() +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(size = rel(2),
                                  family = 'Spline Sans',
                                  hjust = 0.5,
                                  color = '#222222'),
        plot.subtitle = element_text(size = rel(1),
                                     family = 'Spline Sans Mono',
                                     hjust = 0.5,
                                     color = '#222222',
                                     margin=margin(2, 0, 5, 0))) +
  labs(title = 'Calgary',
       subtitle = '51.056°N / 114.068°W')

yyc
