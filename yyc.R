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
place <- 'Calgary Canada'

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

# Municipal boundaries
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
big_streets_cropped <- st_intersection(big_streets_lines, boundary)
med_streets_cropped <- st_intersection(med_streets_lines, boundary)
small_streets_cropped <- st_intersection(small_streets_lines, boundary)

# Use st_bbox to get the bounding box from the boundary
bounding_box <- st_bbox(boundary)

# Boundary-shaped map
yyc <- ggplot() +
  geom_sf(data = med_streets_cropped,
          inherit.aes = FALSE,
          color = '#999999',
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets_cropped,
          inherit.aes = FALSE,
          color = '#D9D9D9',
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets_cropped,
          inherit.aes = FALSE,
          color = '#222222',
          size = .4,
          alpha = .6) +
  theme_void() +
  theme(plot.title = element_text(size = 20,
                                  family = 'Spline Sans',
                                  hjust = 0,
                                  color = '#222222'),
        plot.subtitle = element_text(size = 8,
                                     family = 'Spline Sans Mono',
                                     hjust = 0,
                                     color = '#222222',
                                     margin=margin(2, 0, 5, 0))) +
  labs(title = 'Calgary',
       subtitle = '51.056°N / 114.068°W')
