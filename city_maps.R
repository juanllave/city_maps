# # Load libraries
# library(remotes)
# remotes::install_github("ropensci/osmdata") # this approach retrieves the latests version of osmdata

library(tidyverse)
library(osmdata) # package for working with open street data
library(showtext) # for custom fonts
library(ggmap)
library(rvest)
library(svglite)

# Define the city to be mapped
city <- 'Guadalajara'
country <- 'Mexico'

# Set the bb for the desired city
gdl <- getbb(paste0(city, ' ', country))

# Extract the min and max values for both x and y, then add +/- 0.2 to set as plot limits
x_min <- gdl["x", "min"]
x_max <- gdl["x", "max"]
y_min <- gdl["y", "min"]
y_max <- gdl["y", "max"]

x_min <- x_min + 0.2
x_max <- x_max - 0.2
y_min <- y_min + 0.2
y_max <- y_max - 0.2

# Retrieve the different roads based on size
big_streets <- gdl %>%
  opq()%>%
  add_osm_feature(key = 'highway', 
                  value = c('motorway', 'primary', 'motorway_link', 'primary_link')) %>%
  osmdata_sf()

med_streets <- gdl%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_streets <- gdl%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "footway"
                  )) %>%
  osmdata_sf()

# Retrieve other features suchs as rivers and railways
river <- gdl%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

railway <- gdl%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

# Create the plot
gdl_plot <- ggplot() +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 15,
          alpha = 1) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(x_min, x_max),
           ylim = c(y_min, y_max),
           expand = FALSE)+
  theme_void() + # get rid of background color, grid lines, etc.
  theme(plot.title = element_text(size = 80, family = "helvetica", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "helvetica", size = 20, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = toupper(city), subtitle = paste0(x_max, '/', y_max))

print(gdl_plot)

# # Save the plot as SVG for futher editing
# ggsave("gdl.svg", plot = gdl_plot, device = "svg", width = 18, height = 24, units = "in")  

# # Save the plot as a PDF file with a specified width and height
# ggsave("gdl_plot.pdf", plot = gdl_plot, device = "pdf", width = 18, height = 24, units = "in")
# 
# # Save the plot as a PNG file with a specified width and height
# ggsave("gdl_plot.png", plot = gdl_plot, device = "png", width = 6, height = 8, units = "in")
