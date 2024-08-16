# # Load libraries
# library(remotes)
# remotes::install_github("ropensci/osmdata") # this approach retrieves the latests version of osmdata

library(tidyverse)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(ggmap)
library(rvest)
library(svglite)

# Set the bb for the desired city
yyc <- getbb('Calgary Canada')

big_streets <- yyc %>%
  opq()%>%
  add_osm_feature(key = 'highway', 
                  value = c('motorway', 'primary', 'motorway_link', 'primary_link')) %>%
  osmdata_sf()

med_streets <- yyc%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


small_streets <- yyc%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                           "footway"
                  )) %>%
  osmdata_sf()

bike <- yyc%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c('cycleway', 'path'
                  )) %>%
  osmdata_sf()

river <- yyc%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

railway <- yyc%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

yyc_plot <- ggplot() +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = 15,
          alpha = 1) +
  # geom_sf(data = bike$osm_lines,
  #         inherit.aes = FALSE,
  #         color = 'green',
  #         size = .3,
  #         alpha = 1)+
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
    coord_sf(xlim = c(-114.35, -113.8),
           ylim = c(51.25, 50.8),
           expand = FALSE)+
  theme_void() + # get rid of background color, grid lines, etc.
  theme(plot.title = element_text(size = 80, family = "helvetica", face="bold", hjust=.5),
        plot.subtitle = element_text(family = "helvetica", size = 20, hjust=.5, margin=margin(2, 0, 5, 0))) +
  labs(title = "CALGARY", subtitle = "51.050°N / 114.085°W")

# Save the plot as SVG for futher editing
ggsave("yyc_plot.svg", plot = yyc_plot, device = "svg", width = 18, height = 24, units = "in")  

# # Save the plot as a PDF file with a specified width and height
# ggsave("yyc_plot.pdf", plot = yyc_plot, device = "pdf", width = 18, height = 24, units = "in")
# 
# # Save the plot as a PNG file with a specified width and height
# ggsave("yyc_plot.png", plot = yyc_plot, device = "png", width = 6, height = 8, units = "in")
