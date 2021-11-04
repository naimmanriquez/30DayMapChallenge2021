library(tidyverse)
library(osmdata)
library(extrafont)

city_location <- "Mazatlán"

all_street_types <- c("motorway", "primary", "secondary", "tertiary")

secondary_streets <- c("residential", "living_street", "unclassified", "service", "footway")

streets <- getbb(city_location) %>%
  opq() %>%
  add_osm_feature(key = "highway",value = all_street_types) %>%
  osmdata_sf()

small_streets <- getbb(city_location) %>%
  opq() %>%
  add_osm_feature(key = "highway", value = secondary_streets) %>%
  osmdata_sf()

geo_spatial_limits <- getbb(city_location)

geo_spatial_limits

color1 <- "cyan"
color2 <- "#ffbe7f"
back_color <- "#1d1330"
my_font <- "Arial"



ggplot() +
  geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = color1, size = 0.4, alpha = 0.8) +
  geom_sf(data = small_streets$osm_lines, inherit.aes = FALSE, color = color2, size = 0.2, alpha = 0.6) +
  theme(plot.background = element_rect(fill = back_color, colour = NA),
        panel.background = element_rect(fill = back_color, colour = NA), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) + 
  theme(plot.title = element_text(family = my_font, size = 6, color = "white")) + 
  theme(plot.margin = unit(rep(0.5, 4), "cm")) + 
  labs(title = "30DayMapChallenge2021 Open Street Map Mazatlán")