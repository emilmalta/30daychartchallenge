# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(osmdata)
library(here)
library(ragg)
library(ggfx)
library(sf)

# Import data ------------------------------------------------------------------

aarhus_canvas <- 
  st_bbox(
    c(xmin = 10.14, xmax = 10.24, ymax = 56.2, ymin = 56.10), 
    crs = st_crs(4326)
  ) %>% 
  opq()

# Tidy -------------------------------------------------------------------------

aarhus_buildings <- aarhus_canvas %>% 
  add_osm_feature("building") %>% 
  osmdata_sf() %>% 
  pluck("osm_polygons") %>% 
  select(amenity, geometry) %>% 
  mutate(size = as.numeric(st_area(.))) %>% 
  st_centroid() %>% 
  cbind(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  arrange(desc(size))

aarhus_roads <- aarhus_canvas %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() %>% 
  pluck("osm_lines") %>% 
  st_simplify(dTolerance = 50) 

# Visualise --------------------------------------------------------------------

ggplot() +
  with_blur(geom_point(
    data = aarhus_buildings, alpha = .125,
    aes(X, Y,size = size, color = log(size))
  )) +
  geom_sf(
    data = aarhus_roads, alpha = .5, color = "white", size = .125
  ) +
  geom_point(
    data = aarhus_buildings %>% drop_na(),
    aes(X, Y, size = size), color = "#5eb208", alpha = .125) +
  scale_size_area(max_size = 40) +
  scale_color_gradient2(low = "#d43b14", mid = "#d43b14", high = "#fcca45") +
  xlim(10.16, 10.22) +
  ylim(56.12, 56.18) +
  coord_sf(clip = "off") +
  labs(caption = "Aarhus | Data source: OpenStreetMap | #30daychartchallenge\n@emilmalta") +
  theme_void() +
  theme(text = element_text("MesmerizeScLt-Regular", colour = "#0b1428"),
    legend.position = "none", 
    plot.background = element_rect("#fbf5ed", NA), 
    plot.margin = margin(20,20,20,20)
  )
