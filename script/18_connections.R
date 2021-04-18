# Load libraries ---------------------------------------------------------------

# This was a naíve approach, that only made it because it could run overnight.
# Don't run unless you really want to.

library(tidyverse)
library(tidygraph)
library(rnaturalearth)
library(edgebundle)
library(ggraph)
library(here)
library(sf)

# Import data ------------------------------------------------------------------

airports <- read_csv(here("data", "flights", "airports.csv"))
routes <- read_csv(here("data", "flights", "routes.csv"))

# Make graph -------------------------------------------------------------------

route_graph <- routes %>% 
  select(`Source airport`, `Destination airport`) %>% 
  as_tbl_graph() %>% 
  left_join(airports, by = c("name" = "IATA")) %>% 
  as.igraph()

coords <- 
  cbind(igraph::V(route_graph)$Longitude, igraph::V(route_graph)$Latitude)

force_bundle <- 
  edge_bundle_force(route_graph, coords, compatibility_threshold = .8)
beepr::beep(3)

# write_csv(force_bundle, here("output", "proc_data", "force_bundle_world.csv"))

# Visualize --------------------------------------------------------------------

coast_sf <- ne_countries(returnclass = "sf") %>% 
  filter(geounit != "Antarctica") %>% 
  st_set_crs(4326) %>% 
  st_transform(
    "+proj=vandg +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs"
  )

routes <- force_bundle %>% 
  drop_na() %>% 
  filter(x %>% between(-200, 200), y %>% between(-100, 100)) %>% 
  st_as_sf(crs = 4326, coords = c("x", "y")) %>% 
  st_transform(
    "+proj=vandg +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs"
  ) %>% 
  cbind(st_coordinates(.))

# Visualize --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = coast_sf, color = "#ff8e51", lty = 3, fill = NA, alpha = .25,)  +
  geom_path(
    data = routes, aes(X, Y, group = group), alpha = .025,
    color = "#fffbe9") +
  labs(
    caption = "Data Source: OpenFlights | #30daychartchallenge\n@emilmalta",
    x = "Flight Routes"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "MesmerizeRg-Regular", color = "#fffbe9"), 
    plot.background = element_rect("#8b2500", color = NA), 
    plot.margin = margin(b = 20, r = 20),
    axis.title.x = element_text(size = 48, margin = margin(20, b = 20))
  )
