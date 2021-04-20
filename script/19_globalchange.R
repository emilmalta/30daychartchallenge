# Load libraries ---------------------------------------------------------------

library(rnaturalearth)
library(tidyverse)
library(janitor)
library(scales)
library(statgl)
library(magick)
library(here)
library(sf)

options(scipen = 999)

# Import data ------------------------------------------------------------------

co2_world_raw <- 
  read_csv(here("data", "co2", "annual-co-emissions-by-region.csv")) %>% 
  clean_names()

world_raw <- ne_countries(scale = 50, returnclass = "sf") %>% 
  clean_names()

# Tidy -------------------------------------------------------------------------

world_sf <- world_raw %>% 
  select(iso_a3, continent) %>% 
  mutate(continent = case_when(
    continent == "Seven seas (open ocean)" ~ "Intl. Transport",
    T ~ continent
  )) %>% 
  drop_na()

co2_world <- co2_world_raw %>% 
  inner_join(world_sf, by = c("code" = "iso_a3")) %>% 
  mutate(
    continent = fct_reorder(continent, annual_co2_emissions, sum)
  ) %>% 
  arrange(continent, desc(annual_co2_emissions)) %>% 
  mutate(entity = fct_inorder(entity)) %>% 
  st_as_sf(crs = 4326) %>% 
  st_transform(
    "+proj=vandg +lon_0=0 +x_0=0 +y_0=0 +R_A +datum=WGS84 +units=m +no_defs"
  )

top_15 <- co2_world %>% 
  filter(year == 2019) %>% 
  st_centroid() %>% 
  cbind(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  mutate(entity = fct_lump(entity, 15, w = annual_co2_emissions)) %>% 
  filter(as.character(entity) != "Other")

# Visualise --------------------------------------------------------------------

p1 <- co2_world %>% 
  filter(year %in% c(1850, 1950, 2019)) %>% 
  st_centroid() %>% 
  cbind(st_coordinates(.)) %>% 
  st_drop_geometry() %>% 
  ggplot(aes(
    X, Y, size = annual_co2_emissions, color = continent, label = entity
  )) +
  geom_point(alpha = .75) +
  geom_text(
    data = top_15, size = 5, check_overlap = TRUE, color = "#fffbe9",
    family = "MesmerizeScLt-Regular"
  ) +
  scale_color_statgl(reverse = TRUE) +
  scale_size_area(max_size = 35) +
  facet_wrap(~ year, ncol = 1) +
  coord_fixed() +
  theme_void(base_size = 20) +
  theme(
    legend.position = "none", 
    plot.margin = margin(t = 90, l = 70), 
    strip.text = element_text(size = 20, margin = margin(15,15,15,15)),
    panel.border = element_rect(fill = NA, color = "#fffbe9"),
    text = element_text(family = "MesmerizeRg-Regular", colour = "#fffbe9"),
  ) 

p2 <- co2_world %>% 
  ggplot(aes(
    year, annual_co2_emissions, fill = continent, group = entity
  )) +
  geom_area(color = "#fffbe9", size = .1) +
  scale_fill_statgl(reverse = TRUE) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-9), position = "right") +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  coord_cartesian(xlim = c(1830, 2019)) +
  labs(
    x = NULL, y = NULL, 
    title = bquote(~CO[2]~"Emissions"),
    subtitle = "Billion Tonnes", 
    caption = "\nData Source: ourworldindata.org | #30daychartchallenge\n@emilmalta",
    fill = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "MesmerizeRg-Regular", colour = "#fffbe9"),
    plot.title = element_text(size = 40),
    plot.title.position = "plot",
    plot.subtitle = element_text(hjust = 1),
    plot.background = element_rect("#1b1404", color = NA), 
    plot.margin = margin(20,20,20,20), plot.caption.position = "plot",
    legend.position = "bottom", 
    legend.text = element_text(size = 10), 
    panel.grid = element_blank() 
  ) 

# Mosaic -----------------------------------------------------------------------

ggsave(here("output", "proc_data", "co2_points.png"), p1, 
       width = 10, height = 14, bg = "transparent")

ggsave(here("output", "proc_data", "co2_area.png"), p2, 
       width = 13, height = 14, bg = "transparent")

point_png <- image_read(here("output", "proc_data", "co2_points.png")) %>% 
  image_scale("2200") 
area_png <- image_read(here("output", "proc_data", "co2_area.png"))


image_mosaic(c(area_png, point_png))

