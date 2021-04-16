# Load libraries ---------------------------------------------------------------

# Reading material
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7757576/
# http://www.hzgallery.org/index.html
# https://exoplanetarchive.ipac.caltech.edu/

library(shadowtext)
library(tidyverse)
library(ggforce)
library(ggtext)
library(here)
library(ragg)

options(scipen = 999)

# Import data ------------------------------------------------------------------

planets_raw <- read_csv(here("data", "planets", "table.csv"))
pl_archive <- read_csv(here("data", "planets", "planet_archive.csv"), skip = 100)

# Tidy -------------------------------------------------------------------------

planets_tidy <- pl_archive %>% 
  select(pl_name, pl_insol, st_teff, pl_rade) %>% 
  left_join(planets_raw, by = c("pl_name" = "PLANET")) %>% 
  arrange(desc(pl_rade)) %>% 
  drop_na(THZO, pl_rade, pl_insol, st_teff)

# Visualise --------------------------------------------------------------------

planets_tidy %>% 
  ggplot(aes(
    x = pl_insol, y = st_teff, fill = THZO, label = pl_name, size = pl_rade
  )) + 
  geom_point(
    data = . %>% filter(THZO == 0), 
    pch = 21, fill = "#48433A", color = "lightgrey"
  ) +
  geom_point(
    data = . %>% filter(THZO != 0), 
    aes(fill = THZO), pch = 21, color = "lightgrey"
  ) +
  geom_text(
    data = . %>% filter(THZO == 0),
    size = 2, check_overlap = TRUE, alpha = .5, color = "white",
    family = "MesmerizeScLt-Regular"
  ) +
  geom_shadowtext(
    data = . %>% filter(THZO != 0),
    size = 3, check_overlap = TRUE,
    family = "MesmerizeScLt-Regular", color = "white", nudge_y = -75
  ) +
  annotate(
    geom = "point", y = 5780, x = 1, size = 1, 
    fill = "red", color = "white", pch = 21
  ) +
  annotate(
    geom = "text", y = 5900, x = .7, size = 3, label = "Earth", color = "red",
  ) +
  annotate(
    geom = "text", y = 8900, x = 800, size = 5, color = "white",
    family = "MesmerizeScLt-Regular",
    label = "The Habitable Zone (THZ) is the range of distances from a star,\nwhere a planet could have liquid water on the surface. Colored\ndots in this graph show planets estimated to spend at least\nsome time in THZ. These are optimistic values, based on\nresearch by Kopparapu et al. (2014).",
    hjust = 0
  ) +
  scale_fill_continuous(breaks = c(25, 50, 75, 100), type = "viridis") +
  scale_x_continuous(trans = trans_reverser('log10')) +
  scale_y_continuous() +
  scale_radius(range = c(0, 24)) +
  guides(
    size = "none", 
    fill = guide_legend(title.position = "left", override.aes = list(size = 10))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    x = "\nInsolation Flux [Earth Flux]", y = "Stellar Effective Temperature [K]\n",
    title = "Exoplanets", caption = "#30daychartchallenge | @emilmalta\nData Source: Habitable Zone Gallery & NASA Exoplanet Archive\n",
    fill = "Pct. of Orbit Spent in Optimistic Habitable Zone:"
  ) +
  theme(
    text = element_text(family = "MesmerizeRg-Regular", color = "white"), 
    plot.background = element_rect("#1b1404", color = NA), 
    panel.grid = element_line(linetype = 3, color = "#48433A"),
    plot.title = element_text(size = 40),
    legend.position = "top", legend.key = element_blank(), 
    legend.justification = 0, 
    legend.margin = margin(l = 0),
    title = element_text(margin = margin(l = 0)),
    plot.margin = margin(20,20,20,20), legend.title = element_text(size = 15),
    legend.background = element_blank(),
    panel.background = element_blank()
  )
