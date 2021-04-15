# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(ggforce)
library(here)

# Reading material
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7757576/
# http://www.hzgallery.org/index.html
# https://exoplanetarchive.ipac.caltech.edu/

options(scipen = 999)

# Import data

planets_raw <- read_csv(here("data", "planets/", "exoplanets.csv"))
pl_archive <- read_csv(here("data", "planets", "planet_archive.csv"), skip = 100)

# Visualise --------------------------------------------------------------------

pl_archive %>% 
  left_join(planets_raw, by = c("pl_name" = "PLANET")) %>% 
  arrange(desc(pl_rade)) %>% 
  ggplot(aes(x = pl_insol, y = st_teff, fill = THZC, label = pl_name)) +
  scale_fill_binned(type = "viridis") +
  geom_point(aes(size = pl_rade), pch = 21, color = "white") +
  geom_text(check_overlap = TRUE) +
  scale_size_area(max_size = 40, guide = "none") +
  scale_x_continuous(limits = c(6500, .1), trans = trans_reverser('log10')) +
  ylim(3000, 7000) +
  theme_dark() +
  labs(title = "Exoplanets") +
  theme(
    legend.position = "bottom"
  )
