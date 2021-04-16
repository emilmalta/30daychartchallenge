# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(trekcolors)
library(ggforce)
library(scales)
library(statgl)
library(here)
library(ragg)

# Import data ------------------------------------------------------------------

statgl_search("Qinngorput")

# Impport data -----------------------------------------------------------------

nuuk_raw <- statgl_url("BEXSTNUK") %>% statgl_fetch(.eliminate_rest = FALSE)

# Tidy -------------------------------------------------------------------------

nuuk <- nuuk_raw %>% 
  as_tibble() %>% 
  filter(across(where(is.character), ~!str_detect(.x, pattern = "^Total"))) %>% 
  mutate(
    across(c(time, age), as.numeric),
    citydistrict = case_when(
      citydistrict == "Nuuk" ~ "Nuuk (Downtown)", T ~ citydistrict) %>% 
      fct_reorder(-value, sum),
    `place of birth` = paste("Place of Birth:", `place of birth`)
  ) %>% 
  uncount(value)

# Visualise --------------------------------------------------------------------

nuuk %>% 
  filter(time == max(time)) %>% 
  ggplot(aes(x = gender, y = age, color = gender)) +
  geom_sina(maxwidth = .7, size = .5, alpha = .8) +
  scale_y_continuous(labels = unit_format(unit = "Years")) +
  scale_color_manual(
    values = c("Men" = "#083248", "Women" = "#8c0e0f")
  ) +
  facet_grid(citydistrict ~ `place of birth`) +
  theme_minimal(base_family = "MesmerizeRg-Regular") +
  labs(
    x = "2021", y = NULL, 
    title = "Nuuk",
    subtitle = "Population Structure of Nuuk's City Districts",
    caption = "Data Source: Statistics Greenland | #30daychartchallenge\n@emilmalta") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 48),
    plot.subtitle = element_text(margin = margin(b = 20)),
    plot.caption.position = "plot",
    plot.background = element_rect("#eae7e6", color = NA), 
    plot.margin = margin(20,20,20,20),
    legend.position = "none", 
    panel.grid = element_blank(), 
    panel.background = element_rect(fill = "#dfdfdf", color = "white"),
    axis.title.x = element_text(size = 24, margin = margin(20, b = 20))
   )

  # Save -------------------------------------------------------------------------

ggsave(
  here("output", "plots", "15_multivariate.png"), 
  device = "png", width = 7.5, height = 15
)
