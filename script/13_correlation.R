# Load libraries ---------------------------------------------------------------

library(rnaturalearth)
library(shadowtext)
library(tidyverse)
library(patchwork)
library(scales)
library(here)
library(ragg)

# Helper function --------------------------------------------------------------

draw_path <- function(df, y_val) {
  df %>% 
    ggplot(aes(
      x = `Health Expenditure and Financing (per capita) (OECDstat (2017))`,
      y = {{ y_val }})
    ) +
    geom_path(alpha = .4, size = .5) +
    geom_path(data  = . %>% filter(Entity == "United States"), size = 1) +
    geom_point(data = . %>% filter(Year == max(Year)), alpha = .5) +
    geom_shadowtext(
      bg.color = "white",
      data  = . %>% filter(Year == max(Year)), family = "MesmerizeScLt-Regular",
      check_overlap = TRUE, color = "black", size = 3, nudge_y = -.0125)
}

# Import data ------------------------------------------------------------------

col_types <- cols(
  "Health Expenditure and Financing (per capita) (OECDstat (2017))" = 
    col_number()
)

health_expend_raw <- 
  here("data", "health_expenditure") %>% 
  list.files(full.names = TRUE) %>% 
  map(read_csv, na = c("..", ""), col_types = col_types) %>% 
  reduce(left_join) 

countries <-
  rnaturalearth::ne_countries(returnclass = "sf") %>%
  as.data.frame() %>%
  select(iso_a3, continent)

# Tidy -------------------------------------------------------------------------

health_expend_tidy <- health_expend_raw %>% 
  select(-Continent) %>% 
  left_join(countries, by = c("Code" = "iso_a3")) %>% 
  drop_na() %>% 
  arrange(Year, desc(`Total population (Gapminder, HYDE & UN)`))
  

# Visualise --------------------------------------------------------------------

y = `Life expectancy at birth, total (years)`

p1 <- health_expend_tidy %>% 
  draw_path(`Life expectancy at birth, total (years)`) +
  coord_cartesian(clip = "off", xlim = c(300, 8000), ylim = c(65,85)) +
  scale_y_continuous(labels = unit_format(accuracy = 1, unit = "")) +
  labs(
    x = NULL, title = "Health Expenditure (1970-2015)", 
    subtitle = "\nLife Expectancy (Years)"
  )

p2 <- health_expend_tidy %>% 
  draw_path(`Mortality rate, under-5 (per 1,000 live births)`) +
  coord_cartesian(clip = "off", xlim = c(300, 8000), ylim = c(2, 100)) +
  scale_y_log10() +
  labs(
    subtitle = "\nChild Mortality Rate (pr. 1,000 Live Births - Log Scale)",
    x = "\nHealth Expenditure pr. Capita\n(2010 Int. $ - Log Scale)",
    caption = "\nData Source: World Bank (via ourworldindata.org)\n#30daychartchallenge | @emilmalta"
  )

# Patch up ---------------------------------------------------------------------

cols <- c(
  "Asia" = "#61b568",
  "Europe" = "#6bc3e0",
  "North America" = "#cf4500",
  "Oceania" = "#a02c5d",
  "South America" = "#fca449"
)

p1 / p2  + 
  plot_layout(guides = "collect") &
  aes(size  = `Total population (Gapminder, HYDE & UN)`,
      group = Entity,
      label = Entity,
      color = continent
  ) &
  labs(y = NULL, color = NULL) &
  scale_color_manual(values = cols) & 
  scale_size_area(max_size = 15, guide = "none") &  
  scale_x_log10(labels = dollar) &
  theme(
    text = element_text("MesmerizeRg-Regular"),
    legend.position = "bottom", 
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.margin = margin(10, 15, 10, 5), 
    plot.title = element_text(size = 26),
    plot.title.position = "plot"
  ) 
