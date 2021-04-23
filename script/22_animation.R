# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(countrycode)
library(gganimate)
library(janitor)
library(here)
library(ragg)
library(WDI)

# Import data ------------------------------------------------------------------

cellphones_raw <- 
  WDI(indicator = "IT.CEL.SETS.P2", start = 1960, end = 2019)

landlines_raw <- 
  WDI(indicator = "IT.MLT.MAIN.P2", start = 1960, end = 2019)

pop_world <-
  WDI(indicator = "SP.POP.TOTL", start = 1960, end = 2019) 


# Tidy -------------------------------------------------------------------------

df1  <- landlines_raw %>% 
  left_join(cellphones_raw) %>% 
  left_join(pop_world) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(continent = countrycode(iso2c, "iso2c", "continent")) %>% 
  filter(year >= 1980) %>% 
  group_by(country) %>% 
  complete(year) %>% 
  arrange(country, year) %>% 
  fill(it_cel_sets_p2, it_mlt_main_p2) %>% 
  ungroup()

# Plot setup -------------------------------------------------------------------

lbls <- tibble(
  year = 1980:2019, label = as.character(year),
  it_mlt_main_p2 = 53, it_cel_sets_p2 = 175
)


cols <- c(
  "Africa" = "#34a186",
  "Americas" = "#f9cb45",
  "Asia" = "#b5182b",
  "Europe" = "#4cb1c4",
  "Oceania" = "#ab96d2"
)

# Visualise --------------------------------------------------------------------

anim <- df1 %>% 
  arrange(desc(sp_pop_totl)) %>% 
  ggplot(aes(x = it_mlt_main_p2, y = it_cel_sets_p2, label = country)) +
  geom_point(
    data = . %>% filter(!is.na(continent)),
    aes(size = sp_pop_totl, fill = continent), 
    pch = 21, color = "white", alpha = .5
  ) +
  geom_point(
    data = . %>% filter(is.na(continent)), color = "white", shape = 5
  ) +
  geom_text(
    data = . %>% filter(is.na(continent), country != "Kosovo"), 
    family = "MesmerizeScLt-Regular",
    size = 3, check_overlap = TRUE, hjust = -.01, vjust = .5
  ) +
  geom_text(
    data = lbls, size = 18, family = "MesmerizeRg-Regular", hjust = 0,
    color = "#3a3e4c", aes(label = label)
  ) +
  scale_fill_manual(values = cols) +
  scale_size_area(max_size = 80) +
  coord_cartesian(xlim = c(0, 75), ylim = c(0, 200), clip = "off") +
  guides(size = "none") +
  theme_minimal(base_family = "MesmerizeRg-Regular", base_size = 35) +
  theme(
    text = element_text(color = "#3a3e4c"), 
    plot.background = element_rect(fill = "#f2ebe7", color = NA), 
    panel.grid = element_line(linetype = 3, color = "#c9c8c4"),
    plot.title.position = "plot", 
    plot.title = element_text(size = 100),
    plot.subtitle = element_text(family = "MesmerizeScLt-Regular"),
    legend.position = "bottom", plot.margin = margin(20,20,20,20)
  ) +
  guides(size = "none", fill = guide_legend(title = NULL, override.aes = list(size = 5))) +
  labs(title = "Rise of Cell Phones", 
       subtitle = "Cell phones are not only more convenient for the end user. They are also much easier to implement in\nregions with poor infrastructure. Countries with low income never saw the rise of landline phones, but\nachieved widespread use of cell phones within two decades",
       x = "\nLandline Subscriptions pr. 100 people",
       y = "Cell Phone Subscriptions pr. 100 people\n",
       caption = "Data Source: World Bank | #30daychartchallenge\n@emilmalta"
  ) +
  transition_time(year) +
  shadow_wake(wake_length = .1, exclude_layer = c(3, 4))

# Animate ----------------------------------------------------------------------

animate(
  anim, duration = 11, fps = 20, 
  width = 2000, height = 2000, res = 100
)
beepr::beep(3)
