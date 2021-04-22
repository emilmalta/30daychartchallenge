# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(babynames)
library(scales)
library(ragg)

options(scipen = 999)

# Import data ------------------------------------------------------------------

emil_nikki <- babynames %>% 
  filter(name %in% c("Emil", "Nikki"))

# Visualise --------------------------------------------------------------------

cols <- c(
  "Female" = "#b5142d",
  "Male" = "#f4a92a"
)


emil_nikki %>% 
  mutate(sex = case_when(
    sex == "F" ~ "Female", sex == "M" ~ "Male"
  )) %>% 
  ggplot(aes(year, prop, color = sex, lty = name)) + 
  geom_line(size = 1.5) +
  labs(
    y = NULL, x = NULL,
    title = "Emil Nikki",
    subtitle = "Proportion of babies named 'Emil' or 'Nikki' in the United States",
    lty = NULL, color = NULL, caption = "\nData Source: US SSA (Through 'babynames' R Package)\n#30daychartchallenge | @emilmalta"
  ) +
  scale_y_continuous(labels = percent_format(accuracy = .01)) +
  scale_color_manual(values = cols) +
  guides(lty = guide_legend(order = 1)) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "MesmerizeRg-Regular", color = "#3a3e4c"), 
    plot.background = element_rect(fill = "#f2ebe7", color = "#3a3e4c", size = 10), 
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom", 
    legend.key.width = unit(1.5, "cm"), 
    plot.title.position = "plot",
    plot.margin = margin(50,50,50,50), 
    panel.grid = element_line(color ="#fffbe9"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(), plot.caption.position = "plot"
  )

