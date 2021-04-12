# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(janitor)
library(scales)
library(here)
library(ragg)

# Import data ------------------------------------------------------------------

aarhus_weather <- list.dirs(
  here("data", "aarhus_weather"), full.names = TRUE, recursive = FALSE
  ) %>% 
  map(list.files, full.names = TRUE) %>% 
  map(map_dfr, read_csv2, col_types = cols(
    "DateTime" = col_datetime(), .default = col_double()
    )) %>% 
  reduce(left_join, by = "DateTime") %>% 
  clean_names()

# Helper function --------------------------------------------------------------

plot_strip <- 
  function(df, column, fill_low = "#1b1404", fill_mid = 0, fill_max, subtitle) {
    df %>% 
      ggplot(aes(date_time, 1, fill = {{ column }})) +
      scale_fill_gradient2(
        low = fill_low, mid = "#1b1404", midpoint = fill_mid, high = fill_max
      ) +
      labs(subtitle = subtitle) 
  }

# Declare titles and scales for each group -------------------------------------

plt_list <- list(
  aarhus_weather %>% plot_strip(
    solskin, fill_max = "#f7e500", subtitle = "Sunshine (Hours)"
  ) +
    labs(title = "Weather in Aarhus, Denmark: 2020\n"),
  
  aarhus_weather %>% plot_strip(
    middel, fill_mid = 5, fill_max = "#ff8e51", subtitle = "Temperature (°C)"
  ),
  
  aarhus_weather %>% plot_strip(
    lufttryk, fill_mid = 1000, fill_max = "#bccdd9", subtitle = "Air pressure (hPa)"
  ),
  
  aarhus_weather %>% plot_strip(
    middel_vindhastighed, fill_mid = 4, fill_max = "#bccdd9", subtitle = "Medium Wind Speed (m/s)"
  ),
  
  aarhus_weather %>% plot_strip(
    luftfugtighed, fill_mid = 70, fill_max = "#48abd8", subtitle = "Humidity (%)"
  ),
  
  aarhus_weather %>% plot_strip(
    nedbor, fill_max = "#136fa4", subtitle = "Precipitation (mm)"
  ),
  
  aarhus_weather %>% plot_strip(
    lyn, fill_max = "#f6f0d9", subtitle = "Lightning"
  ),
  
  aarhus_weather %>% replace_na(list(maks_snedybde = 0)) %>% plot_strip(
    maks_snedybde, fill_max = "#f6f0d9", subtitle = "Max depth of snow (cm)"
  ) +
    labs(caption = "\nData Source: DMI | #30daychartchallenge\n@emilmalta")
)

# Patch up ---------------------------------------------------------------------

wrap_plots(plt_list, ncol = 1) & 
  geom_tile() &
  labs(fill = NULL, y = NULL, x = NULL) &
  scale_x_datetime(date_labels = "%b") &
  theme(
    text = element_text(family = "MesmerizeRg-Regular", color = "white"), 
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(hjust = .5), 
    plot.background = element_rect(fill = "#1b1404", color = NA), 
    plot.caption.position = "plot",
    plot.margin = margin(10,10,10,10),
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    legend.text = element_text(size = 6),
    legend.key.width = unit(1, "mm"), 
    legend.key.height = unit(3, "mm"), 
    legend.background = element_blank(), 
    axis.ticks = element_blank(),
    axis.text.y = element_blank()
  ) 