# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gggibbous)
library(extrafont)
library(suncalc)
library(here)

# Make data --------------------------------------------------------------------

moon_df <- tibble(
  date = seq(ymd("2021-01-01"), ymd("2021-12-31"), by = 1),
  month = month(date, label = TRUE),
  moon = getMoonIllumination(date),
)

# Visualise --------------------------------------------------------------------

moon_df %>% 
  ggplot(aes(month, y = day(date))) +
  geom_moon(
    aes(ratio = moon$fraction, fill = moon$fraction, right = moon$phase < .5), 
    color = "white", alpha = .85, stroke = .01, size = 15) +
  geom_text(aes(label = day(date)), size = 2, color = "#FEFEE1") +
  scale_fill_gradient(low = "#fdc07B", high = "#fdd38c") +
  scale_y_reverse() +
  labs(title = "Lunar Calendar", subtitle = "2021", 
       x = NULL, y = NULL,
       caption = "\n#30daychartchallenge\n@emilmalta") +
  theme_minimal(base_family = "MesmerizeRg-Regular", base_size = 15) +
  theme(
    legend.position = "none", axis.text.y.left = element_blank(),
    text = element_text(color = "white"), 
    plot.margin = margin(30,30,30,30), 
    plot.background = element_rect("#3f3f4e"), 
    panel.grid = element_blank(),
    strip.text = element_text(color = "white", hjust = 0), 
    axis.text = element_text(color = "white")
  )
