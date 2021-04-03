# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)
library(janitor)
library(here)

# Import data ------------------------------------------------------------------

# Fixed width file
read_sun <- function(filename) {
  read_fwf(
    filename,
    fwf_positions(
      start = c(1, 5, 7, 9, 13, 21, 23, 25, 26, 30, 31, 35, 36, 40, 41, 45, 46,
                51, 52, 57, 58, 63, 64, 69, 70),
      end   = c(4, 6, 8, 12, 20, 22, 24, 25, 29, 30, 34, 35, 39, 40, 44, 45, 50,
                51, 56, 57, 62, 63, 68, 69, 74),
      col_names = c("year", "month", "day", "time", "sunspot_group", 
                    "group_suffix","group_type", "blank_1", "umbral_area", 
                    "blank_2", "whole_spot_area", "blank_3", 
                    "umbral_area_corrected", "blank_4", 
                    "whole_spot_area_corrected", "blank_5", "dist_from_center", 
                    "blank_6", "position_angle", "blank_7",
                    "carrington_lon", "blank_8", "lat", "blank_9", 
                    "central_meridian_dist")
    ), 
    col_types = cols(.default = col_number())
  )
}

# Look all files up and smush
df <- 
  list.files(here("data", "sunspots"), full.names = TRUE) %>% 
  map_df(read_sun) %>% 
  remove_empty("cols") %>% 
  mutate(date = make_date(year, month, day), lat = round(lat, 1))

# Drawing ----------------------------------------------------------------------

p1 <- df %>% 
  ggplot(aes(x = date, y = lat)) +
  geom_point(size = .5) +
  geom_hline(yintercept = 0, lty = 3) +
  scale_x_date(
    date_labels = "%Y",
    breaks = seq(as.Date("1877-01-01"), as.Date("1902-12-31"), by = "1 year"),
    limits = c(as.Date("1876-01-01"), as.Date("1904-01-01")),
    sec.axis = dup_axis(), 
    expand = c(.05,.05)
  ) +
  scale_y_continuous(
    breaks = seq(-40, 40, by = 10), 
    sec.axis = dup_axis(), 
    labels = scales::unit_format(unit = "°", sep = "")
  ) +
  labs(y = NULL, x = NULL) +
  coord_cartesian(clip = "off") +
  annotate(
    geom="segment", y = seq(-45, 45, 1), yend = seq(-45, 45, 1),
    x = as.Date("1876-01-01"), xend = as.Date("1876-01-30")
  ) +
  annotate(
    geom="segment", y = seq(-45, 45, 1), yend = seq(-45, 45, 1),
    x = as.Date("1903-12-01"), xend = as.Date("1903-12-30")
  ) +
  theme_grey(base_family = "Baskerville") +
  theme(
    plot.background = element_rect("#fefee1"),
    panel.grid = element_blank(), plot.margin = margin(30,30,30,30),
    panel.background = element_rect(fill = NA, color = "black"),
    axis.text.x = element_text(hjust = -.25),
    axis.text.y.left  = element_text(vjust = -1, margin = margin(l = 10, r = -10)),
    axis.text.y.right = element_text(vjust = -1, margin = margin(l = -10, r = 10)),
    axis.ticks.length.x = unit(-.75, "cm"), 
    axis.ticks.length.y = unit(-1.95, "cm"), 
    plot.caption.position = "plot"
  )

p2 <- ggplot() +
  labs(title = "DISTRIBUTION OF SPOT-CENTRES IN LATITUDE, ROTATION BY ROTATION, 1877-1902") +
  theme_void(base_family = "Baskerville") +
  theme(plot.title = element_text(hjust = .5),
        plot.background = element_rect("#fefee1", color = NA),
)
  
  
p1/p2 + plot_layout(heights = c(15,1))
