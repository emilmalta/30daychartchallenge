# Load libraries ---------------------------------------------------------------

library(BasketballAnalyzeR)
library(tidyverse)
library(lubridate)
library(here)
library(ragg)

# Raw --------------------------------------------------------------------------

# nba_shots <- read_csv("NBA Shot Locations 1997 - 2020.csv")
# nba <- nba_shots %>%
#   mutate(`Game Date` = ymd(as.character(`Game Date`))) %>%
#   select(`X Location`, `Y Location`, `Shot Made Flag`, `Game Date`) %>%
#   filter(`Game Date` %>% between(ymd("2018-10-16"), ymd("2019-06-13"))) %>%
#   mutate(`Shot Made Flag` = case_when(
#     `Shot Made Flag` == 1 ~ "Shots that made it",
#     `Shot Made Flag` == 0 ~ "Shots that didn't",
#   ))
# write_csv(nba, here("data", "nba", "nba.zip"))

# Import data ------------------------------------------------------------------

nba <- read_csv(here("data", "nba", "nba.zip"))

# Visualise --------------------------------------------------------------------

# Define colors
types <-
  c("Shots that didn't" = c("#302836"), "Shots that made it" = "#de731d")

# Draw distribution
p <- nba %>%
  ggplot(aes(x = `X Location` / 10, y = `Y Location` / 10 - 41.5)) +
  geom_point(aes(color = `Shot Made Flag`), alpha = .65, size = .1) +
  facet_wrap(~ fct_rev(factor(`Shot Made Flag`)), strip.position = "bottom") +
  coord_fixed() +
  theme_void(base_family = "Tw Cen MT Bold Italic", base_size = 24) +
  labs(
    title = "Every Shot in the NBA", subtitle = "Season 2018/19",
    caption = "\nData Source: Sports Viz Sundays (through data.world)\n#30daychartchallenge | @emilmalta"
  ) +
  scale_color_manual(values = types) +
  scale_y_reverse() +
  theme(
    plot.background = element_rect(fill = "#f8e9d6", color = NA),
    plot.margin = margin(15,15,15,15),
    legend.position = "none", axis.text = element_blank(),
  ) +
  NULL

# Draw court
p %>%
  drawNBAcourt(size = .5, col = "#f8e9d6")
