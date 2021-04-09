# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(ggrepel)
library(readxl)
library(here)
library(glue)
library(ragg)

# Import data ------------------------------------------------------------------

election_municipality_raw <- 
  read_csv(here("data", "gl_election", "mun_election.csv")) %>% 
  fill(municipality)

# Tidy -------------------------------------------------------------------------

election_results <- 
  election_municipality_raw %>% 
  count(party, year, wt = votes, name = "votes")

election_percent <- election_results %>%
  group_by(year) %>%
  mutate(votes = votes / sum(votes)) %>%
  filter(party != "Others")

election_municipality <- election_municipality_raw %>%
  group_by(year, municipality) %>%
  mutate(votes = votes / sum(votes)) %>%
  filter(party != "Others")

# Draw plots -------------------------------------------------------------------

draw_slope <- function(df) {
  df %>%
    ggplot(aes(as.character(year), votes, color = party, group = party)) +
    geom_line(size = 1.5, alpha = .03) +
    geom_line(
      data = . %>% filter(party %in% c("Inuit Ataqatigiit", "Demokraatit")),
      size = 1.5, alpha = .7
    )
}

# National
p1 <- election_percent %>% draw_slope() +
  geom_text_repel(
    aes(label = glue("{party}: {format(votes * 100, digits = 2)}%")),
    data = . %>% filter(year == 2018), hjust = 1, nudge_x = -.07,
    segment.color = NA, size = 3, family = "MesmerizeRg-Regular"
  ) +
  geom_text_repel(
    aes(label = glue("{format(votes * 100, digits = 2)}%")),
    data = . %>% filter(year == 2021), hjust = 0, nudge_x = .07,
    segment.color = NA, size = 3, family = "MesmerizeRg-Regular"
  ) +
  labs(
    title = "2021 Greenlandic General Election",
    subtitle = "An Arctic mining project in Southern Greenland took centre stage in the 2021 election for Inatsisartut.\nInuit Ataqatigiit, a left wing party who has opposed uranium mining for decades, won a clear victory.\nDemokraatit, who were optimistic about the mining project, suffered a devastating loss.\n"
  )

# By municipality
p2 <- election_municipality %>% draw_slope() +
  geom_text_repel(
    aes(label = glue("{format(votes * 100, digits = 3)}%")),
    data = . %>%
      filter(year == 2018, party %in% c("Demokraatit", "Inuit Ataqatigiit")),
    hjust = 0, nudge_x = -.07, size = 2, segment.color = NA,
    family = "MesmerizeRg-Regular"
  ) +
  geom_text_repel(
    aes(label = glue("{format(votes * 100, digits = 2)}%")),
    data = . %>%
      filter(year == 2021, party %in% c("Demokraatit", "Inuit Ataqatigiit")),
    hjust = 1, nudge_x = .07, size = 2, segment.color = NA,
    family = "MesmerizeRg-Regular"
  ) +
  facet_wrap(~ municipality, nrow = 1) +
  labs(
    subtitle = "\nThe project was set to be in Kujalleq Municipality, where IA won nearly half the vote.\n",
    caption = "Data source: http://qinersineq.gl | #30daychartchallenge\n@emilmalta"
  )

# Patchwork --------------------------------------------------------------------

# Assign party colors
party_colors <- c(
  "Atassut" = "#231a95", "Demokraatit" = "#016738",
  "Inuit Ataqatigiit" = "#b92732", "Naleraq" = "#ff9d00",
  "Nunatta Qitornai" = "#a03499", "#0175b2",
  "Suleqatigiissitsisut" = "#311279"
)

# Patchwork
(p1 / p2) + 
  plot_layout(heights = c(5,2)) &
  geom_line(size = 1.5, alpha = .12) &
  geom_point(size = 3, pch = 21, fill = "white", stroke = 1.5) &
  scale_color_manual(values = party_colors) &
  labs(x = NULL, y = NULL, color = NULL) &
  theme_minimal(base_family = "MesmerizeRg-Regular") +
  theme(
    plot.background = element_blank(),
    plot.title = element_text(hjust = .5, size = 34), 
    plot.subtitle = element_text(hjust = .5), 
    plot.caption = element_text(hjust = .5, margin = margin(30)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line("darkgrey"),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(), axis.title.x = element_text()
  ) 
