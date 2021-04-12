# Laod libraries ---------------------------------------------------------------

library(tidyverse)
library(tidygraph)
library(tidytext)
library(ggraph)
library(ragg)
library(here)

# Import data ------------------------------------------------------------------

pi <- tibble(
  pi = read_lines(here("data", "pi")) %>% str_remove("\\.")
) 

# Graph ------------------------------------------------------------------------

pi_graph <- pi %>% 
  unnest_tokens(digit, pi, "characters") %>%
  mutate(from = paste(digit, row_number()), to = lead(from)) %>% 
  drop_na() %>% 
  as_tbl_graph() %>% 
  mutate(digit = str_sub(name, end = 1L)) %>% 
  arrange(name) 

# Visualise --------------------------------------------------------------------

cols <- c(
  "0" = "#d9d3c5",
  "1" = "#fcca45",
  "2" = "#e56e0a",
  "3" = "#e05a3f",
  "4" = "#b5142d",
  "5" = "#136fa4",
  "6" = "#4db3c2",
  "7" = "#136fa4",
  "8" = "#31a285",
  "9" = "#778530"
)

pi_graph %>% 
  ggraph("linear", circular = TRUE) +
  geom_edge_link(aes(color = digit), alpha = .25, show.legend = FALSE) +
  geom_node_point(aes(color = digit), size = 2) +
  annotate(
    geom = "text", x = 0, y = -1.25, label = "π", 
    family = "Baskerville", size = 64, color = "white"
  ) +
  scale_edge_color_manual(values = cols)  +
  scale_color_manual(values = cols) +
  expand_limits(y = -1.5) +
  coord_fixed() +
  labs(color = NULL, caption = "#30daychartchallenge\n@emilmalta") +
  guides(color = guide_legend(nrow = 1, label.position = "bottom")) +
  theme_void(base_family = "MesmerizeRg-Regular") +
  theme(
    legend.position = "bottom", 
    plot.background = element_rect("#231d1f", color = NA), 
    plot.margin = margin(20,20,20,20),
    text = element_text(color = "white")
  )
