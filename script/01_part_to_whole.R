# Load libraries ---------------------------------------------------------------

library(here)
library(ggraph)
library(igraph)
library(statgl) 
library(extrafont)
library(tidyverse)

# Import data ------------------------------------------------------------------

# Statistics Greenland API
import_export_raw <- statgl_url("IEXSITC") %>% 
  statgl_fetch(time = 2019, quarter = 0, .eliminate_rest = FALSE) %>% 
  select(-c(quarter, time)) %>% 
  replace_na(list(value = 1)) %>% 
  pivot_wider(names_from = "transaction") %>% 
  as_tibble()

# Tidy -------------------------------------------------------------------------

# Tidied
import_export <- import_export_raw %>% 
  filter(!str_detect(processing, "\\d-\\d")) %>% 
  separate(processing, c("id", "text"), sep = " ", extra = "merge", convert = T)

# Categories
cats <- import_export_raw %>% 
  filter(str_detect(processing, "\\d-\\d"), !str_detect(processing, "Total")) %>% 
  extract(
    processing, c("lo", "hi", "cat"), "(\\d+)-(\\d+) (.+), total", 
    convert = TRUE
  ) %>% 
  group_by(cat) %>% 
  summarise(id = seq(from = lo, to = hi), .groups = "drop") %>% 
  arrange(id)

# Build Tree -------------------------------------------------------------------

leaves <- import_export %>% left_join(cats) %>% 
  select(group = cat, subitem = text, size = import)

df <- tibble(group = "Root", subitem = unique(cats$cat), size = 0) %>% 
  bind_rows(leaves) %>% 
  mutate(size = size + 1)

vertices <- df %>% 
  distinct(subitem, size) %>% 
  add_row(subitem = "Root", size = 1)

# Manually punch in categories -------------------------------------------------

# vertices %>% 
#   write_csv2(here("data", "01_vertices_import.csv"))

vertices <- read_csv2(here("data", "01_vertices_import_edited.csv")) 

# Draw -------------------------------------------------------------------------

graph <- graph_from_data_frame(df, vertices = vertices) 
set.seed(2021)
ggraph(graph, 'circlepack', weight = size) +
  geom_node_circle(
    aes(fill = size), 
    size = 0.25, alpha = .5, color = "#eccd86"
  ) +
  geom_text(
    aes(x = x, y = y, label = lbl, size = size),
    check_overlap = FALSE, show.legend = FALSE, color = "#212e53"
  ) +
  scale_size_continuous(range = c(1.125, 7)) +
  scale_fill_gradient2(
    low = "white",mid = "#FAFAFA", high = "#ebaca2", 
    labels = scales::comma, midpoint = 500000000
  ) +
  coord_fixed() +
  labs(
    title = "\nMain Imports of Greenland\n", fill = "DKK",
    subtitle = "\n2019\n",
    caption = "\n\nSource: Statistics Greenland | #30daychartchallenge\n@emilmalta"
  ) +
  theme_void(base_family = "MesmerizeRg-Regular") +
  theme(
    plot.title = element_text(size = 36, hjust = .5, color = "#212e53"), 
    plot.subtitle = element_text(hjust = .5), 
    plot.background =  element_rect(fill = "#FAFAFA", color = NA), 
    plot.caption = element_text(hjust = .5),
    plot.margin = margin(25,25,25,25), 
    panel.background = element_rect(color = NA, fill = "#EFE9DB"), 
    legend.position = "bottom", 
    legend.key.width = unit(2, "cm")
  )

