# Load libraries ---------------------------------------------------------------

library(ggraph)
library(igraph)
library(statgl)
library(tidyverse)

# Import data ------------------------------------------------------------------

import_export_raw <- statgl_url("IEXSITC") %>% 
  statgl_fetch(time = 2019, quarter = 0, .eliminate_rest = FALSE) %>% 
  as_tibble()

# Tidy -------------------------------------------------------------------------

# Define categories
cats <- import_export_raw %>% 
  filter(
    str_detect(processing, "\\d-\\d"), !str_detect(processing, "Total")
  ) %>% 
  pivot_wider(names_from = "transaction") %>% 
  extract(
    processing, c("lo", "hi", "cat"), "(\\d+)-(\\d+) (.+), total", 
    convert = TRUE
  ) %>% 
  complete(lo = min(lo):max(lo)) %>% 
  fill(cat) %>% 
  select(lo, cat)

# Tidy data
import_export <- import_export_raw %>% 
  filter(!str_detect(processing, "-")) %>% 
  separate(processing, c("lo", "det_text"), convert = TRUE, extra = "merge") %>% 
  unite(text1, det_text, transaction, sep = ".") %>% 
  left_join(cats)

# Treeify ----------------------------------------------------------------------

df <- 
  import_export %>% 
  filter(str_detect(text1, "export$")) %>% 
  select(cat, text1, size = value) %>% tidyr::drop_na()

root <- df %>% 
  distinct(cat) %>% mutate(root = "root") %>% rename(text1 = cat, cat = root)

df <- df %>% 
  bind_rows(root) %>% 
  tidyr::replace_na(list(size = 0))

vertices <- df %>% distinct(text1, size) %>% 
  mutate(labl = "test") %>% 
  add_row(text1 = "root", size = 0)

# TODO: Beter labels (like)
# c("Live Animals (Not Fish)", "Meat", "Dairy and Eggs", "Fish", "Cereal", "Vegetables and Fruit", "Sugar", "Coffee, Tea, Cocoa, Spices", "Fodder", "Misc. Edible", "Beverages", "Tobacco", "Hides and Skins", "Oil Seeds", "Crude rubber", "Cork and Wood", "Pulp", "Textile Fibers", "Crude Fertilizers", "Metal Ores and Scrap", "Crude Animal/Vegetable materials", "Coal", "Petroleum", "Gas", "Animal Oil and Fats", "Vegetable Oil and Fats", "Processed Oils (Animal+Veg)", "Organic Chemicals", "Inorganic Chemicals", "Dye", "Pharmaceutical", "Perfume Materials", "Fertilizer", )

# Visualise --------------------------------------------------------------------

df %>% 
  graph_from_data_frame(vertices = vertices) %>% 
  ggraph(layout = "circlepack", weight = size) + 
  geom_node_circle(aes(color = size), alpha = .5) +
  geom_text(aes(x = x, y = y, label = paste(labl)), check_overlap = TRUE, size = 3) +
  geom_point(aes(x = x, y = y), size = 1) +
  coord_fixed() +
  theme_void()
