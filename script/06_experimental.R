# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(tidygraph)
library(statgl)
library(ragg)

# Import data ------------------------------------------------------------------

migration_raw <- statgl_url("BEXBAF4B") %>%
  statgl_fetch(
    time = 2020, origin = px_all(), distination = px_all(), gender = c("M", "K"),
    .val_code = TRUE
  )

pop <- statgl_url("BEXSTD") %>% 
  statgl_fetch(time = 2020, locality = px_all(), .val_code = TRUE)

# Drawing ----------------------------------------------------------------------

migration_raw %>%
  as_tibble() %>%
  mutate(gender = case_when(gender == "M"~ "Men", gender == "K" ~ "Women")) %>% 
  filter(value > 0) %>%
  select(origin, distination, everything()) %>%
  as_tbl_graph() %>%
  mutate(mun = str_sub(name, end = 3), lbl = str_sub(name, -3)) %>%
  left_join(pop, c("name" = "locality")) %>% 
  arrange(value) %>% 
  ggraph("hive", axis = mun, normalize = FALSE, sort.by = "value") +
  geom_node_point(aes(size = value), fill = NA, color = "#3a3e4c", pch = 21, alpha = .7) +
  geom_edge_hive(aes(color = gender, edge_width = value, alpha = value),
                 strength = 15) +
  geom_node_text(aes(label = lbl), size = 3, nudge_x = .03, nudge_y = -.01, 
                 family = "MesmerizeCdEl-Regular", color = "#3a3e4c") +
  scale_edge_width_continuous(range = c(.3, 1.2)) +
  scale_size_area(max_size = 14) +
  scale_edge_alpha(range = c(.3,1)) +
  scale_edge_color_manual(values = c("Women" = "#7c9027", "Men" = "#ea754c")) +
  coord_fixed() +
  facet_wrap(~gender %>% fct_rev(), ncol = 1) +
  labs(title = "Migration", subtitle = "Greenland 2020", 
       caption = "Data source: Statistics Greenland | #30daychartchallenge\n@emilmalta") + 
  theme_void(base_family = "MesmerizeScLt-Regular", base_size = 16) +
  theme(
    plot.title = element_text(),
    plot.background = element_rect(fill = "#f0ece2", color = NA),
    plot.subtitle = element_text(size = 10), plot.margin = margin(20,20,20,20),
    text = element_text(color = "#3a3e4c"), 
    legend.position = "none", 
    strip.text = element_text(hjust = .65),
  )
