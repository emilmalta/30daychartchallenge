# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(gganimate)
library(magick)
library(statgl)
library(here)

# Import data ------------------------------------------------------------------

statgl_url("BEXSTC") %>% statgl_meta() %>% print(Inf)

pop_raw <- statgl_url("BEXSTC") %>% 
  statgl_fetch(
    district = px_all(), gender = c("M", "K"), age = px_all()) %>% 
  as_tibble() %>% 
  filter(!str_detect(district, "ommun|municip"))

pop <- pop_raw %>% 
  mutate(
    across(c(age, time), as.numeric),
    value = case_when(
      gender == "Men" ~ -value,
      gender == "Women" ~ value)
  ) %>% 
  replace_na(list(value = 0))

# Visualise --------------------------------------------------------------------

cols <- c(
  "Men" = "#fba465",
  "Women" = "#ffe5c4"
)

tot_anim <- pop %>% 
  filter(district == "Total") %>% 
  ggplot(aes(x = age, y = value, fill = gender, group = gender)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_manual(values = cols) +
  scale_y_continuous(labels = function(br) abs(br)) +
  coord_flip() +
  guides(fill = guide_legend(label.position = "bottom", ncol = 1,
                             title.theme = element_text(size = 8))) +
  labs(
    title = "Greenland Population", subtitle = "Age", 
    y = "\n{floor(frame_time)}", x = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 5) +
  theme(
    text = element_text(family = "MesmerizeRg-Regular", colour = "#fffbe9"),
    legend.position = "right", 
    plot.background = element_rect("#1b1404", color = NA), 
    axis.text = element_text(colour = "#fffbe9"),
    plot.title = element_text(size = 15),
    panel.grid = element_blank(), 
    axis.title.x = element_text(size = 10, margin = margin(10,5,5,5)),
    plot.margin = margin(10,10,10,10),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    panel.spacing.x = unit(2, "mm")
  ) +
  transition_time(time)

dis_anim <-pop %>% 
  filter(!district %in% c("Total")) %>% 
  ggplot(aes(x = age, y = value, fill = gender, group = gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = NULL, caption = "Data Source: Statistics Greenland | #30daychartchallenge\n@emilmalta") +
  facet_wrap(~district, scales = "free_x", ncol = 3) +
  theme_minimal(base_size = 5) +
  theme(
    text = element_text(family = "MesmerizeRg-Regular", colour = "#fffbe9"),
    legend.position = "none", 
    plot.background = element_rect("#1b1404", color = NA), 
    axis.text = element_text(colour = "#fffbe9", size = 4, family = "MesmerizeScLt-Regular"),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 15),
    panel.grid = element_blank(), 
    strip.text = element_text("MesmerizeRg-Regular", colour = "#fffbe9", size = 7),
    axis.title.x = element_text(size = 10, margin = margin(5,5,5,5)),
    plot.margin = margin(10,10,10,10),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm")
  ) +
  scale_fill_manual(values = cols) +
  scale_y_continuous("",
    labels = function(br) abs(br)
  ) +
  transition_time(time) 
  
# Save gifs --------------------------------------------------------------------

a_gif <- 
  animate(tot_anim, duration = 10, fps = 20, width = 700, height= 1000, res = 200)
beepr::beep(3)

b_gif <- 
  animate(dis_anim, duration = 10, fps = 20, width = 500, height= 1000, res = 200)
beepr::beep(3)