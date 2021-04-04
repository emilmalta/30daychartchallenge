# Load libraries ---------------------------------------------------------------

library(paletteer)
library(wesanderson)
library(ggbeeswarm)
library(tidyverse)
library(tidytext)
library(ggforce)
library(rvest)
library(here)
library(ragg)

library(extrafont)
font_import(paths = "data/Mountains_of_Christmas")

loadfonts()

# Import data ------------------------------------------------------------------

# Gather all the books
hp_books <- list(
  philosophers_stone   = philosophers_stone,
  chamber_of_secrets   = chamber_of_secrets,
  prisoner_of_azkaban  = prisoner_of_azkaban,
  goblet_of_fire       = goblet_of_fire,
  order_of_the_phoenix = order_of_the_phoenix, 
  half_blood_prince    = half_blood_prince,
  deathly_hallows      = deathly_hallows
) %>% 
  enframe(name = "book", value = "text") %>% 
  mutate(
    book = book %>% 
      str_replace_all("_", " ") %>% str_to_title() %>% fct_inorder()
  ) %>% 
  unnest(text) %>% 
  group_by(book) %>% 
  mutate(chapter = row_number()) %>%
  ungroup() 

# Scrape the spells 
url <- "https://www.pojo.com/harry-potter-spell-list/"
selector <- "#td-outer-wrap > div.td-main-content-wrap.td-container-wrap > div > 
   div.td-pb-row > div.td-pb-span8.td-main-content > div > 
   div.td-page-content.tagdiv-type > table"

spells_raw <- url %>%
  read_html() %>% html_nodes(selector) %>% html_table(header = TRUE) %>% 
  .[[1]] %>% as_tibble()

# Saved the spells, just in case
# write_csv(spells_raw, here("data", "hp", "spells.csv"))

# Tidy -------------------------------------------------------------------------

# Get the spells
spell_library <- spells_raw %>%
  mutate(
    Incantation = 
      Incantation %>% str_replace_all("-", " ") %>% str_to_lower(),
    Type = 
      factor(Type, levels = c("Enchanment", "Charm", "Jinx", "Spell", "Curse"))
  ) %>% 
  # Some of these things are basically just verbs...
  filter(
    !Incantation %in% c("", "—", "cheering", "flying", "pack", "gripping")
  )

# Smush all the 1-2-3 grams of the books
ngrams <- map_df(1:3, ~ {
  hp_books %>% 
    unnest_tokens(Incantation, text, token = "ngrams", n = .x) %>% 
    group_by(book) %>% 
    mutate(pos = row_number()) %>% 
    ungroup()
  }
) 

# Tidy spells
spells <- spell_library %>% left_join(ngrams) %>% drop_na()

# Visualise --------------------------------------------------------------------

# Wingardium draw a plot
spells %>% 
  ggplot(aes(x = pos, y = fct_rev(book), color = Type, label = Incantation)) +
  geom_quasirandom(
    groupOnX = FALSE, size = 2.25, alpha = .65, width = .25, shape = 8
  ) +
  scale_x_continuous(expand = c(0.05,.5)) +
  scale_color_paletteer_d("futurevisions::atomic_clock") +
  theme_minimal(base_size = 17) +
  labs(
    title = "Spells of the Harry Potter Books",
    x = NULL, y = NULL, color = NULL,
    caption = "List of Spells: pojo.com/harry-potter-spell-list\n#30daychartchallenge | @emilmalta"
  ) +
  theme(
    text = element_text(family = "Imprint MT Shadow"), 
    plot.background = element_rect(fill = "#F6F5F0"),
    plot.title.position = "plot",
    legend.position = "bottom", 
    legend.justification = "left",
    panel.background = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_text(hjust = 0), 
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank()
  ) 
