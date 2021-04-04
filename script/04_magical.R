# Laod libraries ---------------------------------------------------------------

library(harrypotter)
library(tidyverse)
library(tidytext)

# Import data ------------------------------------------------------------------

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
  unnest(text) %>% 
  group_by(book) %>% 
  mutate(chapter = row_number()) %>%
  ungroup() %>% 
  unnest_tokens(word, text)
  
