# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(scales)
library(ragg)

# Wright fisher function -------------------------------------------------------

# Without mutation
wright_fisher_simple <- function(n = 250, p = .5, t = 100L) {
  vector(mode = "numeric", length = t - 1) %>% 
    accumulate(~ rbinom(1, 2 * n, .x)/(2 * n), .init = p)
}

# With mutation
wright_fisher_sim <- 
  function(n = 250, p = .5, t = 100L, mut_to = 0, mut_from = 0) {
    vector(mode = "numeric", length = t - 1) %>% accumulate(
      ~ rbinom(1, 2 * n, (1 - mut_from) * .x + mut_to * (1 - .x))/(2 * n), 
      .init = (1 - mut_from) * p + mut_to * (1 - p)
    )
  }

# Simulations ------------------------------------------------------------------

simple_sim <- crossing(sim = 1:1000, n = c(500, 250, 10)) %>% 
  group_by(sim, n) %>% 
  summarise(
    p = wright_fisher_simple(n = n), t = seq_along(p), .groups = "drop"
  )

mutation_sim <-
  crossing(sim = 1:100, p_init = c(.1, .9), mut_from = .05, mut_to = .05) %>% 
  group_by(sim, p_init) %>% 
  summarise(
    p = wright_fisher_sim(p = p_init, mut_to = mut_to, mut_from = mut_from),
    t = seq_along(p), .groups = "drop"
  )

# Make plots -------------------------------------------------------------------

p1 <- simple_sim %>% 
  ggplot(aes(x = t, y = p)) +
  geom_line(aes(group = sim), color = "#31004a", size = .1, alpha = .2) +
  facet_wrap(~ paste("Population size:", n), ncol = 1) +
  labs(
    title = "Genetic Drift",
    subtitle = "Wright Fisher Simulations of allele frequencies through time. The time to fixation, or\nloss of an allele, is dependent on population size, and initial allele frequency.\nDensities show distribution of allele frequencies at t = 100 for 1,000 simulations",
    y = "Allele frequency", x = NULL
  ) +
  theme_minimal(base_family = "MesmerizeScLt-Regular")

p2 <- simple_sim %>% 
  filter(t == max(t)) %>% 
  ggplot(aes(y = p)) +
  geom_density(fill = "#cbc3e3", color = NA) +
  facet_wrap(~n, ncol = 1) +
  labs(y = NULL) + 
  theme_void() +
  theme(strip.text = element_blank())

p3 <- mutation_sim %>% 
  ggplot(aes(x = t, y = p, group = sim)) +
  geom_line(aes(color = factor(p_init)), alpha = .2) + 
  scale_color_manual(values = c("0.1" = "#d01b1a", "0.9" = "#48abd8")) +
  facet_wrap(~ paste0("Initial allele frequency: ", p_init * 100, "%")) +
  labs(subtitle = "Things get interesting with the introduction of random mutation. The following shows\nsimulations, where individuals have 50% chance of losing, and 50% chance of gaining\nthe allele by mutation.") +
  theme_minimal(base_family = "MesmerizeScLt-Regular") +
  theme(legend.position = "none") +
  labs(x = "\nGeneration", y = "")

p4 <- mutation_sim %>% 
  filter(t == max(t)) %>% 
  ggplot(aes(y = p)) +
  geom_density(aes(fill = factor(p_init)), color = NA, alpha = .3) +
  scale_fill_manual(values = c("0.1" = "#d01b1a", "0.9" = "#48abd8")) +
  expand_limits(y = c(0,1)) +
  facet_wrap(~"") +
  theme_void(base_family = "MesmerizeRg-Regular") +
  theme(legend.position = "none") +
  labs(caption = "#30daychartchallenge\n@emilmalta")

# Patch it up ------------------------------------------------------------------

# What is this magic?!
design <- "
AAAAB
AAAAB
AAAAB
CCCCD
"

p1 + p2 + p3 + p4 +
  plot_layout(design = design) &
  expand_limits(y = c(0,1))  &
  scale_y_continuous(labels = scales::percent) &
  theme(
    plot.title = element_text(family = "MesmerizeRg-Regular", size = 32),
    plot.title.position = "plot", 
    plot.subtitle = element_text(
      family = "MesmerizeScLt-Regular", size = 14, 
      margin = margin(10,10,15,10))
  )
