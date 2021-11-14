options(tidyverse.quiet = TRUE)
library(readxl)
library(tidyverse)
library(broom)
library(investr)
library(quantreg)
library(splines)
source('C:/Users/javiera/OneDrive - Cawthron/Stats/R code/theme_javier.R')
theme_set(theme_javier())

mono <- read_csv('data/mono_clean.csv')

mono_nested <-
  mono %>%
  filter(sal > 24) %>% 
  group_by(species) %>%
  nest()


# Find temperature optima for hatching success-----
h_success_rq <-
  mono_nested %>%
  mutate(
    rq_models = map(.x = data, ~ rq(
      hatch_success ~ poly(temp, df = 2),
      data = .x,
      tau = .95
    )),
    rq_predict = map(
      rq_models,
      ~ predict(.x, newdata = tibble(temp = seq(8, 36, 0.1))) %>% as_tibble %>% bind_cols(tibble(temp = seq(8, 36, 0.1)))
    ),
    peak =  map(rq_predict, ~ .$temp[which.max(.$value)])
  )

peaks <- 
  h_success_rq %>% 
  select(peak) %>% 
  unnest(cols = c(peak))

# Explore hatching success-----------
dataLine <- 
  mono %>%
  group_by(species) %>%
  filter(hatch_success>0) %>% 
  summarise(max_x = max(temp, na.rm = T),
            min_x = min(temp, na.rm = T))


#Hatch success plot----------
hatch_success_plot <-
  mono %>%
  filter(sal > 24 ) %>%
  ggplot() +
  geom_point(alpha = 0.5, aes(color = species, x = temp, y = hatch_success)) +
  stat_quantile(aes(color = species, x = temp, y = hatch_success),
                method = rq,
                formula = y ~ poly(x, df = 2),
                quantiles = 1,
                size = .7
  ) +
  labs(y = 'Hatch success (%)', x = "Temperature (°C)") +
  theme_javier() +
  theme(
    legend.text = element_text(face = 'italic', size = 8),
    legend.position = c(.15, .8),
    legend.background = element_blank(),
    legend.title = element_blank()
  ) +  
  geom_linerange(data = dataLine,
                 aes(
                   xmin = min_x ,
                   xmax = max_x,
                   color = species,
                   y = 0
                 ),
                 size = 3,
                 position = position_dodge(10),
                 alpha = .5
  ) +
  geom_vline(data = peaks, aes(xintercept = peak, color = species), lty = 2) +
  geom_vline(data = peaks, aes(xintercept = peak, color = species), lty = 2)+
  geom_vline(data = peaks, aes(xintercept = peak, color = species), lty = 2) +
  geom_hline(data = peaks, aes(yintercept = 100), lty = 2) 

ggsave(
  hatch_success_plot,
  filename = 'figures/hatch_success_plot.png',
  height = 4.5,
  width = 6,
  dpi = 300
)

hatch_success_plot
mono %>%
  filter(sal > 24 ) %>% 
  group_by(species) %>% 
  summarise(mean(hatch_success, na.rm = T))

