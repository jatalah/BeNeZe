library(tidyverse)
library(janitor)
source('C:/Users/javiera/OneDrive - Cawthron/Stats/R code/theme_javier.R')
theme_set(theme_javier())

mono <- read_csv('data/mono_clean.csv')
onco <- read_csv('data/onco_longevity_clean.csv')
pred_all <- read_csv('data/all_predicted_data.csv')

# Regression plots -------
obs_all <-
  mono %>%
  left_join(onco %>% rename(onco_longe = "longevity"),
            by = c("species", "temp", "sal")) %>%
  clean_names() %>% 
    pivot_longer(
    cols = c(time_1st_hatch:time_maturity, onco_longe, hatch_success),
    values_drop_na = T,
    names_to = 'name'
  ) %>%
  mutate(name = fct_relevel(
    name,
    c(
      "time_1st_hatch",
      "time_last_hatch",
      "onco_longe",
      "time_maturity"
    )
  ),
  source = if_else(is.na(source), source_2, source)) %>% 
  select(-source_2) %>% 
  write_csv('clean_data/BeNeZe_data_raw_all.csv')


# regression plot all  ---------
p_all_dat <- 
  pred_all %>% 
  left_join(obs_all %>% 
              select(species, temp, name, value), by = c("temp", "species", "name")) %>% 
  mutate(name = 
           fct_recode(
             name,
             `Time to first hatch` = "time_1st_hatch",
             `Time to last hatch` = "time_last_hatch",
             `Oncomiracidia longevity` = "onco_longe",
             `Time to sexual maturity` = "time_maturity"
           ),
         name = fct_relevel(name, "Oncomiracidia longevity", after = 2))

p_all <- 
  ggplot(p_all_dat, aes(x = temp, group = species)) +
  geom_point(aes(y = value, color = species), alpha = .5) +
  geom_line(aes(y =  fit, color = species)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr, x = temp),
    alpha = 0.3,
    fill = "gray"
  ) +
  scale_color_discrete(name = NULL) +
  labs(x = "Temperature (°C)", y = 'Days') +
  facet_wrap(~name, scales = 'free_y') +
  theme(
    legend.position =c(0.8,.33),
    legend.text = element_text(face = 'italic', size = 7),
    legend.background = element_blank()
  )

p_all

# save plots --------
ggsave(
  p_all,
  filename = 'figures/all_plots.png',
  height = 4.5,
  width = 4.5,
  dpi = 300
)

ggsave(
  p_all,
  filename = 'figures/all_plots.svg',
  height = 4,
  width = 4,
  device = 'svg'
)

# salinity plot------------
sal_plots <- 
  obs_all %>% 
  filter(name != 'onco_longe') %>% 
  mutate(name = 
           fct_recode(
             name,
             `Time to first hatch` = "time_1st_hatch",
             `Time to last hatch` = "time_last_hatch",
             `Oncomiracidia longevity` = "onco_longe",
             `Time to sexual maturity` = "time_maturity"
           )) %>% 
  ggplot(aes(sal, value, color = species)) +
  geom_point(alpha = .5, position = position_jitter(width = 0.7)) +
  stat_smooth(se = F, span = 0.95) +
  scale_color_discrete(name = NULL) +
  labs(x = "Salinity (ppt)", y = 'Days') +
  facet_wrap(~name, scales = 'free_y') +
  theme(
    legend.position =c(0.15,.33),
    legend.text = element_text(face = 'italic', size = 7),
    legend.background = element_blank()
  )

sal_plots

ggsave(
  sal_plots,
  filename = 'figures/salinity_plots.png',
  height = 5,
  width = 6
)
