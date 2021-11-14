library(tidyverse)
source('C:/Users/javiera/OneDrive - Cawthron/Stats/R code/theme_javier.R')
theme_set(theme_bw())


# Treatment windows calculations --------
treatment_data <- 
  pred_all %>% 
  # mutate(
  #   time_last_hatch   = if_else(
  #     species == "Neobenedenia girellae", mean_neo$mean,
  #     time_last_hatch
  #   )
  # ) 
  # add_row(species = "Benedenia seriolae", fit = predict(m_onco_zeu, newdata = tibble(temp = 12:30)), temp = 12:30, name = "onco_longe") %>% 
  select(-c(lwr, upr)) %>% 
  arrange(name) %>% 
  pivot_wider(names_from = name, values_from = fit) %>%
  mutate(lower_treat = time_last_hatch + (onco_longe *.5),
         upper_treat = time_maturity - 1)


treatment_data %>% 
  group_by(temp) %>% 
  summarise(Taxa = first(paste0(str_sub(species,start = 1, end = 2), collapse = ""))) %>% 
  clipr::write_clip()

# Add multiple species combinations treatment windows -----
multiple_spp_pred <-
  treatment_data %>%
  group_by(temp) %>%
  summarise(lower_treat = max(lower_treat, na.rm = T),
            upper_treat = min(upper_treat, na.rm = T)) %>%
  mutate(species = "BeNeZe")


beze_spp_pred <-
  treatment_data %>%
  filter(species != "Neobenedenia girellae") %>% 
  group_by(temp) %>%
  summarise(lower_treat = max(lower_treat, na.rm = T),
            upper_treat = min(upper_treat, na.rm = T)) %>%
  mutate(species = "BeZe")

neze_spp_pred <-
  treatment_data %>%
  filter(species != "Benedenia seriolae") %>% 
  group_by(temp) %>%
  summarise(lower_treat = max(lower_treat, na.rm = T),
            upper_treat = min(upper_treat, na.rm = T)) %>%
  mutate(species = "NeZe")

bene_spp_pred <-
  treatment_data %>%
  filter(species != "Zeuxapta seriolae") %>% 
  group_by(temp) %>%
  summarise(lower_treat = max(lower_treat, na.rm = T),
            upper_treat = min(upper_treat, na.rm = T)) %>%
  mutate(species = "BeNe")


# Collate all treatment windows data and filter -------- 
treatment_data_all <- 
  bind_rows(treatment_data, multiple_spp_pred, beze_spp_pred, neze_spp_pred, bene_spp_pred) %>%
  arrange(temp) %>%
  filter(if_else(species == "BeNeZe", temp >22 & temp<30, temp>0),
         if_else(species == "BeZe", temp >11 & temp < 30, temp>0),
         if_else(species == "NeZe", temp >22 & temp < 30, temp>0),
         if_else(species == "BeNe", temp >17 & temp <31, temp>0),) %>% 
  # mutate(upper_treat = if_else(upper_treat>50, 50, upper_treat)) %>% # cap treatment to 50 days
  filter(temp>9 & temp <31) %>%
  mutate(
    species = fct_relevel(
      species,
      "Benedenia seriolae"   ,
      "Neobenedenia girellae",
      "Zeuxapta seriolae" 
    )
  ) %>%
  filter(temp>9 & temp <31) %>% 
  mutate(onco_infectivity = onco_longe/2) %>% 
  relocate(onco_infectivity, .after = onco_longe) %>% 
  write_csv('clean_data/predicted_data_and_treatment_windows.csv', na = '')


treatment_data_all

# Treatment windows plot ---------
treat_plot <- 
  treatment_data_all %>%
  filter(temp %% 2 == 0) %>%
  ggplot(aes(x = temp, color = species)) +
  geom_linerange(
    aes(
      ymin = lower_treat,
      ymax = upper_treat
    ), size = 2, 
    position = position_dodge(1.5), alpha = .7
  ) +
  coord_flip() +
  theme_javier() +
  scale_color_discrete(name = NULL) +
  theme(legend.text =  element_text(face = 'italic'), legend.position = c(.65,.75)) +
  labs(x = "Temperature (°C)", y = 'Days after initial treatment') +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  scale_x_continuous(breaks = seq(10, 30, 2)) 

treat_plot


ggsave(
  treat_plot,
  filename = 'figures/treatment_window_plots.png',
  height = 6,
  width = 4
)
