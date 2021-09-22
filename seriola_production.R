library(tidyverse)
source('C:/Users/javiera/OneDrive - Cawthron/Stats/R code/theme_javier.R')
theme_set(theme_javier())

prod_data_all <- read_rds('C:/Users/javiera/OneDrive - Cawthron/Stats/data/FAO aqua/production_data_all.RDS')

seriola_data <-
  prod_data_all %>%
  filter(str_detect(Scientific_Name, 'Serio')) %>%
  arrange(YEAR) %>%
  add_row(
    QUANTITY = c(500, 571, 1098, 2559, 2287, 2640, 3324, 3166, 3295),
    country_name = "Australia",
    Scientific_Name = "Seriola lalandi",
    YEAR = 2013:2021
  ) %>% 
  mutate(country_name= fct_recode(country_name,
                                  `Republic of Korea` = "Korea, Republic of",
                                  `Taiwan` = "Taiwan Province of China"))

seriola_prod_plot <- 
ggplot(seriola_data) +
  geom_path(aes(YEAR, QUANTITY, color = country_name)) +
  facet_wrap( ~Scientific_Name, scales = 'free_y') +
  scale_color_discrete(name = NULL) +
  theme( strip.text = element_text(face = 'italic'), 
         legend.position = c(.83,.2),
         legend.text = element_text(size = 7)) +
  guides(color=guide_legend(ncol=2)) +

  scale_y_continuous(labels = scales::comma_format(accuracy = 1L)) +
  labs(x = "Year", y = "Production (tonnes)") 

ggsave(
  seriola_prod_plot,
  filename = 'figures/seriola_prod_plot.png',
  height = 5,
  width = 8,
  dpi = 300,
  device = 'png'
)

seriola_prod_plot
ggsave(
  seriola_prod_plot,
  filename = 'figures/seriola_prod_plot.svg',
  height = 5,
  width = 8,
  device = 'svg'
)
