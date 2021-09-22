options(tidyverse.quiet = TRUE)
library(readxl)
library(tidyverse)
library(broom)
library(investr)
library(quantreg)
library(splines)
source('C:/Users/javiera/OneDrive - Cawthron/Stats/R code/theme_javier.R')
theme_set(theme_bw())

# read and clean data ----
mono <-
  read_excel('data/All data sent 22 Sept 21.xlsx') %>%
  mutate_at(
    vars(time_1st_hatch:time_maturity),
    ~ str_replace_all(., "[^0-9.-]", "") %>% as.numeric(.)
  ) %>%
  mutate(temp = as.numeric(if_else(temp == "27-30", "28.5", temp)),
         sal = as.numeric(if_else(sal == "seawater", "35", sal))) %>% 
  mutate(hatch_success = 
           str_replace_all(`Hatch success rate (%)`, pattern = "No hatch", "0"),
         hatch_success = na_if(hatch_success , "ND")) %>% 
  mutate(hatch_success = str_remove_all(
    string = hatch_success,
    pattern =  "[^[:alnum:]].*"
  ) %>%  
    as.numeric()) %>% 
  select(-`Hatch success rate (%)`) %>% 
  write_csv('data/mono_clean.csv')


# summarise data ----------
mono %>% 
  group_by(species) %>%
  summarise(across(temp:sal, list(
    min = min,
    max = max,
    mean = mean,
    n = ~n()),
    na.rm = T))

# nest data --------
mono_nested <-
  mono %>%
  group_by(species) %>%
  nest()

# new data for predictions -------
new_data <- tibble(temp = seq(8, 36, by = 1))


# Oncomiracidia data -----------------
onco <- 
  read_excel('data/Oncomiracidia longevity.xlsx') %>%
  mutate(longevity = str_replace(
    string = longevity,
    pattern =  "\\±.*",
    replacement =  ""
  ) %>%  as.numeric()) %>% 
  filter(sal>22) %>% 
  mutate(longevity = longevity/24) %>% 
  write_csv('data/onco_longevity_clean.csv')

# nest data ----
onco_nested <-
  onco %>%
  group_by(species) %>%
  nest()

# plot data ----
ggplot(onco, aes(temp, longevity, color = species)) +
  geom_point(alpha = .9) +
  stat_smooth(
    method = 'nls',
    formula = 'y~a*x^b',
    method.args = list(start = c(a = 40, b = -1)),
    se = FALSE
  ) +
  scale_color_discrete(name = NULL) +
  labs(y = "Oncomiracidia longevity (days)", x = "Temperature (?C)") +
  theme_javier() +
  theme(legend.position = c(.2,.2)) 


# onco longevity models ------
onco_lon_models <- 
  onco_nested %>%
  mutate(
    nls_models = map(
      .x = data,
      ~ nls(longevity   ~ a * temp ^ b ,
            start = list(a = 40, b = -1),
            control = list(maxiter = 500),
            trace = F,
            data = .x
      )),
    nls_summary = map(.x = nls_models, tidy))


# by species using bootstrapping for CI estimation-----
m_onco_zeu <- nls(longevity    ~ a * temp ^ b ,
                  start = list(a = 40, b = -.3),
                  control = list(maxiter = 500),
                  trace = F,
                  data = onco %>% filter(species =="Zeuxapta seriolae")
)

m_zeu_boot <- nlsBoot(m_onco_zeu, niter = 999)

zeu_onco_pred <-
  nlsBootPredict(m_zeu_boot, newdata = new_data, interval = 'confidence') %>% as_tibble() %>%
  rename(fit = Median, lwr = `2.5%`, upr = `97.5%`) %>%
  bind_cols(new_data) %>% 
  mutate(name = "onco_longe", species = "Zeuxapta seriolae")


# use bootstrapping to get neobenedenia confidence intervals
library(nlstools)
m_onco_bene <- nls(longevity    ~ a * temp ^ b ,
                   start = list(a = 40, b = -.3),
                   control = list(maxiter = 500),
                   trace = F,
                   data = onco %>% filter(species =="Neobenedenia girellae")
)

m_boot <- nlsBoot(m_onco_bene, niter = 999)

neo_onco_pred <-
  nlsBootPredict(m_boot, newdata = new_data, interval = 'confidence') %>% as_tibble() %>%
  rename(fit = Median, lwr = `2.5%`, upr = `97.5%`) %>%
  bind_cols(new_data) %>% 
  mutate(name = "onco_longe", species = "Neobenedenia girellae")


# first hatch model ----
first_hatch_models <- 
  mono_nested %>%
  mutate(
    nls_models = map(
      .x = data,
      ~ nls(time_1st_hatch   ~ a * temp ^ b ,
            start = list(a = 40, b = -1),
            control = list(maxiter = 500),
            trace = F,
            data = .x
      )),
    nls_summary = map(.x = nls_models, tidy),
    first_hatch = map(.x = nls_models, ~predict(.x, newdata = new_data)),
    first_hatch_se = map(.x = nls_models, ~as_tibble(predFit(.x, newdata = new_data, interval = "confidence", level= 0.99)) %>% bind_cols(new_data))
  )

# check model residuals--
plot(first_hatch_models$nls_models[[1]])
plot(first_hatch_models$nls_models[[2]])
plot(first_hatch_models$nls_models[[3]])



# last hatch models ---------
last_hatch_models <- 
  mono_nested %>%
  mutate(
    nls_models = map(
      .x = data,
      ~ nls(time_last_hatch   ~ a * temp ^ b ,
            start = list(a = 40, b = -1),
            control = list(maxiter = 500),
            trace = F,
            data = .x
      )),
    nls_summary = map(.x = nls_models, tidy),
    last_hatch = map(.x = nls_models, ~predict(.x, newdata = new_data)),
    last_hatch_se = map(.x = nls_models, ~as_tibble(predFit(.x, newdata = new_data, interval = "confidence", level= 0.95)) %>% bind_cols(new_data))
  )

# check model residuals---
plot(last_hatch_models$nls_models[[1]])
plot(last_hatch_models$nls_models[[2]])
plot(last_hatch_models$nls_models[[3]])

# time to sexual maturity ----
mat_models <- 
  mono_nested %>%
  mutate(
    nls_models = map(
      .x = data,
      ~ nls(time_maturity   ~ a * temp ^ b ,
            start = list(a = 40, b = -1),
            control = list(maxiter = 500),
            trace = F,
            data = .x
      )),
    nls_summary = map(.x = nls_models, tidy),
    time_maturity = map(.x = nls_models, ~predict(.x, newdata = new_data)),
    time_maturity_se = map(.x = nls_models, ~as_tibble(predFit(.x, newdata = new_data, interval = "confidence", level= 0.95)) %>% bind_cols(new_data))
  )

# check model residuals---
plot(mat_models$nls_models[[1]])
plot(mat_models$nls_models[[2]])
plot(mat_models$nls_models[[3]])


# NLS summary tables------
model_summary_tables <- 
  bind_rows(
 first_hatch =  first_hatch_models %>% 
    select(nls_summary) %>%
    unnest(cols = nls_summary),
  last_hatch = last_hatch_models %>%
    select(nls_summary) %>%
    unnest(cols = nls_summary),
  maturity = mat_models %>%
    select(nls_summary) %>%
    unnest(cols = nls_summary),
 onco_longevity = onco_lon_models %>%
   select(nls_summary) %>%
   unnest(cols = nls_summary),
  .id = "Model"
) %>% 
  mutate(across(estimate:statistic, ~round(.,2))) %>% 
  write_csv('tables/nls_summary_table.csv')


# Prepare predicted and observed data for plots---------

# Benedenia seriolae 11.5 °C  - 30 °C* 
# Neobenedenia girellae  17 °C - 35 °C 
# Zeuxapta seriolae 9 °C  - 29 °C 
pred_all <-
  bind_rows(
    time_maturity = mat_models %>%
      select(time_maturity_se) %>%
      unnest(cols = time_maturity_se) ,
    time_last_hatch =
      last_hatch_models %>%
      select(last_hatch_se) %>%
      unnest(cols = last_hatch_se),
    time_1st_hatch =
      first_hatch_models %>%
      select(first_hatch_se) %>%
      unnest(cols = first_hatch_se),
    .id = "name"
  ) %>%
  bind_rows(zeu_onco_pred, neo_onco_pred) %>% 
  filter(
    if_else(species == "Benedenia seriolae", temp <= 30, temp <= 35),
    if_else(species == "Zeuxapta seriolae", temp <= 29, temp <= 35),
    if_else(species == "Benedenia seriolae", temp >= 11.5, temp >= 9),
    if_else(species == "Neobenedenia girellae", temp >= 17, temp >=9)
  ) %>%
  ungroup() %>% 
  add_row(species = "Benedenia seriolae", fit = predict(m_onco_zeu, newdata = tibble(temp = 12:30)), temp = 12:30, name = "onco_longe") %>% 
  # select(-c(lwr, upr)) %>% 
  write_csv('data/all_predicted_data.csv')
