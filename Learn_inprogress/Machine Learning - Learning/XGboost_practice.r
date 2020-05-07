pacman::p_load(mlr,mlrMBO,tidyverse, tidymodels)

here::here()
setwd(here::here())

dat <- read_csv(".RData/audit_data/audit_risk.csv") %>% 
  mutate(risk = as.character(Risk),
         risk = as.factor(risk),
         location_id = case_when(
           LOCATION_ID == "SAFIDON" ~ "-1",
           LOCATION_ID == "NUH" ~ "-2",
           LOCATION_ID == "LOHARU" ~ "-3",
           TRUE ~ LOCATION_ID)) #%>% 
dat <- dat %>% select(-Risk, -LOCATION_ID, -Detection_Risk) %>% 
  mutate(location_id = parse_number(location_id))


dat_split <- dat %>% initial_split(prop = .75)
dat_split %>% training() %>% glimpse

dat_recipe <- training(dat_split) %>% 
  recipe(risk ~ .) %>% 
  step_corr(all_numeric(), -all_outcomes()) %>% 
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_scale(all_numeric(), -all_outcomes()) %>%
  prep()
dat_recipe

dat_training <- juice(dat_recipe)

dat_testing <- dat_recipe %>% 
  bake(testing(dat_split))

dat_boost <- boost_tree(mode = "classification") %>% 
  fit(risk ~ ., data = dat_training)

dat_boost %>% 
  predict(dat_testing) %>% 
  bind_cols(dat_testing) %>% 
  metrics(truth = risk, estimate = .pred_class)

conf_mat_dat <- dat_boost %>% 
  predict(dat_testing) %>% 
  bind_cols(dat_testing) %>%
  conf_mat(truth = risk, estimate = .pred_class)

autoplot(conf_mat_dat, type = "heatmap")
