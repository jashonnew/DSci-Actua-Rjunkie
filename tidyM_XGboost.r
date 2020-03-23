pacman::p_load(tidymodels, rattle, here, ranger)
here <- here()

dat <- read.csv(here(".Rdata/wine.csv"), header = FALSE) %>% 
  mutate(V1 = as.factor(V1))

set.seed(100)

wine_split <- dat %>% initial_split(prop = .7)
wine_split %>% training() %>% glimpse()

wine_recipe <- training(wine_split) %>%
  recipe(V1 ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
wine_recipe

wine_test <- wine_recipe %>% 
  bake(testing(wine_split))
glimpse(wine_test)

wine_train <- juice(wine_recipe)

wine_tree <- decision_tree(mode = "classification") %>%
  set_engine("rpart") %>% 
  fit(V1 ~ ., data = wine_train)

wine_tree %>% 
  predict(wine_test) %>% 
  bind_cols(wine_test) %>% 
  metrics(truth = V1, estimate = .pred_class)

rattle::fancyRpartPlot(wine_tree$fit)
wine_tree$fit

wine_forest <- rand_forest(mode = "classification") %>%
  set_engine("ranger") %>% 
  fit(V1 ~ ., data = wine_train)

wine_forest %>% 
  predict(wine_test) %>% 
  bind_cols(wine_test) %>% 
  metrics(truth = V1, estimate = .pred_class)

wine_boost <- boost_tree(mode = "classification", trees = 20, tree_depth = 3, learn_rate = .01) %>% 
  set_engine("xgboost") %>% 
  fit(V1 ~ ., data = wine_train)

wine_boost %>% 
  predict(wine_test) %>% 
  bind_cols(wine_test) %>% 
  metrics(truth = V1, estimate = .pred_class)
