library(tidymodels, rattle)
View(iris)

set.seed(100)

iris_split <- iris %>% initial_split(prop = .7)
iris_split %>% training() %>% glimpse()

iris_recipe <- training(iris_split) %>%
  recipe(Species ~.) %>%
  step_corr(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
iris_recipe

iris_testing <- iris_recipe %>% 
  bake(testing(iris_split))

glimpse(iris_testing)

iris_training <- juice(iris_recipe)

glimpse(iris_training)

iris_tree <- decision_tree(tree_depth = 4, mode = "classification") %>%
  set_engine("rpart") %>% 
  fit(Species ~ ., data = iris_training)

iris_tree %>% 
  predict(iris_testing) %>% 
  bind_cols(iris_testing) %>% 
  metrics(truth = Species, estimate = .pred_class)

rattle::fancyRpartPlot(iris_tree$fit)
iris_tree$fit
