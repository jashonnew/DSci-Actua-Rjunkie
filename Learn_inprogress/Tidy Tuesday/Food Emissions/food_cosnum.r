pacman::p_load(tidyverse)

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>% 
  filter()

food_consumption %>% 
  ggplot() +
  geom_point(aes(x = consumption, y = co2_emmission, color = food_category))
