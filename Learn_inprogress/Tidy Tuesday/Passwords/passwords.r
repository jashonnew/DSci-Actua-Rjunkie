pacman::p_load(tidyverse)


passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')
passwords <- passwords %>% 
  mutate(guess_time = case_when(
    time_unit == "years" ~ value*365,
    time_unit == "months" ~ value*30,
    time_unit == "weeks" ~ value *7,
    time_unit == "days" ~ value,
    time_unit == "hours" ~ value/24,
    time_unit == "minutes" ~ value/1440,
    time_unit == "seconds" ~ value/86400,
  )) %>% 
  filter(!is.na(guess_time))
passwords %>% 
  ggplot() + 
  geom_text(aes(y = rank, x = log(guess_time), label = password), size = 3)

passwords %>% 
  ggplot() + 
  geom_histogram(aes(x = log(guess_time))) +
  scale_x
