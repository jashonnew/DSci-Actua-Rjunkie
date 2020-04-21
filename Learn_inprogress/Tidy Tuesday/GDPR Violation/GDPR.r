pacman::p_load(tidyverse, lubridate)


gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')


gdpr_violations %>% 
  filter(mdy(date) > 1980, price < 1000000) %>% 
  ggplot() +
  geom_point(aes(x = mdy(date), y = price)) +
  geom_smooth(aes(x = mdy(date), y = price),se = FALSE) +
  labs(y = "Price in Euros", x = "Date", title = "General Data Protection Regulation")
