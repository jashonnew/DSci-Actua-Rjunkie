pacman::p_load(tidyverse)

the_dark_knight <- theme(panel.background = element_rect(fill = "#404040"),    # This is my theme I will use 
                         plot.background = element_rect(fill = "#404040"),
                         panel.grid.major.y = element_blank(),
                         panel.grid.minor.y = element_blank(),
                         panel.grid.major.x = element_line(color = "#7d7c7b"),
                         panel.grid.minor.x = element_line(color = "#7d7c7b"),
                         axis.text = element_text(color = "white"),
                         axis.title = element_text(color = "white"),
                         title = element_text(color = "white"),
                         legend.background = element_rect(fill = "#404040"),
                         legend.key = element_rect(fill = "#404040"),
                         legend.text = element_text(color = "white"))


food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>% 
  filter() %>% 
  group_by(country) %>% 
  summarize(TotalConsumption = sum(consumption), TotalEmissions = sum(co2_emmission)) %>% 
  mutate(ranking = rank(-TotalConsumption)) %>% 
  filter(ranking <= 30) %>% 
  select(-ranking) %>% 
  pivot_longer(-country,names_to = "var_type")

pally = palette(c("#ffb950","#7350ff"))

food_consumption %>% 
  ggplot() +
  geom_col(aes(y = value, x = reorder(country, value), fill = var_type), position = "dodge") +
  coord_flip() +
  labs(y = "Kg Food Consumption/Emissions per person per year", x = "", title = "Consumption and Emissions Don't Correlate", subtitle = "Production Seems like the More Likely Cause") +
  the_dark_knight +
  scale_fill_manual(values = c("#ffc83d","#913dff"))
