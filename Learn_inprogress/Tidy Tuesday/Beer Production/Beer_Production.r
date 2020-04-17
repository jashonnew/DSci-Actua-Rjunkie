pacman::p_load(tidyverse, forcats, sf, USAboundaries, scales)

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')


brewer_size %>% 
  ggplot() +
  geom_point(aes(x = taxable_removals, y = total_barrels)) +
  facet_wrap(~brewer_size)

brewing_materials %>% 
  filter(year < 2015) %>% 
  group_by(material_type, year) %>% 
  summarise(count = sum(month_current)) %>% 
  #mutate(month = as.character(month)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = count/100000000, color = material_type), lwd = 3) 

 

brewing_materials %>%
  filter(year < 2015) %>% 
  ggplot() +
  geom_line(aes(x = month, y = month_current/10000000, color = type), lwd = 1.1) +
  facet_wrap(~year)



states <- us_states() %>% 
  filter(stusps != "HI" & stusps != "AK"& stusps != "PR") %>% 
  rename(state = stusps) %>%
  select(state,geometry)
  

usdat <- left_join(states,beer_states, key = state) 


usdat %>% 
  ggplot() +
  geom_sf(aes(fill = log(barrels))) +
  facet_wrap(~usdat$year, nrow = 3) +
  scale_fill_gradient2(high = muted("blue"), low = "white") +
                       #high = muted("red"), midpoint = 1000000, space = "Lab",
                       #na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Which States Make our Beer?", fill = "Barrels Produced")
