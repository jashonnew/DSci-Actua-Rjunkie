pacman::p_load(tidyverse, ggrepel)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv') %>% 
  select(volcano_name, elevation)
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv') %>% 
  filter(!is.na(vei)) %>% 
  left_join(volcano)
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')


the_dark_knight <- theme(panel.background = element_rect(fill = "#404040"),    # This is my theme I will use 
                         plot.background = element_rect(fill = "#404040"),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.y = element_line(color = "#7d7c7b"),
                         panel.grid.minor.y = element_line(color = "#7d7c7b"),
                         axis.text = element_text(color = "white"),
                         axis.title = element_text(color = "white"),
                         title = element_text(color = "white"),
                         legend.background = element_rect(fill = "#404040"),
                         legend.key = element_rect(fill = "#404040"),
                         legend.text = element_text(color = "white"))

eruptions %>% 
  ggplot() +
  geom_point(aes(x = start_year, y = elevation, size = vei ), alpha = .4, color = "#ff3c22") +
  geom_point(data = dat1, aes(x = start_year, y = elevation, size = vei ), color = "#22ff8d") +
  geom_label(data = dat, aes(x = 1900, y = elevation, label = volcano_name), size = 3) +
  geom_label_repel(data = dat1, aes(x = start_year, y = elevation, label = volcano_name), nudge_y = 400, size = 3) +
  labs(x = "Start Year of Eruption", y = "Elevation Above Sea Level \n(in meters)", title = "Were Tiny eruptions nearly non-existant pre 1825? Or Has Our Data Collection Improved?", subtitle = "Tambora is the Only 7 VEI Since 1800 | Other Labeled Volcanos Have Erupted More Than 70 Times Since 1800 ", size = "Volcanic Explosivity Index") +
  coord_cartesian(xlim = c(1800,2020)) +
  the_dark_knight


dat1 <- eruptions %>% 
  filter(vei > 6, start_year > 1800)

dat <- eruptions %>% 
  filter(start_year > 1800) %>% 
  group_by(volcano_name, elevation) %>% 
  summarise(n = n()) %>% 
  filter(n > 70)
