pacman::p_load(tidyverse, rcolorbrewer, stringi, png, grid)

critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

get_png <- function(filename) {
grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("ray.png")

ray <- readPNG(system.file("ray","ray.png", package="png"))
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


villagers %>% 
  ggplot() +
  geom_point(aes(x = personality, y = species), alpha = .2)

dat <- villagers %>% 
  mutate(species = stri_trans_totitle(species), personality = stri_trans_totitle(personality)) %>% 
  group_by(personality, species) %>% 
  summarise(tot = n()) %>% 
  filter(str_detect(species, paste(dato$species, collapse = "|"))) 

dato <- villagers %>% 
  mutate(species = stri_trans_totitle(species), personality = stri_trans_totitle(personality)) %>% 
  group_by(species) %>% 
  summarise(tot = n()) %>% 
  filter(tot > 14)

dat %>% 
  ggplot() +
  geom_col(aes(y = tot, x = fct_reorder(species, tot), fill = fct_reorder(personality, -tot)), position = position_dodge(), width =.8) +
  geom_text(aes(x = 7.3, y = 7, label = "Raymond"), color = "#CDC9CD") +
  #geom_point(aes(x = 6, y = 7)) +
  geom_segment(aes(xend = 10.3, yend = 1.1, x = 10.7, y = 6), color = "#CDC9CD", arrow = arrow(length = unit(.5,"cm")), size = 2, linejoin = "round", lineend = "round") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  the_dark_knight +
  annotation_custom(l, ymin = 6.1, ymax = 7.9,xmin = 7,xmax = 12) +
  labs(x = "Species", y = "Count", title = "Animal Crossing - Everbody Loves Raymond", subtitle = "Clearly the Distribution of Personalities is not Consistent. \nNintendo Intends for us to Fight over Certain Villagers.", fill = "Personality")
  
