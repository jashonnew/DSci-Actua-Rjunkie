pacman::p_load(tidyverse, ggrepel)

TDF <- theme(panel.background = element_rect(fill = "#3b2abf"),    # This is my theme I will use 
             plot.background = element_rect(fill = "#3b2abf"),
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             axis.text = element_text(color = "white", size = 12),
             axis.title = element_text(color = "white", vjust = 2, size = 16),
             title = element_text(color = "white"),
             legend.background = element_rect(fill = "#3b2abf"),
             legend.text = element_text(color = "#white"),
             legend.key = element_rect(fill = "#3b2abf"))



tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv') %>% 
  group_by(winner_name) %>% 
  summarize("1" = cumsum(distance)[1], "2" = cumsum(distance)[2], "3" = cumsum(distance)[3], "4" = cumsum(distance)[4], "5" = cumsum(distance)[5], "6" = cumsum(distance)[6], "7" = cumsum(distance)[7]) %>% 
  pivot_longer(-winner_name, names_to = "card_race_won", values_to = "Distance" ) %>% 
  filter(!is.na(Distance)) %>% 
  mutate(card_race_won = parse_number(card_race_won))

dat <- tdf_winners %>% 
  filter(card_race_won > 4) %>% 
  group_by(winner_name) %>% 
  mutate(Distance = max(Distance))




tdf_winners %>% 
  ggplot(aes(x = card_race_won, y = Distance)) +
  geom_point( alpha = .6, color = "white") +
  geom_line(aes(group = winner_name), color = "#bf312a") +
  geom_text_repel(data = dat[c(-4,-5),], aes(label = winner_name), color =  "#bf312a", nudge_y = -4000, nudge_x = 1) +
  labs(x = "Race Won (By Cyclist)", y = "Cumuative Distance of Races Won", title = "Lance Maintains the Unequivocal Lead") + 
  TDF +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7),labels = c("1st","2nd","3rd","4th","5th","6th","7th"))

  
  