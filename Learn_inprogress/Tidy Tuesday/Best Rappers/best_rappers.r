pacman::p_load(tidyverse, ggrepel)


polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

dat <- rankings %>% 
  filter(n > 6)

dat1 <- rankings %>% 
  filter(n >6 )

Rap <- theme(panel.background = element_rect(fill = "#5a44b0"),    # This is my theme I will use 
                    plot.background = element_rect(fill = "#5a44b0"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    axis.text = element_text(color = "#ffc164", size = 12),
                    axis.title = element_text(color = "#ffc164", vjust = 2, size = 16),
                    title = element_text(color = "#ffc164"),
                    legend.background = element_rect(fill = "#5a44b0"),
                    legend.text = element_text(color = "#ffc164"),
                    legend.key = element_rect(fill = "#5a44b0"))

rankings %>%  
  ggplot() + 
  geom_point(aes(x = n, y = points), alpha = .6, color = "#ffc164") +
  geom_text_repel(data = dat, aes(x = n, y = points, label = title),nudge_y = 10, color = "#ffc164", nudge_x = 1.5) +
  geom_vline(aes(xintercept = 6.5), lty = 2) + 
  geom_text(aes(x = 11, y = 150, label = "Songs here are all pre-1995")) +
  geom_text(aes(x = 3, y = 150, label = "Not Time-bound")) +
  #geom_text_repel(data = dat, aes(x = n, y = points, label = artist)) +
  labs(y = "Total Score", x = "Total Votes", title = "Best Hip-Hop Songs", subtitle = "According to 100 BBC Critics and Artists") + 
  theme_bw() + 
  Rap

#5a44b0
#ffc164