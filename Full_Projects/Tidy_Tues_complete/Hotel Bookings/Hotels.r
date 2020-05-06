pacman::p_load(tidyverse)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv') %>% 
  mutate(nights = stays_in_weekend_nights + stays_in_week_nights)

the_dark_knight <- theme(panel.background = element_rect(fill = "#404040"),    # This is my theme I will use 
                         plot.background = element_rect(fill = "#404040"),
                         panel.grid.major.y = element_blank(),
                         panel.grid.minor.y = element_blank(),
                         panel.grid.major.x = element_line(color = "#7d7c7b"),
                         panel.grid.minor.x = element_line(color = "#7d7c7b"),
                         axis.text = element_text(color = "white"),
                         axis.title = element_text(color = "white"),
                         title = element_text(color = "white"),
                         strip.background =element_rect(fill="#404040"),
                         strip.text = element_text(colour = 'white'),
                         legend.background = element_rect(fill = "#404040"),
                         legend.key = element_rect(fill = "#404040"),
                         legend.text = element_text(color = "white"))

hotels %>% 
 ggplot(aes(x = factor(arrival_date_month, levels = month.name), y = nights)) +
  geom_jitter(alpha = .3, width = .2, color = "purple1") +
  geom_boxplot(outlier.color = "white", fill = "goldenrod", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, color = "firebrick", size = 1) +
  #geom_smooth(se = FALSE) +
  the_dark_knight +
  coord_cartesian(ylim = c(0,20)) +
  labs(x = "", y = "Nights Stayed", title = "Clear Trend in Resort Stay Lengths in Summer Months, City Hotel Shows General Consistency.", subtitle = "Red Lines Represent Mean | Hotel Booking Demands Data by Antonio, Almeida, Nunes.") +
  facet_wrap(~hotel, ncol = 1)
 