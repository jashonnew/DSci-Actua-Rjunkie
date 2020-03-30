pacman::p_load(tidyverse, lubridate, ggrepel)


dat <- read_csv("../../.Rdata/office.csv")

the_office <- theme(panel.background = element_rect(fill = "#391239"),    # This is my theme I will use 
                         plot.background = element_rect(fill = "#391239"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                         axis.text = element_text(color = "#DAF7A6", size = 12),
                         axis.title = element_text(color = "#DAF7A6", vjust = 2, size = 16),
                         title = element_text(color = "#DAF7A6"),
                         legend.background = element_rect(fill = "#391239"),
                         legend.text = element_text(color = "#DAF7A6"),
                         legend.key = element_rect(fill = "#391239"))

dat <- dat %>% mutate(air_date = ymd(air_date)) %>% 
  select(-date)
ep1_dt <- dat %>% filter(episode == 1)
ep1_dt <- dat %>% filter(episode == 1)
ep_imp <- dat %>% filter(imdb_rating > 9.3 | imdb_rating < 7 | title %in% c("Dinner Party","Scott's Tots", "Booze Cruise", "Threat Level Midnight", "The Alliance", "Safety Training"))


dat %>% 
  ggplot(aes(x = air_date, y = imdb_rating)) +
  geom_point(col = "#DAF7A6") +
  geom_point(data = ep_imp, aes(x = air_date, y = imdb_rating), size = 3, col = "#00ffff") +
  geom_vline(xintercept = ep1_dt$air_date[-1], alpha = .6, lty = 2, col = "#DAF7A6") +
  geom_smooth(se = FALSE, col = "#DAF7A6") +
  geom_text_repel(data = ep_imp, aes(x = air_date, y = imdb_rating, label = title), col = "#DAF7A6") +
  coord_cartesian(xlim = c(ymd("2005-04-24", "2013-04-16"))) +
  the_office +
  labs(y = "IMDB", title = "Seasons 3 and 4 were the best. The people agree.", x = " ") +
  #scale_x_date(breaks = ep1_dt$air_date)
  scale_x_date(breaks = c(ymd("2005-03-24") + 30, ymd(ep1_dt$air_date[-1] + 180)),labels = c("Season 1","Season 2","Season 3","Season 4","Season 5","Season 6","Season 7","Season 8","Season 9"))



