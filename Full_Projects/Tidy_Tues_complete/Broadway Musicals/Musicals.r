pacman::p_load(tidyverse,ggrepel, lubridate, ggridges, ggalt, Rcolorbrewer)


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

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv') %>%
  mutate(year_month = ymd(floor_date(ymd(week_ending), unit = "month"))) %>% 
  left_join(mutate(cpi, year_month = ymd(year_month)), by = "year_month")

dat <- grosses %>% 
  group_by(show) %>% 
  summarize(total = sum((weekly_gross/cpi)*266.795)) %>% 
  mutate(ranking = rank(desc(total))) %>% 
  filter(ranking <= 30)

dat1 <- grosses %>% 
  group_by(show) %>% 
  summarise(ending = max(week_ending), Starting = min(week_ending),total = sum((weekly_gross/cpi)*266.795)) %>% 
  mutate(current = case_when(
    ending > ymd("2020-01-01") ~ "Yes",
    TRUE ~ "No"
  ))


synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')


filter(grosses, str_detect(show, paste(dat$show, collapse = "|"))) %>% 
  ggplot(aes(x = ymd(week_ending), y = (weekly_gross/cpi)*266.795)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Week of Show", y = "Amount Grossed Adjusted for Inlation")


filter(grosses, str_detect(show, paste(dat$show, collapse = "|"))) %>% 
  ggplot() +
  geom_density_ridges_gradient(stat = "identity", aes(y = show, x = week_ending, height = (weekly_gross/cpi)*266.795, fill = week_ending), scale = 2.5) 


filter(dat1, str_detect(show, paste(dat$show, collapse = "|"))) %>% 
  ggplot() +
  geom_dumbbell(aes(x = Starting, xend = ending, y = reorder(show,total), color = current), size_x = 3, size_xend = 3, dot_guide_size = 2) +
  labs(x = "Dates the Show was Open on Broadway", y = "Show Title", title = "Thirty Highest-Grossing Broadway Shows since 1985", subtitle = "Descending from Highest Grossing to Least - Adjusted For Inflation", color = "Currently Playing? \n(Until COVID-19)") +
  the_dark_knight +
  theme(axis.text.x = element_text(angle = 20)) + 
  guides(colour = guide_legend(override.aes = list(size=3))) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_color_brewer(palette = "Spectral")
  
  #scale_x_datetime(breaks = c(as.Date("1985-01-01"), as.Date("1990-01-01"), as.Date("1995-01-01"), as.Date("2000-01-01"), as.Date("2005-01-01"),as.Date("2010-01-01"),as.Date("2015-01-01"),as.Date("2020-01-01")))
