pacman::p_load(tidyverse, ggrepel, Rcolorbrewer)


tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv') %>% 
  mutate(name = str_replace(name, "Brigham Young University-Provo", "Brigham Young University"))

historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')


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
                         legend.text = element_text(color = "white"))


dat <- left_join(tuition_cost, salary_potential, by = "name") %>% 
  filter(degree_length == "4 Year", !is.na(mid_career_pay))

dat1 <- dat %>% 
  filter(mid_career_pay > 139000)
dat2 <- dat %>% 
  filter(str_detect(name,"Brigham"))

dat %>% 
  ggplot() +
  geom_point(aes(x = in_state_total, y = mid_career_pay, color = type)) +
  geom_point(data = dat1, aes(x = in_state_total, y = mid_career_pay, color = type), size = 3) +
  geom_point(data = dat2, aes(x = in_state_total, y = mid_career_pay, color = type), size = 3) +
  geom_smooth(aes(x = in_state_total, y = mid_career_pay), se = FALSE) +
  geom_label_repel(data = dat1, aes(y = mid_career_pay, x = in_state_total, label = name), size = 3, segment.colour = "white") +
  geom_label_repel(data = dat2, aes(y = mid_career_pay, x = in_state_total, label = name), size = 3, segment.colour = "white") +
  the_dark_knight +
  labs(x = "In-state Tuition (USD)", y = "Average Pay Mid Career (USD Yearly)", title = "Public - Choose Wisely; Private - You Get What You Pay For (Mostly)") +
  scale_color_brewer(palette = "Paired")

