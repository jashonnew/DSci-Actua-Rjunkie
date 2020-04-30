pacman::p_load(tidyverse, lubridate, knitr)

#[ ] Read in the data from https://byuistats.github.io/M335/data/sales.csv and format it for visualization and analysis

sales <- read_csv("https://byuistats.github.io/M335/data/sales.csv")

#[ ] The data are for businesses in the mountain time zone make sure you read in times correctly

dat <- sales %>% mutate(MDT = force_tzs(Time, "UTC", "US/Mountain")) %>% filter(Name != "Missing")

#[ ] This is point of sale (pos) data, so you will need to use library(lubridate) to create the correct time aggregations

datt <- dat %>%
  mutate(Day = (ceiling_date(MDT, unit = "days"))) %>% 
  mutate(Week = (ceiling_date(MDT, unit = "week"))) %>% 
  mutate(Month = (ceiling_date(MDT, unit = "month"))) %>% 
  mutate(Hour = (ceiling_date(MDT, unit = "hour")))
  


datd <- datt %>% 
  group_by(Day, Type, Name) %>% 
  summarise(Tsales = sum(Amount))


datw <- datt %>% group_by(Week, Type, Name) %>% 
  summarise(Tsales = sum(Amount))

dath <- datt %>% group_by(Hour, Type, Name) %>% 
  summarise(Tsales = sum(Amount))

datm <- datt %>% group_by(Month, Type, Name) %>% 
  summarise(Tsales = sum(Amount))

#[ ] Check the data for any inaccuracies

#Going to vizualise with and without negatives, Missing Values were removed. 

#[ ] Help your boss understand which business is the best investment through visualizations
datt %>% 
  group_by(Name) %>% 
  summarise(Sales = n()) %>% 
  mutate(Sales = as.factor(Sales)) %>% 
  kable(caption = "Number of Sales for each Company")


datd %>% 
  filter(Day != "2016-04-21") %>% 
  ggplot(aes(x = Day ,y = Tsales, color = Name)) +
  theme_bw() +
  geom_point() +
  coord_cartesian(ylim = c(0, 900)) +
  geom_line(aes(group = Name), size = 1) +
  facet_wrap(~Name) +
  theme(legend.position = "none" ) +
  labs(x = "", y = " Total Sales by Day", title = "Though Lebelle peaks highest, Its low N pushes investors towards Hotdiggity")

datw %>% 
  filter(Week != "2016-04-24") %>% 
  ggplot(aes(x = Week ,y = Tsales, color = Name)) +
  theme_bw() +
  geom_point() +
  geom_line(aes(group = Name), size = 1) +
  facet_wrap(~Name) +
  theme(legend.position = "none" ) +
  labs(x = "", y = " Total Sales by Week ", title = "Though Lebelle peaks highest, Its low N pushes investors towards Hotdiggity")

datm %>% 
  filter(Month != "2016-04-24") %>% 
  ggplot(aes(x = Month ,y = Tsales, color = Name)) +
  theme_bw() +
  geom_point(size = 3) +
  geom_line(aes(group = Name), size = 2) +
  facet_wrap(~Name) +
  theme(legend.position = "none" ) +
  labs(x = "", y = " Total Sales by Month ", title = "Though Lebelle peaks highest, Its low N pushes investors towards Hotdiggity")




#[ ] Provide an understanding and recommendation for hours of operation

datt %>% 
  mutate(hour = hour(Time)) %>% 
  ggplot(aes(x = hour),fill = "purple4") +
  geom_histogram() +
  theme_bw() +
  coord_cartesian(xlim = c(8, 24)) +
  facet_wrap(~Name, scales = "free_y") +
  labs(title = " Hours of Operation", x = "hours", fill = "")
  

#[ ] We don’t have employee numbers, but sales traffic can help. Provide some visualizations on customer traffic

datt %>% 
  group_by(Name) %>% 
  summarise(Sales = n()) %>% 
  mutate(Sales = as.factor(Sales)) %>% 
  kable(caption = "Number of Sales for each Company")

#[ ] Provide a final comparison of the six companies and a final recommendation

datt %>% 
  ggplot(aes(x = Name, y = Amount )) +
  geom_jitter(width = .2, color = "goldenrod") +
  geom_boxplot(outlier.shape = NA, fill = "purple3", alpha = .75) +
  theme_bw() +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, color = "forestgreen", size = 1.3) +
  labs(x = "Comapny", y =" Sales Amount",title = "Lebelle Clear Lead", subtitle = "Sales Distributions Between Companies") +
  coord_cartesian(ylim = c(-25, 50)) 
  

#[ ] Compile your .md and .html file into your git repository
#[ ] Find two other student’s compiled files in their repository and provide feedback using the issues feature in GitHub (If they already have three issues find a different student to critique)
#[ ] Address 1-2 of the issues posted on your project and push the updates to GitHub