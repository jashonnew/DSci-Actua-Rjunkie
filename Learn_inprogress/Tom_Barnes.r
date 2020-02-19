pacman::p_load(tidyverse, lubridate, patchwork)

dat <- read_csv("../.Rdata/US Onions.csv")


the_dark_knight <- theme(panel.background = element_rect(fill = "#404040"),    # This is my theme I will use 
                         plot.background = element_rect(fill = "#404040"),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         panel.grid.major.y = element_line(color = "#7d7c7b"),
                         panel.grid.minor.y = element_line(color = "#7d7c7b"),
                         axis.text = element_text(color = "white"),
                         axis.title = element_text(color = "white"),
                         title = element_text(color = "white"))
onion_col <- c("green", "#F9ECC5", "#F560C3", "skyblue", "orange", "#F7F6F3", "#F7EF43")

dat <-  dat %>% 
  mutate(ppp = `GA $` /`GA EQ`, pppl = `GA $ Prior Yr` / `GA EQ Prior Yr`, date = ymd(Week)) %>% 
  filter(`BC SUB CATEGORY` != "VALUE ADD", `BC SUB CATEGORY` != "REMAINING ONIONS", `BC SUB CATEGORY` != "MIXED ONIONS", ppp < 60, pppl < 50)
  

par(mfrow = c(1,2))

#ppp price over time - split by organic
a <- dat %>% 
  ggplot() +
  geom_jitter(aes(x = date, y = ppp, color = `BC SUB CATEGORY`), height = 0, alpha = .5) +
  geom_smooth(aes(x = date, y = ppp, color = `BC SUB CATEGORY`), se = FALSE, size = 2) +
  coord_cartesian(ylim = c(0,10)) +
  labs(y = "Price Per Pound", x = "Week", title = "Current Year") +
  the_dark_knight +
  scale_color_manual(values = onion_col) +
  facet_wrap(~`HW ORGANIC`, nrow = 1)

#pppl price over time
b <- dat %>% 
  ggplot() +
  geom_jitter(aes(x = date, y = pppl, color = `BC SUB CATEGORY`), height = 0, alpha = .5) +
  geom_smooth(aes(x = date, y = pppl, color = `BC SUB CATEGORY`), se = FALSE, size = 2) +
  coord_cartesian(ylim = c(0,10)) +
  labs(y = "Price Per Pound", x = "Week", title = "Last Year") +
  the_dark_knight +
  scale_color_manual(values = onion_col)
a / b

#ppp boxplots
d <- dat %>% 
  ggplot(aes(x = `BC SUB CATEGORY`, y = ppp, fill = `BC SUB CATEGORY`)) +
  geom_boxplot(color = "#45B08C") +
  the_dark_knight +
  theme(legend.position = "none") +
  scale_fill_manual(values = onion_col)

#ppl boxplot (NOT DIFFERENT)
e <- dat %>% 
  ggplot(aes(x = `BC SUB CATEGORY`, y = pppl, color = `BC SUB CATEGORY`, fill = `BC SUB CATEGORY`)) +
  geom_boxplot() +
  the_dark_knight +
  theme(legend.position = "none") +
  scale_fill_manual(values = onion_col)

# Difference in prices between years
dat %>% 
  ggplot(aes(x = date, y = pppl - ppp, color = `BC SUB CATEGORY`)) +
  geom_jitter(height = 0, alpha = .5) +
  geom_smooth() +
  the_dark_knight +
  scale_fill_manual()

#Random Weight, Organic, Segment

dat %>% 
  ggplot() +
  geom_boxplot(aes(y = ppp, x = `HW ORGANIC`))


# Agregate total cost by onion type, barplot

g <- dat %>% 
  group_by(`BC SUB CATEGORY`) %>% 
  summarise(ppp = sum(ppp), pppl = sum(pppl)) %>% 
  ggplot() +
  geom_col(aes(y = ppp, x = `BC SUB CATEGORY`, fill = `BC SUB CATEGORY`)) +
  the_dark_knight +
  scale_fill_manual(values = onion_col) +
  theme(axis.text.x = element_text(angle = 20), legend.position = "none") +
  labs(title = "2019 Sales") #+
  #geom_col(aes(y = pppl, x = `BC SUB CATEGORY`))

h <- dat %>% 
  group_by(`BC SUB CATEGORY`) %>% 
  summarise(ppp = sum(ppp), pppl = sum(pppl)) %>% 
  ggplot() +
  geom_col(aes(y = pppl, x = `BC SUB CATEGORY`, fill = `BC SUB CATEGORY`)) +
  the_dark_knight +
  scale_fill_manual(values = onion_col) +
  theme(axis.text.x = element_text(angle = 20), legend.position = "none") +
  labs(title = "2018 Sales")

#g / h

i <- dat %>% 
  group_by(`BC SUB CATEGORY`) %>% 
  summarise(ppp = sum(ppp), pppl = sum(pppl)) %>% 
  mutate(pppp = ppp - pppl) %>% 
  ggplot() +
  geom_col(aes(y = pppp, x = `BC SUB CATEGORY`, fill = `BC SUB CATEGORY`)) +
  the_dark_knight +
  scale_fill_manual(values = onion_col) +
  labs(title = "Increase in Sales from 2018 to 2019")
  
(h + g) /i
 