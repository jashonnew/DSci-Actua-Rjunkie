pacman::p_load(tidyverse, lubridate, patchwork, forcats)

dat1 <- read_csv("../.Rdata/US Onions.csv") %>% 
  mutate(ppp = `GA $` /`GA EQ`, pppl = `GA $ Prior Yr` / `GA EQ Prior Yr`, date = ymd(Week))


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
                         legend.text = element_text(color = "white"),
                         legend.key = element_rect(fill = "#404040"))
onion_col <- c("green", "#F9ECC5", "#F560C3", "skyblue", "orange", "#F7F6F3", "#F7EF43")
onion_col_add <- c("green", "darkblue", "#F9ECC5", "#F560C3", "darkblue", "skyblue", "orange", "darkblue", "#F7F6F3", "#F7EF43")


dat <-  dat1 %>% 
  filter(`BC SUB CATEGORY` != "VALUE ADD", `BC SUB CATEGORY` != "REMAINING ONIONS", `BC SUB CATEGORY` != "MIXED ONIONS", ppp < 60, pppl < 50)
  

par(mfrow = c(1,2))

#ppp price over time - split by organic
a <- dat %>% 
  ggplot() +
  geom_jitter(aes(x = date, y = ppp, color = `BC SUB CATEGORY`), height = 0, alpha = .6) +
  geom_smooth(aes(x = date, y = ppp, color = `BC SUB CATEGORY`), se = FALSE, size = 2) +
  coord_cartesian(ylim = c(0,10)) +
  labs(y = "Price Per Pound $", x = "Month", title = "2019 Price per Pound", color = "Onion Category") +
  the_dark_knight +
  scale_color_manual(values = onion_col) 

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
  mutate(Organic = case_when(
    `HW ORGANIC` == "NOT APPLICABLE" ~ "Not Organic",
    TRUE ~ "Organic"
  )) %>% 
  ggplot(aes(x = fct_reorder(`BC SUB CATEGORY`, ppp, .fun = median, .desc = TRUE), y = ppp, fill = `BC SUB CATEGORY`)) +
  geom_jitter(color = "firebrick", alpha = .8, height = 0) +
  geom_boxplot(color = "#276A53", outlier.size = -5) +
  the_dark_knight +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0,20)) +
  scale_fill_manual(values = onion_col) +
  labs(x = "Onion Type", y = "Price per Pound $", title = "Shallots are Expensive, Yellow are Cheap", fill = "Onion Category") +
  facet_wrap(~Organic)

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

onion_col_add1 <- c("#F7EF43","orange", "green", "#F9ECC5", "#F560C3", "darkblue", "skyblue",  "darkblue", "#F7F6F3","darkblue")

g <- dat1 %>% 
  filter(!is.na(`GA $ Prior Yr`), !is.na(`GA $`)) %>% 
  group_by(`BC SUB CATEGORY`) %>% 
  summarise(totsales = sum(`GA $`), totsalesL = sum(`GA $ Prior Yr`)) %>% 
  mutate(`BC SUB CATEGORY` = parse_factor(`BC SUB CATEGORY`), `BC SUB CATEGORY` = fct_reorder(`BC SUB CATEGORY`, desc(totsalesL))) %>% 
  ggplot() +
  geom_col(aes(y = totsalesL/1000000, x = `BC SUB CATEGORY`, fill = `BC SUB CATEGORY`)) +
  geom_text(aes(y = totsalesL/1000000, x = `BC SUB CATEGORY`, label = round(totsalesL/1000000, digits = 1) ), hjust = -.1, color = "white") +
  the_dark_knight +
  scale_fill_manual(values = onion_col_add1) +
  #coord_cartesian(ylim = c(0, 335)) +
  theme( legend.position = "none") +
  labs(title = "2018 Total Sales", y = "Dollars (in millions)", x = "Onion Type", subtitle = "1.105 Billion Dollars total") +
  coord_flip(ylim = c(0,350))#+
  #geom_col(aes(y = pppl, x = `BC SUB CATEGORY`))

onion_col_add2 <- c("#F7EF43","orange", "green", "#F9ECC5", "#F560C3", "darkblue", "skyblue",  "darkblue", "#F7F6F3", "darkblue")

h <- dat1 %>% 
  filter(!is.na(`GA $ Prior Yr`), !is.na(`GA $`)) %>% 
  group_by(`BC SUB CATEGORY`) %>% 
  summarise(totsales = sum(`GA $`), totsalesL = sum(`GA $ Prior Yr`)) %>% 
  mutate(`BC SUB CATEGORY` = parse_factor(`BC SUB CATEGORY`), `BC SUB CATEGORY` = fct_reorder(`BC SUB CATEGORY`, desc(totsales))) %>% 
  mutate(Saleschg = totsales - totsalesL) %>% 
  ggplot() +
  geom_col(aes(y = totsales/1000000, x = `BC SUB CATEGORY`, fill = `BC SUB CATEGORY`)) +
  geom_text(aes(y = totsales/1000000, x = `BC SUB CATEGORY`, label = round(totsales/1000000, digits = 1) ), hjust = -.1, color = "white") +
  the_dark_knight +
  scale_fill_manual(values = onion_col_add2) +
  theme( legend.position = "none") +
  labs(title = "2019 Total Sales", y = "Dollars (in millions)", x = "Onion Type", subtitle = "1.189 Billion Dollars Total") +
  coord_flip(ylim = c(0,350)) 

#g / h

onion_col_add3 <- c("green", "orange", "#F7EF43", "#F9ECC5", "#F560C3", "skyblue", "darkblue", "darkblue", "#F7F6F3", "darkblue")

i <- dat1 %>% 
  filter(!is.na(`GA $ Prior Yr`), !is.na(`GA $`)) %>% 
  group_by(`BC SUB CATEGORY`) %>% 
  summarise(totsales = sum(`GA $`), totsalesL = sum(`GA $ Prior Yr`)) %>% 
  mutate(Saleschg = totsales - totsalesL) %>% 
  mutate(`BC SUB CATEGORY` = parse_factor(`BC SUB CATEGORY`), `BC SUB CATEGORY` = fct_reorder(`BC SUB CATEGORY`,desc(Saleschg) )) %>% 
  ggplot() +
  geom_col(aes(y = Saleschg/1000000, x = `BC SUB CATEGORY`, fill = `BC SUB CATEGORY`)) +
  geom_text(aes(y = Saleschg/1000000, x = `BC SUB CATEGORY`, label = round(Saleschg/1000000, digits = 1) ), hjust = -.1, color = "white") +
  the_dark_knight +
  scale_fill_manual(values = onion_col_add3) +
  labs(title = "Increase in Total Sales from 2018 to 2019", y = "Dollars (in millions)", x = "Onion Type", subtitle = "83.1 Million Dollar Increase Total", color = "Onion Category") +
  coord_flip()
  
(g / h) | i
 

dat2 <- dat1 %>% 
  filter(!is.na(`GA $ Prior Yr`), !is.na(`GA $`)) %>% 
  group_by(`BC SUB CATEGORY`) %>% 
  summarise(totsales = sum(`GA $`), totsalesL = sum(`GA $ Prior Yr`)) %>% 
  mutate(`BC SUB CATEGORY` = parse_factor(`BC SUB CATEGORY`), `BC SUB CATEGORY` = fct_reorder(`BC SUB CATEGORY`, totsales)) %>% 
  mutate(Saleschg = totsales - totsalesL) 
  