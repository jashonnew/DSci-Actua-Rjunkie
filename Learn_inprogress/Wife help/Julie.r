pacman::p_load(tidyverse, patchwork)

dat_p <- read_csv("Pre-Instruction Test Performance.csv") %>% 
  select(-Info) %>% 
  mutate(id = "pre") %>% 
  mutate(total = (`1` + `2`+ `3`+ `4`+ `5`+ `6`+ `7`+ `8`+ `9`+ `10`+ `11`+ `12`+ `13`+ `14`+ `15`+ `16`+ `17`+ `18`+ `19`+ `20`))


dat_a <- read_csv("Post-Instruction Test Performance.csv") %>% 
  select(-Info) %>% 
  mutate(total = (`1` + `2`+ `3`+ `4`+ `5`+ `6`+ `7`+ `8`+ `9`+ `10`+ `11`+ `12`+ `13`+ `14`+ `15`+ `16`+ `17`+ `18`+ `19`+ `20`))

dat <- dat_a %>% 
  mutate(id = "post") %>% 
  select(Name, total) %>% 
  bind_rows(select(dat_p,total,Name,id)) %>% 
  mutate(id = replace_na(.$id, replace = "post"))


#PLOTS

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
                         legend.text = element_text(color = "white"),
                         legend.key = element_rect(fill = "#404040"))
onion_col <- c("green", "#F9ECC5", "#F560C3", "skyblue", "orange", "#F7F6F3", "#F7EF43")
onion_col_add <- c("green", "darkblue", "#F9ECC5", "#F560C3", "darkblue", "skyblue", "orange", "darkblue", "#F7F6F3", "#F7EF43")

a <- dat_p %>% 
  ggplot(aes(x = reorder(Name,-total), y = total)) +
  geom_col(fill = "#00BFC4") +
  the_dark_knight +
  labs(title = "Pre-Assesment", y = "Score", x = "Name") + 
  theme(legend.position = "none") +
  coord_flip()

b <- dat_a %>% 
  ggplot(aes(x = reorder(Name,-total), y = total)) +
  geom_col(fill = "#F8766D") +
  the_dark_knight +
  labs(title = "Post-Assesment", y = "Score", x = "Name") +
  theme(legend.position = "none") +
  coord_flip()

d <- dat %>% 
  ggplot() +
  geom_col(aes(x = reorder(Name,-total), y = total, fill = id), position ="dodge2") +
  the_dark_knight +
  labs(title = "Change in Score", y = "Score", x = "Name", fill = "ID") +
  coord_flip()

dat %>% 
  ggplot() +
  geom_col(aes(x = Name, y = total))
