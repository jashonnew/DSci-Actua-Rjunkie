pacman::p_load(tidyverse, png, grid)

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv") %>% 
  pivot_longer(c(-EPISODE,-TITLE),"Art_element") %>% 
  filter(value == 1) %>% 
  group_by(Art_element) %>% 
  summarize(tot = n()) %>% 
  filter(tot > 1)

bob_ross_theme <- theme(panel.background = element_rect(fill = "#967f4e"),    # This is my theme I will use 
                    plot.background = element_rect(fill = "#967f4e"),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    axis.text = element_text(color = "#940644", size = 9),
                    axis.title = element_text(color = "#940644", vjust = 2, size = 16),
                    title = element_text(color = "#940644"),
                    legend.background = element_rect(fill = "#940644"),
                    legend.text = element_text(color = "#940644"),
                    legend.key = element_rect(fill = "#967f4e"))

get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

l <- get_png("bobby.png")

bob <- readPNG(system.file("bobby.png"))


bob_ross %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(Art_element, -tot), y = tot), fill ="#940644" ) +
  coord_flip() +
  bob_ross_theme +
  labs(y = "Total Occurences in Shows", x = "Frame Type or Landscape Element", title = "Bob's Happy Little Trees Were His Most Frequented Element", subtitle = "Most Who've Had the Opportunity to Watch a Show of His Are Grateful.")  
#+
  #annotation_custom(l, xmin = 20, xmax = 30,ymin = 200,ymax = 200) +
  #geom_point(aes(y = 200, x = 20))

#940644
#967f4e