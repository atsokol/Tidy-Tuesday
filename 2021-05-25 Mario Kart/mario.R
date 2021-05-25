library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(tidyr)
library(RColorBrewer)
library(ggtext)
library(ggrepel)

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv',
                           col_types = "fffffDcdd")
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

# Total days record held on all tracks combined
rankings <- records %>% 
  mutate(year = year(date)) %>% 
  group_by(player, year) %>% 
  summarise(tot_time = sum(record_duration), .groups = "drop") %>% 
  complete(player, year, fill=list(tot_time=0)) %>% 
  group_by(player) %>% 
  mutate(cum_tot = cumsum(tot_time))

top <- rankings %>% 
  filter(year==2020) %>%  
  arrange(desc(cum_tot))
  
g1 <- rankings %>% 
  ggplot(aes(x=year, y=cum_tot, group=player)) +
  geom_step(color = "grey70") +
  geom_step(data=. %>% filter(player %in% top$player[1:4]),
            aes(color = player)) +
  geom_text_repel(data=. %>% filter(player %in% top$player[1:5], year==max(year)),
            aes(label = player, colour = player), hjust = -0.1, size = 3,
            direction="y", segment.color = 'transparent') +
  labs(title="Careers of Mario Kart players",
       subtitle="Total days record held across all tracks",
       caption = "TidyTuesday challenge, Author: Andrii Tsokol",
       x=NULL, y="Days") +
  theme_tinyhand(grid = "Y") +
  theme(legend.position = 'none',
        plot.caption = element_text(),
        plot.caption.position = "plot") +
  scale_y_comma() +
  scale_color_brewer(palette = "Set2") +
  coord_cartesian(clip="off")

ggsave("./2021-05-25 Mario Kart/plots/top_record_holders.png", g1, dpi=300)
