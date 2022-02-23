library(tidyverse)
library(janitor)
library(viridis)
library(ggtext)

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

df <- df %>% 
  clean_names()

df

df %>% 
  filter(is.na(cl) | is.na(pr)) %>% 
  tally()

exp_val <- 0.2

ggplot(df, aes(x = pr, y = cl, color = region_name, fill = region_name)) +
  geom_point(shape = 21,
             position = position_jitter(width = 0.5,
                                        height = 0.5),
             alpha = .25) +
  geom_smooth(method = "lm") +
  theme(legend.position = "none") +
  facet_wrap(~ region_name)
