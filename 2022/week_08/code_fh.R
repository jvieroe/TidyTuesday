library(tidyverse)
library(janitor)

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

df <- df %>% 
  clean_names()

df

ggplot(df, aes(x = pr, y = cl, fill = region_name)) +
  geom_point(shape = 21,
             position = position_jitter(width = 0.5,
                                        height = 0.5),
             alpha = .5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ region_name) +
  theme(legend.position = "none")
