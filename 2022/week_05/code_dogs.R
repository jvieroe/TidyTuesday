library(tidyverse)
library(janitor)
library(cowplot)
library(ggtext)
library(wesanderson)
library(MetBrewer)

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

breed_traits <- breed_traits %>% 
  clean_names() %>% 
  mutate(breed = str_squish(breed)) %>% 
  mutate(breed = str_replace(breed, pattern = "’", " ")) #%>% .[, c(1:5, 7, 10:16)]

breed_rank_all <- breed_rank_all %>% 
  clean_names() %>% 
  mutate(breed = str_squish(breed))

df <- breed_traits %>% 
  tidylog::left_join(.,
                     breed_rank_all,
                     by = "breed")

df <- df %>% 
  mutate(coat_length = str_squish(coat_length))

rm(list = setdiff(ls(), "df"))

df <- df %>% 
  filter(!coat_length %in% c("Plott Hounds"))

df %>% 
  select(ends_with("_rank")) %>% 
  names(.)

df <- df %>% 
  rowwise() %>% 
  mutate(rank_mean = mean(c_across(ends_with("_rank")),
                          na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-ends_with("_rank"))


df <- df %>% 
  group_by(coat_length) %>% 
  arrange(desc(rank_mean)) %>% 
  slice_tail(n = 1) %>% 
  ungroup()

names(df)

df <- df %>% 
  select(breed,
         coat_length,
         affectionate_with_family,
         good_with_young_children,
         good_with_other_dogs,
         shedding_level,
         coat_grooming_frequency,
         drooling_level,
         openness_to_strangers,
         playfulness_level,
         watchdog_protective_nature,
         adaptability_level,
         trainability_level,
         energy_level,
         barking_level,
         mental_stimulation_needs)


df <- df %>% 
  pivot_longer(-c(breed,
                  coat_length))



df <- df %>% 
  group_by(breed) %>% 
  mutate(seq_ = row_number()) %>% 
  ungroup()

tabyl(df$name)

test <- df %>% 
  filter(coat_length == "Long")

ggplot(test) +
  geom_col(aes(x = seq_, y = value),
           fill = "blue", alpha = .1) +
  coord_polar() +
  theme_void() +
  geom_segment(data = tibble(y = c(1, 3, 5)),
               aes(x = 0, xend = 14.5,
                   y = y, yend = y),
               linetype = "97",
               size = .25) +
  scale_y_continuous(limits = c(-0.25, 5),
                     breaks = seq(1, 5, 1),
                     labels = seq(1, 5, 1))

ggplot(df) +
  geom_col(aes(x = seq_, y = value)) +
  coord_polar() +
  theme_void() +
  theme(panel.grid.major.y = element_line(size = .15)) +
  facet_wrap(~ breed, ncol = 2)
