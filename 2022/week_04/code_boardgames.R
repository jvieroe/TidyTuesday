library(tidytuesdayR)
library(tidyverse)

# tt <- tidytuesdayR::tt_load('2022-01-25')
# 
# details <- tt[[1]]
# ratings <- tt[[2]]
# 
# rm(tt)
# 
# saveRDS(details, "2022/week_04/data/details.rds")
# saveRDS(ratings, "2022/week_04/data/ratings.rds")


details <- readRDS("2022/week_04/data/details.rds")
ratings <- readRDS("2022/week_04/data/ratings.rds")

details <- details %>% 
  mutate(test = 1)

df <- ratings %>% 
  tidylog::left_join(., details,
                     by = "id")

df %>% 
  select(c(name, primary))


df <- df %>% 
  arrange(yearpublished, year)


df <- df %>% 
  filter(yearpublished >= 1922) %>% 
  filter(yearpublished <= 2021)


df %>% 
  group_by(yearpublished) %>% 
  summarize(count = n()) %>% 
  ggplot(., aes(yearpublished, count)) +
  geom_line()


df <- df %>% 
  mutate(minpl = factor(minplayers))

ggplot(df, aes(yearpublished, count)) +
  geom_line() +
  facet_wrap(.~ minplayers)

