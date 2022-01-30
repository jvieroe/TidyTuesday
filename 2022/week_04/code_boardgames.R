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


df <- ratings %>% 
  tidylog::left_join(., details,
                     by = "id")

df %>% 
  select(c(name, primary))


df <- df %>% 
  arrange(yearpublished, year)


df <- df %>% 
  filter(yearpublished >= 1921) %>% 
  filter(yearpublished <= 2021)

df %>% 
  ggplot(.) +
  geom_point(aes(x = average, y = owned),
             alpha = 0.3)

df %>% 
  group_by(yearpublished) %>% 
  summarize(count = n()) %>% 
  ggplot(., aes(yearpublished, count)) +
  geom_line()

df %>% 
  filter(grepl("Dice rolling", boardgamemechanic, ignore.case = T)) %>% 
  group_by(yearpublished) %>% 
  summarize(count = n()) %>% 
  ggplot(., aes(yearpublished, count)) +
  geom_line()

plot_df <- df %>% 
  mutate(dice = grepl("Dice rolling", boardgamemechanic, ignore.case = T),
         memory = grepl("Memory", boardgamemechanic, ignore.case = T)) %>% 
  group_by(yearpublished) %>% 
  summarize(count = n(),
            dice = sum(dice),
            memory = sum(memory)) %>% 
  mutate(pct_dice = dice/count,
         pct_memory = memory/count)

ggplot(plot_df) +
  geom_line(aes(yearpublished, pct_dice))



df %>% 
  group_by(yearpublished) %>% 
  summarize(avg = mean(average)) %>% 
  ggplot(., aes(yearpublished, avg)) +
  geom_line() 

df %>% 
  group_by(yearpublished) %>% 
  summarize(avg = median(playingtime)) %>% 
  ggplot(., aes(yearpublished, avg)) +
  geom_line() 

df %>% 
  group_by(yearpublished) %>% 
  summarize(avg = median(maxplaytime)) %>% 
  ggplot(., aes(yearpublished, avg)) +
  geom_line() 




civi <- df %>% 
  mutate(civi = grepl("Civilization", boardgamefamily, ignore.case = TRUE)) %>% 
  filter(civi)


top5 <- df %>% 
  arrange(desc(owned)) %>% 
  head(5)

ggplot(df) +
  geom_point(aes(x = minplayers, y = minplaytime))

df <- df %>% 
  arrange(desc(bayes_average))
