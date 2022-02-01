library(tidytuesdayR)
library(tidyverse)
library(cowplot)

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



df <- df %>% 
  rownames_to_column()


df <- df %>% 
  mutate(across(c(boardgameartist, boardgamedesigner),
                ~ str_replace(.x, "\\[|\\]", ""))) %>% 
  mutate(across(c(boardgameartist, boardgamedesigner),
                ~ str_remove_all(.x, "\'"))) %>% 
  mutate(across(c(boardgameartist, boardgamedesigner),
                ~ str_replace(.x, "\\[|\\]", "")))

df_long <- df %>% 
  separate_rows(boardgameartist, sep = ",") %>% 
  arrange(yearpublished, boardgamedesigner)

df_long <- df_long %>% 
  filter(boardgamedesigner != "(Uncredited)") %>% 
  filter(!is.na(boardgamedesigner))


df_designer <- df_long %>% 
  group_by(boardgamedesigner) %>% 
  summarise(sum_owned = sum(owned, na.rm = TRUE),
            mean_rating = mean(average, na.rm = TRUE),
            mean_bayes = mean(bayes_average, na.rm = TRUE),
            games_produced = n(),
            mean_year = mean(yearpublished, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(desc(games_produced),
          desc(sum_owned),
          desc(mean_rating))


plot_df <- df_designer %>% 
  slice_head(n = 100)



ggplot(plot_df, aes(x = games_produced, y = sum_owned)) +
  geom_point(aes(size = mean_bayes), alpha = .5)

p1 <- last_plot()

ggplot(plot_df, aes(x = log(games_produced), y = log(sum_owned))) +
  geom_point(aes(size = mean_bayes,
                 color = mean_year), 
             alpha = .5)

p2 <- last_plot()



cat_df <- df_long %>% 
  select(boardgamedesigner, primary, boardgamecategory) %>% 
  separate_rows(boardgamecategory, sep = ",") %>% 
  arrange(boardgamedesigner, primary, boardgamecategory)

# df <- df %>% 
#   filter(yearpublished >= 1921) %>% 
#   filter(yearpublished <= 2021)
# 
# df %>% 
#   ggplot(.) +
#   geom_point(aes(x = average, y = owned),
#              alpha = 0.3)
# 
# df %>% 
#   group_by(yearpublished) %>% 
#   summarize(count = n()) %>% 
#   ggplot(., aes(yearpublished, count)) +
#   geom_line()
# 
# df %>% 
#   filter(grepl("Dice rolling", boardgamemechanic, ignore.case = T)) %>% 
#   group_by(yearpublished) %>% 
#   summarize(count = n()) %>% 
#   ggplot(., aes(yearpublished, count)) +
#   geom_line()
# 
# plot_df <- df %>% 
#   mutate(dice = grepl("Dice rolling", boardgamemechanic, ignore.case = T),
#          memory = grepl("Memory", boardgamemechanic, ignore.case = T)) %>% 
#   group_by(yearpublished) %>% 
#   summarize(count = n(),
#             dice = sum(dice),
#             memory = sum(memory)) %>% 
#   mutate(pct_dice = dice/count,
#          pct_memory = memory/count)
# 
# ggplot(plot_df) +
#   geom_line(aes(yearpublished, pct_dice))
# 
# 
# 
# df %>% 
#   group_by(yearpublished) %>% 
#   summarize(avg = mean(average)) %>% 
#   ggplot(., aes(yearpublished, avg)) +
#   geom_line() 
# 
# df %>% 
#   group_by(yearpublished) %>% 
#   summarize(avg = median(playingtime)) %>% 
#   ggplot(., aes(yearpublished, avg)) +
#   geom_line() 
# 
# df %>% 
#   group_by(yearpublished) %>% 
#   summarize(avg = median(maxplaytime)) %>% 
#   ggplot(., aes(yearpublished, avg)) +
#   geom_line() 
# 
# 
# 
# 
# civi <- df %>% 
#   mutate(civi = grepl("Civilization", boardgamefamily, ignore.case = TRUE)) %>% 
#   filter(civi)
# 
# 
# top5 <- df %>% 
#   arrange(desc(owned)) %>% 
#   head(5)
# 
# ggplot(df) +
#   geom_point(aes(x = minplayers, y = minplaytime))
# 
# df <- df %>% 
#   arrange(desc(bayes_average))
