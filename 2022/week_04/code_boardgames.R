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
  tidylog::left_join(., ratings,
                     by = "id")

df <- df %>% 
  filter(is.na(test))
