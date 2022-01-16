library(tidyverse)
library(tidytuesdayR)
library(lubridate)
library(sf)
library(janitor)

# https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md


# tt <- tidytuesdayR::tt_load('2022-01-11')

colony <- tt[[1]]

stressor <- tt[[2]]

rm(tt)

stressor <- stressor %>% 
  pivot_wider(id_cols = c(year, months, state),
              names_from = stressor,
              values_from = stress_pct)

df <- colony %>% 
  tidylog::left_join(.,
                     stressor,
                     by = c("state", "year", "months")) %>% 
  clean_names()




df <- colony %>% 
  #filter((months == "January-March" & year == 2015) | (months == "April-June" & year == 2021)) %>% 
  filter(months == "January-March") %>% 
  filter(year %in% c(2015, 2021)) %>% 
  arrange(state, year) %>% 
  group_by(state) %>% 
  mutate(tmp = lag(colony_n)) %>% 
  ungroup() %>% 
  mutate(delta = (colony_n - tmp)/1000)

hist(df$delta)

length(unique(colony$state))

















hexa <- read_sf(dsn = "2022/week_02/data/us_states_hexgrid",
                layer = "us_states_hexgrid")

hexa <- hexa %>% 
  st_transform(crs = 3857)


ggplot() +
  geom_sf(data = hexa)

hexa <- hexa %>% 
  mutate(state = gsub(" \\(United States\\)", "", google_nam)) %>% 
  relocate(state, .before = geometry)


hexa <- hexa %>% 
  mutate(cent = st_centroid(.))

cents <- hexa %>% 
  st_centroid()

ggplot() +
  geom_sf(data = hexa) +
  geom_sf(data = cents)




colony <- colony %>% 
  separate(months, into = c("month_first", "month_second"),
           remove = FALSE,
           sep = "-")



colony <- colony %>% 
  filter(year %in% c(2015, 2021)) %>% 
  filter((year == 2015 & months == "January-March") | (year == 2021 & months == "April-June"))




